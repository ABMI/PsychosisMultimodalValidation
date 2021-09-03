#' @export
noteNLP <- function(connectionDetails,
                    cdmDatabaseSchema,
                    cohortDatabaseSchema,
                    cohortTable,
                    cohortId,
                    noteTitle,
                    population,
                    ldaModeldir,
                    outputFolder,
                    databaseName,
                    analysisId
                    ){

  #note extraction
  ParallelLogger::logInfo("Starting note extraction")
  sql <- 'select person_id, note_date, note_title, note_text from(
select person_id, note_date, note_title, note_text, abs(datediff(dd, note_date, cohort_start_date)) as gap, row_number() over(partition by person_id order by abs(datediff(dd, note_date, cohort_start_date))) as rowId
  from (select * from @cdmDatabaseSchema.note where note_title like @noteTitle ) n
  join (select * from @cohortDatabaseSchema.@cohortTable where cohort_definition_id = @cohortId ) c
  on n.person_id = c.subject_id
  where n.note_date <= dateadd(dd, 30, c.cohort_start_date) and n.note_date >= dateadd(dd, -30, c.cohort_start_date)) d
where d.rowId = 1
 ;'

  sql <- SqlRender::render(sql,
                           cdmDatabaseSchema = cdmDatabaseSchema,
                           cohortDatabaseSchema = cohortDatabaseSchema,
                           cohortTable = cohortTable,
                           cohortId = cohortId,
                           noteTitle = noteTitle)
  note <- DatabaseConnector::querySql(DatabaseConnector::connect(connectionDetails), sql)

  saveName <- file.path(outputFolder, databaseName, analysisId)
  if(!dir.exists(saveName)){
    dir.create(saveName, recursive = T)
  }

  if(!file.exists(paste0 (saveName, '/noteProcesseddtm.rds'))){

    #processing
    ParallelLogger::logInfo("Starting note processing")

    note_text_cleaned <- clean_txt(note$NOTE_TEXT)
    note_text_selected <- sapply(note_text_cleaned, nlp)

    stopword <- readRDS(paste0(strsplit(ldaModeldir, 'LDA')[[1]][1], '/stopword.rds'))

    process <- tm::VCorpus(tm::VectorSource(note_text_selected))
    process <- tm::tm_map(process, tm::removePunctuation)
    process <- tm::tm_map(process, tm::removeNumbers)
    process <- tm::tm_map(process, tolower)
    process <- tm::tm_map(process, tm::removeWords, stopword)
    process <- tm::tm_map(process, tm::stripWhitespace)
    processed <- tm::tm_map(process, tm::PlainTextDocument)

    dtm <- tm::DocumentTermMatrix(processed, control = list(wordLength=c(2,Inf), tokenizer = tokenizer))
    saveRDS(dtm, paste0 (saveName, '/noteProcesseddtm.rds'))

  }else{
    dtm <- readRDS(paste0 (saveName, '/noteProcesseddtm.rds'))
  }


  #LDA

  #Model load
  ldaModel <- readRDS(ldaModeldir)

  #predict posterior probability
  ParallelLogger::logInfo("Predicting posterior probabilities")
  ldaCovariates <- topicmodels::posterior(object = ldaModel,
                                         newdata = dtm)$topics %>% as.matrix()

  #add probability to newData$plpData$covariates
  ldaCovariates <- as.data.frame(ldaCovariates)
  ldaCovariates <- cbind(note$PERSON_ID, ldaCovariates)

  ldaCovariates <- merge(ldaCovariates, population %>% dplyr::select(subjectId, rowId) %>% unique(), by.x = 'note$PERSON_ID', by.y = 'subjectId', all.x = T)


  ldaCovariates <- ldaCovariates %>% dplyr::select(rowId, `1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`)

  ldaCovariates <- reshape2::melt(ldaCovariates, id.vars = c("rowId"))
  ldaCovariates$variable <- as.integer(paste0(ldaCovariates$variable,456))

  colnames(ldaCovariates) <- c("rowId", "covariateId", "covariateValue")

  return(ldaCovariates)

}

clean_txt = function(x){
  x=tolower(x)
  x=tm::removePunctuation(x)
  x=tm::removeNumbers(x)
  x=tm::stripWhitespace(x)
  return(x)
}


nlp<- function(x){
  x <- paste(KoNLP::SimplePos09(x))
  x <- grep(pattern = "/N|/F", x = x, value=T)
  x <- stringr::str_remove(x, "/.*$")
  x <- paste(x, collapse=" ")
}


tokenizer <- function(x){unlist(lapply(NLP::ngrams(NLP::words(x), 1:3), paste, collapse = " "), use.names = FALSE)}
