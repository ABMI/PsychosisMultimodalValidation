#' @export
evaluateMultiplePlp <- function(analysesLocation,
         outputLocation,
         connectionDetails,
         validationSchemaTarget,
         validationSchemaOutcome,
         validationSchemaCdm,
         databaseNames,
         validationTableTarget,
         validationTableOutcome,
         validationIdTarget = NULL,
         validationIdOutcome = NULL,
         oracleTempSchema = NULL,
         verbosity = 'INFO',
         keepPrediction = F,
         recalibrate = NULL,
         sampleSize = NULL,
         noteTitle1 = NULL,
         noteTitle2 = NULL,
         noteTitle3 = NULL){

  PatientLevelPrediction:::clearLoggerType("Multple Evaluate PLP Log")
  if(!dir.exists(outputLocation)){dir.create(outputLocation,recursive=T)}
  logFileName = paste0(outputLocation,'/plplog.txt')
  logger <- ParallelLogger::createLogger(name = "Multple Evaluate PLP Log",
                                         threshold = verbosity,
                                         appenders = list(ParallelLogger::createFileAppender(layout = ParallelLogger::layoutParallel,
                                                                                             fileName = logFileName)))
  ParallelLogger::registerLogger(logger)

  if(missing(databaseNames)){
    stop('Need to put a shareable name/s for the database/s')
  }

  # for each model run externalValidatePlp()
  modelSettings <- dir(analysesLocation, recursive = F, full.names = T)

  # now fine all analysis folders..
  modelSettings <- modelSettings[grep('Analysis_',modelSettings)]



  for(i in 1:length(modelSettings)){

    ParallelLogger::logInfo(paste0('Evaluating model in ',modelSettings[i] ))

    if(dir.exists(file.path(modelSettings[i],'plpResult'))){
      ParallelLogger::logInfo(paste0('plpResult found in ',modelSettings[i] ))

      plpResult <- PatientLevelPrediction::loadPlpResult(file.path(modelSettings[i],'plpResult'))

      if(i != 1){
        noteTitle <- c(noteTitle1, noteTitle2, noteTitle3)
        noteTitle <- noteTitle[i-1]

        ldaModeldir <-  dir(file.path(modelSettings[i], 'plpResult'), recursive = F, full.names = T)
        ldaModeldir <- ldaModeldir[grep('LDA', ldaModeldir)]

      }else{
        noteTitle <- NULL
        ldaModeldir <- NULL
        }
        validations <- tryCatch(psytestV1Validation::externalValidatePlp(plpResult = plpResult,
                                                                         connectionDetails = connectionDetails,
                                                                         validationSchemaTarget = validationSchemaTarget,
                                                                         validationSchemaOutcome = validationSchemaOutcome,
                                                                         validationSchemaCdm = validationSchemaCdm,
                                                                         databaseNames = databaseNames,
                                                                         validationTableTarget = validationTableTarget,
                                                                         validationTableOutcome = validationTableOutcome,
                                                                         validationIdTarget = validationIdTarget,
                                                                         validationIdOutcome = validationIdOutcome,
                                                                         oracleTempSchema = oracleTempSchema,
                                                                         verbosity = verbosity,
                                                                         keepPrediction = keepPrediction,
                                                                         recalibrate = recalibrate,
                                                                         sampleSize=sampleSize,
                                                                         ldaModeldir = ldaModeldir,
                                                                         noteTitle = noteTitle,
                                                                         outputFolder = outputLocation),
                                error = function(cont){ParallelLogger::logInfo(paste0('Error: ',cont ))
                                  ;return(NULL)})

      if(!is.null(validations)){
        if(length(validations$validation)>1){
          for(j in 1:length(validations$validation)){
            saveName <- file.path(outputLocation, databaseNames[j], paste0(plpResult$analysisRef$analysisId))
            if(!dir.exists(saveName)){
              dir.create(saveName, recursive = T)
            }
            ParallelLogger::logInfo(paste0('Evaluation result save in ',file.path(saveName,'validationResult.rds') ))

            saveRDS(validations$validation[[j]], file.path(saveName,'validationResult.rds'))

          }
        } else {
          saveName <- file.path(outputLocation, databaseNames,paste0(plpResult$analysisRef$analysisId))
          if(!dir.exists(saveName)){
            dir.create(saveName, recursive = T)
          }
          ParallelLogger::logInfo(paste0('Evaluation result save in ',file.path(saveName,'validationResult.rds') ))
          saveRDS(validations$validation[[1]], file.path(saveName,'validationResult.rds'))
        }
      }
    }
  }

}
