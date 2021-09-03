#' @export
externalValidatePlp <- function(plpResult,
         connectionDetails,
         validationSchemaTarget,
         validationSchemaOutcome,
         validationSchemaCdm, databaseNames,
         validationTableTarget='cohort', validationTableOutcome='cohort',
         validationIdTarget = NULL, validationIdOutcome = NULL,
         oracleTempSchema=NULL,#validationSchemaCdm,
         verbosity="INFO", keepPrediction=F,
         recalibrate = NULL,
         sampleSize = NULL,
         outputFolder,
         ldaModeldir = NULL,
         noteTitle = NULL){

  # TODO:: ADD LOGGING, MORE INOUT TESTS, ADD TEST CASE IN PACKAGE...
  if(missing(plpResult))
    stop('Need to input a plpResult')
  if(class(plpResult)!="runPlp")
    stop('Need to input a plpResult of class runPlp')

  if(!missing(outputFolder)){
    if(missing(databaseNames)){
      stop('Need to enter databaseNames if saving results to outputFolder')
    }}

  if(missing(connectionDetails))
    stop('Need to enter connection details')

  if(missing(validationSchemaTarget))
    stop('Need to enter validationSchemaTarget ')
  if(missing(validationSchemaOutcome))
    stop('Need to enter validationSchemaOutcome ')
  if(missing(validationSchemaCdm))
    stop('Need to enter validationSchemaCdm ')

  # convert the lists to vectors (to keep backwards compat)
  if(class(validationSchemaTarget)=='list'){
    validationSchemaTarget <- unlist(validationSchemaTarget)
  }
  if(class(validationSchemaOutcome)=='list'){
    validationSchemaOutcome <- unlist(validationSchemaOutcome)
  }
  if(class(validationSchemaCdm )=='list'){
    validationSchemaCdm  <- unlist(validationSchemaCdm)
  }
  if(class(databaseNames)=='list'){
    databaseNames  <- unlist(databaseNames)
  }
  if(class(validationTableTarget)=='list'){
    validationTableTarget  <- unlist(validationTableTarget)
  }
  if(class(validationTableOutcome)=='list'){
    validationTableOutcome  <- unlist(validationTableOutcome)
  }
  if(class(validationIdTarget)=='list'){
    validationIdTarget  <- unlist(validationIdTarget)
  }
  if(class(validationIdOutcome)=='list'){
    validationIdOutcome  <- unlist(validationIdOutcome)
  }


  # check lengths
  if(length(validationSchemaTarget) != length(validationSchemaOutcome)){
    stop('validationSchemaTarget and validationSchemaOutcome need to be the same length')
  }
  if(length(validationSchemaTarget) != length(validationSchemaCdm)){
    stop('validationSchemaTarget and validationSchemaCdm need to be the same length')
  }

  # check class
  if(class(validationSchemaTarget)!=class(validationSchemaOutcome)){
    stop('validationSchemaTarget and validationSchemaOutcome not same class')
  }
  if(class(validationSchemaTarget)!=class(validationSchemaCdm)){
    stop('validationSchemaTarget and validationSchemaCdm not same class')
  }

  if(!missing(databaseNames)){
    if(length(validationSchemaTarget)!=length(databaseNames)){
      stop('DatabaseNames not same length as validationSchemaTarget')
    }
  }


  # add lots of test for tables and ids -  TODO?

  if(is.null(validationIdTarget))
    validationIdTarget <- plpResult$inputSetting$populationSettings$cohortId# set the model ids
  if(is.null(validationIdOutcome))
    validationIdOutcome <- plpResult$inputSetting$populationSettings$outcomeId# set the model ids


  results <- list()
  length(results) <- length(validationSchemaCdm)
  for(i in 1:length(validationSchemaCdm)){
    # Now extract the data:
    targetTable <- validationTableTarget
    outcomeTable <- validationTableOutcome

    if(length(validationTableTarget)>1)
      targetTable <- validationTableTarget[i]
    if(length(validationTableOutcome)>1)
      outcomeTable <- validationTableOutcome[i]
    newData <- PatientLevelPrediction::similarPlpData(plpModel= plpResult$model, createCohorts = F,
                              newConnectionDetails = connectionDetails,
                              newCdmDatabaseSchema = validationSchemaCdm[i],
                              newCohortDatabaseSchema = validationSchemaTarget[i],
                              newCohortTable = targetTable,
                              newCohortId = validationIdTarget,
                              newOutcomeDatabaseSchema = validationSchemaOutcome[i],
                              newOutcomeTable = outcomeTable,
                              newOutcomeId = validationIdOutcome,
                              newOracleTempSchema = oracleTempSchema,
                              sample = sampleSize,
                              createPopulation = T)

    if(!is.null(ldaModeldir)){
      tryCatch({

        ldaCovariates <- noteNLP(connectionDetails,
                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                cohortDatabaseSchema = validationSchemaTarget[[i]],
                                outputFolder = outputFolder,
                                databaseName = databaseNames[i],
                                analysisId = plpResult$analysisRef$analysisId,
                                cohortTable = targetTable,
                                cohortId = validationIdTarget,
                                noteTitle = noteTitle,
                                population = newData$population,
                                ldaModeldir)

        newData$plpData$covariateData$covariates <- rbind(as.data.frame(newData$plpData$covariateData$covariates),ldaCovariates)
        covariateId <- seq(1456,10456, by = 1000)
        covariateName <- paste0('topic', seq(1,10))
        analysisId <- rep(456, times = 10)
        conceptId <- seq(1:10)
        covariateRef <- data.frame(covariateId, covariateName, analysisId, conceptId)
        newData$plpData$covariateData$covariateRef <-  rbind(as.data.frame(newData$plpData$covariateData$covariateRef), covariateRef)
      })
    }

    if(sum(newData$population$outcomeCount>0)<20){
      warning('Outcome count is less than 20... external validation may be inaccurate')
    }
    if(sum(newData$population$outcomeCount>0)<5){
      warning('Outcome count is less than 5... external validation stopped')
      results[[i]] <- 'not run due to outcome count less than 5'
    } else{

      results[[i]] <- applyModel(population=newData$population, plpData = newData$plpData,
                                 calculatePerformance = T, plpModel = plpResult$model)

      if(!is.null(recalibrate)){
        ParallelLogger::logInfo('Recalibrating')
        for(k in 1:length(recalibrate)){
          if(recalibrate[k] %in% c('RecalibrationintheLarge', 'weakRecalibration')){
            ParallelLogger::logInfo(paste0('Using method ', recalibrate[k]))
            recal <- recalibratePlp(results[[i]]$prediction, analysisId = plpResult$model$analysisId,
                                    method = recalibrate[k])

            results[[i]]$prediction <- recal$prediction
            results[[i]]$performanceEvaluation <- addRecalibration(results[[i]]$performanceEvaluation,
                                                                   recalibration = recal)
          }

        }
      }

      if(!keepPrediction){
        results[[i]]$prediction <- NULL
      }

      if(missing(databaseNames)){
        niceName <-   rep('Not Entered', length(validationSchemaCdm))
      } else{
        niceName <-   databaseNames
      }
      results[[i]]$inputSetting<- list(databaseNames = niceName[i],
                                       cohortId = validationIdTarget,
                                       outcomeId = validationIdOutcome,
                                       # added the below
                                       modelSettings = plpResult$model$modelSettings,
                                       testSplit = 'NA',
                                       testFraction = -1,
                                       nfold = -1,
                                       splitSeed = -1,
                                       populationSettings = plpResult$model$populationSettings,
                                       dataExtrractionSettings = list(covariateSettings = plpResult$model$metaData$call$covariateSettings,
                                                                      cdmDatabaseSchema = validationSchemaCdm[[i]],
                                                                      databaseNames = niceName[i], #added [i]
                                                                      cohortDatabaseSchema = validationSchemaTarget[[i]],
                                                                      cohortTable = targetTable,
                                                                      cohortId = validationIdTarget,
                                                                      outcomeDatabaseSchema = validationSchemaOutcome[[i]],
                                                                      outcomeTable = outcomeTable,
                                                                      outcomeIds = validationIdOutcome,
                                                                      oracleTempSchema = oracleTempSchema,
                                                                      sampleSize = sampleSize)
      )

      results[[i]]$executionSummary <- list(PackageVersion = list(rVersion= R.Version()$version.string,
                                                                  packageVersion = utils::packageVersion("PatientLevelPrediction")),
                                            PlatformDetails= list(platform= R.Version()$platform,
                                                                  cores= Sys.getenv('NUMBER_OF_PROCESSORS'),
                                                                  RAM=utils::memory.size()), #  test for non-windows needed
                                            # Sys.info()
                                            TotalExecutionElapsedTime = NULL,
                                            ExecutionDateTime = Sys.Date())

    }

  }

  if(!missing(databaseNames)){
    names(results) <- databaseNames
    # do summary
    summary <- do.call(rbind, lapply(1:length(results), function(i) summariseVal(results[[i]],
                                                                                 database=databaseNames[[i]])))
  } else{
    names(results) <- validationSchemaCdm # do I want to paste ids onto this?
    # do summary
    summary <- do.call(rbind, lapply(1:length(results), function(i) summariseVal(results[[i]],
                                                                                 database=validationSchemaCdm[[i]])))
  }


  summary <- reshape2::dcast(summary, Database ~ Metric, value.var="Value" )


  result <- list(summary=summary,
                 validation=results)

  class(result) <- 'validatePlp'

  # save results if not missing:
  if(!missing(outputFolder)){
    if(!dir.exists(outputFolder)){
      dir.create(outputFolder)
    }
    for(i in 1:length(databaseNames)){
      if(!dir.exists(file.path(outputFolder,databaseNames[i],result$validation[[i]]$model$modelAnalysisId))){
        dir.create(file.path(outputFolder,databaseNames[i],result$validation[[i]]$model$modelAnalysisId), recursive = T)
      }
      saveRDS(result$validation[[i]], file.path(outputFolder,databaseNames[i],result$validation[[i]]$model$modelAnalysisId,'validationResult.rds'))
    }
  }

  # Now return results
  return(result)
}

summariseVal <- function(result, database){
  row.names(result$performanceEvaluation$evaluationStatistics) <- NULL
  result <- as.data.frame(result$performanceEvaluation$evaluationStatistics)
  result$performanceEvaluation$evaluationStatistics$Metric <- gsub('.auc','',result$performanceEvaluation$evaluationStatistics$Metric)
  result$Database <- database
  return(result)
}

