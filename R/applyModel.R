applyModel <- function(population,
                       plpData,
                       plpModel,
                       calculatePerformance=T,
                       databaseOutput = NULL,
                       silent = F) {

  # check logger
  if(length(ParallelLogger::getLoggers())==0){
    logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                           threshold = "INFO",
                                           appenders = list(ParallelLogger::createConsoleAppender(layout = 'layoutTimestamp')))
    ParallelLogger::registerLogger(logger)
  }

  # check input:
  if (is.null(population))
    stop("NULL population")
  if (class(plpData) != "plpData")
    stop("Incorrect plpData class")
  if (class(plpModel) != "plpModel")
    stop("Incorrect plpModel class")

  # log the trained model details TODO

  # get prediction counts:
  peopleCount <- nrow(population)

  start.pred <- Sys.time()
  if (!silent){
    ParallelLogger::logInfo(paste("Starting Prediction ", Sys.time(), "for ", peopleCount, " people"))

    if('outcomeCount' %in% colnames(population)){
      ParallelLogger::logInfo(paste("Outcome count: ", sum(population$outcomeCount>0), " people"))
    }
  }

  prediction <- plpModel$predict(plpData = plpData, population = population)

  delta <- start.pred - Sys.time()
  if (!silent)
    ParallelLogger::logInfo(paste("Prediction completed at ", Sys.time(), " taking ", signif(delta, 3), attr(delta, "units")))


  if (!"outcomeCount" %in% colnames(prediction))
    return(list(prediction = prediction))

  if(!calculatePerformance || nrow(prediction) == 1)
    return(prediction)

  if (!silent)
    ParallelLogger::logInfo(paste("Starting evaulation at ", Sys.time()))

  performance <- PatientLevelPrediction::evaluatePlp(prediction, plpData)

  # reformatting the performance
  analysisId <-   '000000'
  if(!is.null(plpModel$analysisId)){
    analysisId <-   plpModel$analysisId
  }

  nr1 <- length(unlist(performance$evaluationStatistics[-1]))
  performance$evaluationStatistics <- cbind(analysisId= rep(analysisId,nr1),
                                            Eval=rep('validation', nr1),
                                            Metric = names(unlist(performance$evaluationStatistics[-1])),
                                            Value = unlist(performance$evaluationStatistics[-1])
  )
  nr1 <- nrow(performance$thresholdSummary)
  performance$thresholdSummary <- tryCatch({cbind(analysisId=rep(analysisId,nr1),
                                                  Eval=rep('validation', nr1),
                                                  performance$thresholdSummary)},
                                           error = function(e){return(NULL)})
  nr1 <- nrow(performance$demographicSummary)
  if(!is.null(performance$demographicSummary)){
    performance$demographicSummary <- cbind(analysisId=rep(analysisId,nr1),
                                            Eval=rep('validation', nr1),
                                            performance$demographicSummary)
  }
  nr1 <- nrow(performance$calibrationSummary)
  performance$calibrationSummary <- cbind(analysisId=rep(analysisId,nr1),
                                          Eval=rep('validation', nr1),
                                          performance$calibrationSummary)
  nr1 <- nrow(performance$predictionDistribution)
  performance$predictionDistribution <- tryCatch({cbind(analysisId=rep(analysisId,nr1),
                                                        Eval=rep('validation', nr1),
                                                        performance$predictionDistribution)}, error = function(e){return(NULL)})

  delta <- start.pred - Sys.time()
  if (!silent)
    ParallelLogger::logInfo(paste("Evaluation completed at ", Sys.time(), " taking ", signif(delta, 3), attr(delta, "units") ))

  if (!silent)
    ParallelLogger::logInfo(paste("Starting covariate summary at ", Sys.time()))
  start.pred  <- Sys.time()
  # covSum <- covariateSummary(plpData, population, model = plpModel)

  delta <- start.pred - Sys.time()
  if (!silent)
    ParallelLogger::logInfo(paste("Covariate summary completed at ", Sys.time(), " taking ", signif(delta, 3), attr(delta, "units")))

  executionSummary <- list(PackageVersion = list(rVersion= R.Version()$version.string,
                                                 packageVersion = utils::packageVersion("PatientLevelPrediction")),
                           PlatformDetails= list(platform= R.Version()$platform,
                                                 cores= Sys.getenv('NUMBER_OF_PROCESSORS'),
                                                 RAM=utils::memory.size()), #  test for non-windows needed
                           # Sys.info()
                           TotalExecutionElapsedTime = NULL,
                           ExecutionDateTime = Sys.Date())

  result <- list(prediction = prediction,
                 performanceEvaluation = performance,
                 inputSetting = list(outcomeId=attr(population, "metaData")$outcomeId,
                                     cohortId= plpData$metaData$call$cohortId,
                                     database = plpData$metaData$call$cdmDatabaseSchema),
                 executionSummary = executionSummary,
                 model = list(model='applying plp model',
                              modelAnalysisId = plpModel$analysisId,
                              modelSettings = plpModel$modelSettings),
                 analysisRef=list(analysisId=NULL,
                                  analysisName=NULL,
                                  analysisSettings= NULL))
  return(result)
}
