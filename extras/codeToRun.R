library(PsychosisMultimodalValidation)

# add details of your database setting:
databaseName <- 'add a shareable name for the database you are currently validating on'

# add the cdm database schema with the data
cdmDatabaseSchema <- 'your cdm database schema for the validation'

# add the work database schema this requires read/write privileges
cohortDatabaseSchema <- 'your work database schema'

# if using oracle please set the location of your temp schema
oracleTempSchema <- NULL

# the name of the table that will be created in cohortDatabaseSchema to hold the cohorts
cohortTable <- 'PsychosisMultimodalValidationCohortTable'

# the location to save the prediction models results to:
# NOTE: if you set the outputFolder to the 'Validation' directory in the
#       prediction study outputFolder then the external validation will be
#       saved in a format that can be used by the shiny app
outputFolder <- '../Validation'

# add connection details:
options(fftempdir = 'T:/fftemp')


dbms <- 'dbms'
user <- Sys.getenv("userID")
pw <- Sys.getenv("userPW")
server <- Sys.getenv('server17')
port <- 'port'
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port,
                                                                pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER"))

#note title like '(\'%Admission%note%\')'
noteTitle1 = '(\'%Admission%note%\')'
noteTitle2 = '(\'%초기간호%평가%\')'
noteTitle3 = '(\'%심리평가%\')'

# Now run the study
psytestV1Validation::execute(connectionDetails = connectionDetails,
                             databaseName = databaseName,
                             cdmDatabaseSchema = cdmDatabaseSchema,
                             cohortDatabaseSchema = cohortDatabaseSchema,
                             oracleTempSchema = oracleTempSchema,
                             cohortTable = cohortTable,
                             outputFolder = outputFolder,
                             createCohorts = T,
                             runValidation = T,
                             packageResults = F,
                             minCellCount = 5,
                             sampleSize = NULL,
                            noteTitle1 = noteTitle1,
                            noteTitle2 = noteTitle2,
                            noteTitle3 = noteTitle3)

# the results will be saved to outputFolder.  If you set this to the
# predictionStudyResults/Validation package then the validation results
# will be accessible to the shiny viewer

# to package the results run (run after the validation results are complete):
# NOTE: the minCellCount = N will remove any result with N patients or less
psytestV1Validation::execute(connectionDetails = connectionDetails,
                                 databaseName = databaseName,
                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                 oracleTempSchema = oracleTempSchema,
                                 cohortTable = cohortTable,
                                 outputFolder = outputFolder,
                                 createCohorts = F,
                                 runValidation = F,
                                 packageResults = T,
                                 minCellCount = 5,
                                 sampleSize = NULL)
