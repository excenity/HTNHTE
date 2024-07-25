#' Generate the cohorts for the study
#'
#' @details
#' This function loads a json file with all the study cohorts
#' that were created using ATLAS and then generates them in the
#' user specified OMOP CDM database
#'
#' @param connectionDetails  The OMOP CDM database connection details
#' @param cdmDatabaseSchema  The schema of the OMOP CDM data
#' @param cohortDatabaseSchema  The schema of the database where the cohort table will be created
#' @param cohortTable  the table name that will be created to contain the cohorts
#' @param incremental  Only regenerate new cohorts (i.e., skip cohorts that were previously generated and are unchanged)
#' @param incrementalFolder  Location to save a file that tracks what cohorts have been generated previously for incremental
#'
#' @return
#' The cohorts are generated into the specified cohort schema and table
#'
#' @export
#'
generateCohorts <- function(
    connectionDetails,
    cdmDatabaseSchema,
    cohortDatabaseSchema,
    cohortTable = 'htn_hte',
    incremental = F,
    incrementalFolder = NULL
  ){

cohortTableNames <- CohortGenerator::getCohortTableNames(
  cohortTable = cohortTable
  )

CohortGenerator::createCohortTables(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTableNames = cohortTableNames,
  incremental = incremental
  )

# load the cohort definitions
cohortDefinitionSet <- ParallelLogger::loadSettingsFromJson(
  system.file(package = 'HTNHTE', 'cohorts.json')
)

CohortGenerator::generateCohortSet(
  cohortDefinitionSet = cohortDefinitionSet,
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortTableNames = cohortTableNames,
  incremental = incremental,
  incrementalFolder = incrementalFolder
)

return(invisible(T))
}


#' Extract the data for the study
#'
#' @details
#' This function extracts the data for the study
#'
#' @param connectionDetails  The OMOP CDM database connection details
#' @param cdmDatabaseSchema  The schema of the OMOP CDM data
#' @param cdmDatabaseName. A friendly name for theOMOP CDM database
#' @param cohortDatabaseSchema  The schema of the database where the cohort table will be created
#' @param cohortTable  the table name that will be created to contain the cohorts
#'
#' @return
#' Extracts the data for the study and then converts into a sparse matrix
#'
#' @export
getData <- function(
    connectionDetails,
    cdmDatabaseSchema,
    cdmDatabaseName,
    cohortDatabaseSchema,
    cohortTable = 'htn_hte'
  ){

  demoFeatures <- FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsGender = T,
    useDemographicsRace = T,
    useDemographicsEthnicity = T
    )

  cohortFeatures <- FeatureExtraction::createCohortBasedCovariateSettings(
    analysisId = 667,
    covariateCohortDatabaseSchema = cohortDatabaseSchema,
    covariateCohortTable = cohortTable,
    covariateCohorts =
      data.frame(
  cohortId = c(
      1789836, 1789837, 1789838, 1789839,
      1789832, 1789840, 1789841, 1789842,
      1789829, 1789830, 1789833, 1789834,
      1789848, 1789849, 1789850, 1789851, 1789852
      ),
  cohortName = c(
    'T2DM', 'CKD', 'Sleep_Apnea', 'HF',
    'antidepressants', 'hormonal_therapy', 'statins', 'PPI',
    'acei', 'acei_diuretic', 'arb',  'arb_diuretic',
    'beta_blockers', 'ccb', 'ccb_acei',
    'ccb_arb', 'diuretics'
  )
  ),
    valueType = "binary",
    startDay = -365,
    endDay = 0
    )

  measurementFeature1 <- createMeasurementCovariateSettings(
    covariateName = 'SBP baseline',
    conceptSet = c(3004249),
    startDay = -30*6, endDay=0,
    aggregateMethod = 'recent',
    covariateId = 1466,
    analysisId = 466
  )
  measurementFeature2 <- createMeasurementCovariateSettings(
    covariateName = 'DBP baseline',
    conceptSet = c(3012888),
    startDay = -30*6, endDay=0,
    aggregateMethod = 'recent',
    covariateId = 2466,
    analysisId = 466
  )
  measurementFeature3 <- createMeasurementCovariateSettings(
    covariateName = 'total_cholesterol baseline',
    conceptSet = c(3019900, 3027114),
    startDay = -30*6, endDay=0,
    aggregateMethod = 'recent',
    covariateId = 3466,
    analysisId = 466
  )
  measurementFeature4 <- createMeasurementCovariateSettings(
    covariateName = 'LDL baseline',
    conceptSet = c(3001308,3009966,3035899,3035009,36032416,36031267,36031404,927157,42870529,40758569,3039873,3053341,3038988,4041556,4210878,3008631,3028288,3028437),
    startDay = -30*6, endDay=0,
    aggregateMethod = 'recent',
    covariateId = 4466,
    analysisId = 466
  )
  measurementFeature5 <- createMeasurementCovariateSettings(
    covariateName = 'creatinine baseline',
    conceptSet = c(3016723,3020564),
    startDay = -30*6, endDay=0,
    aggregateMethod = 'recent',
    covariateId = 5466,
    analysisId = 466
  )
  measurementFeature6 <- createMeasurementCovariateSettings(
    covariateName = 'BMI negControl',
    conceptSet = c(3038553),
    startDay = 30*1+1, endDay=30*6,
    aggregateMethod = 'mean',
    covariateId = 6466,
    analysisId = 466
  )
  measurementFeature7 <- createMeasurementCovariateSettings(
    covariateName = 'hba1c baseline',
    conceptSet = c(3004410),
    startDay = -30*6, endDay=0,
    aggregateMethod = 'recent',
    covariateId = 7466,
    analysisId = 466
  )

  measurementFeature8 <- createMeasurementCovariateSettings(
    covariateName = 'SBP 6 Months',
    conceptSet = c(3004249),
    startDay = 30*1+1, endDay=30*6,
    aggregateMethod = 'recent',
    covariateId = 8466,
    analysisId = 466
  )

  measurementFeature9 <- createMeasurementCovariateSettings(
    covariateName = 'DBP 6 months',
    conceptSet = c(3012888),
    startDay = 30*1+1, endDay=30*6,
    aggregateMethod = 'latest',
    covariateId = 9466,
    analysisId = 466
  )

  measurementFeature10 <- createMeasurementCovariateSettings(
    covariateName = 'BMI baseline',
    conceptSet = c(3038553),
    startDay = -30*6, endDay=0,
    aggregateMethod = 'recent',
    covariateId = 10466,
    analysisId = 466
  )

  data <- PatientLevelPrediction::getPlpData(
    databaseDetails = PatientLevelPrediction::createDatabaseDetails(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cdmDatabaseName = cdmDatabaseName,
      cohortDatabaseSchema = cohortDatabaseSchema,
      outcomeDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      outcomeTable = cohortTable,
      targetId = 1790266,
      outcomeIds = -999
      ),
    covariateSettings = list(
      demoFeatures,
      cohortFeatures,
      measurementFeature1,measurementFeature2,measurementFeature3,
      measurementFeature4,measurementFeature5,measurementFeature6,
      measurementFeature7, measurementFeature8, measurementFeature9, measurementFeature10
    ), restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(

    )
      )

  matrixData <- PatientLevelPrediction::toSparseM(
    plpData = data,
    cohort = data$cohorts
      )

return(matrixData)
}
