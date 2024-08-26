# Copyright 2024 Observational Health Data Sciences and Informatics
#
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Extracts covariates based on measurements
#'
#' @details
#' This extracts measurement values for a concept set of measurement concept ids
#'
#' @param connection  The database connection
#' @param oracleTempSchema  The temp schema if using oracle
#' @param cdmDatabaseSchema  The schema of the OMOP CDM data
#' @param cdmVersion  version of the OMOP CDM data
#' @param cohortTable  the table name that contains the target population cohort
#' @param rowIdField  string representing the unique identifier in the target population cohort
#' @param aggregated  whether the covariate should be aggregated
#' @param cohortId  cohort id for the target population cohort
#' @param covariateSettings  settings for the covariate cohorts and time periods
#'
#' @return
#' A measurement value for people with the measurement
#'
#' @export
getMeasurementCovariateData <- function(
    connection,
    oracleTempSchema = NULL,
    cdmDatabaseSchema,
    cdmVersion = "5",
    cohortTable = "#cohort_person",
    rowIdField = "row_id",
    aggregated,
    cohortId,
    covariateSettings,
    ...
) {

  # Some SQL to construct the covariate:
  sql <- paste("select c.@row_id_field AS row_id, measurement_concept_id, unit_concept_id,",
               "value_as_number, ",
               "measurement_date, abs(datediff(dd, measurement_date, c.cohort_start_date)) as index_time",
               "from @cdm_database_schema.measurement m inner join @cohort_temp_table c on c.subject_id = m.person_id",
               "and measurement_date >= dateadd(day, @start_day, cohort_start_date) and ",
               "measurement_date <= dateadd(day, @end_day, cohort_start_date)",
               "inner join @cdm_database_schema.person p on p.person_id=c.subject_id",
               "where m.measurement_concept_id in (@concepts);"
  )

  sql <- SqlRender::render(
    sql,
    cohort_temp_table = cohortTable,
    row_id_field = rowIdField,
    start_day= covariateSettings$startDay,
    end_day= covariateSettings$endDay,
    concepts = paste(covariateSettings$conceptSet, collapse = ','),
    cdm_database_schema = cdmDatabaseSchema
  )
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql)
  # Convert colum names to camelCase:
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))

  # map data:
  covariates <- covariates[!is.na(covariates$valueAsNumber),]
  #covariates <- covariateSettings$scaleMap(covariates)

  # aggregate data:
  if(covariateSettings$aggregateMethod == 'max'){
    covariates <- covariates %>% dplyr::group_by(.data$rowId) %>%
      dplyr::summarize(covariateValue = max(.data$valueAsNumber))
  } else if(covariateSettings$aggregateMethod == 'min'){
    covariates <- covariates %>% dplyr::group_by(.data$rowId) %>%
      dplyr::summarize(covariateValue = min(.data$valueAsNumber))
  } else if(covariateSettings$aggregateMethod == 'mean'){
    covariates <- covariates %>% dplyr::group_by(.data$rowId) %>%
      dplyr::summarize(covariateValue = mean(.data$valueAsNumber))
  } else if(covariateSettings$aggregateMethod == 'median'){
    covariates <- covariates %>% dplyr::group_by(.data$rowId) %>%
      dplyr::summarize(covariateValue = median(.data$valueAsNumber))
  } else if(covariateSettings$aggregateMethod == 'latest'){
    last <- covariates %>% dplyr::group_by(.data$rowId) %>%
      dplyr::summarize(lastTime = max(.data$indexTime))
    covariates <- merge(covariates,last,
                        by.x = c('rowId','indexTime'),
                        by.y = c('rowId','lastTime') )

    covariates <- covariates %>% dplyr::group_by(.data$rowId) %>%
      dplyr::summarize(covariateValue = mean(.data$valueAsNumber))
  }
  else{
    last <- covariates %>% dplyr::group_by(rowId) %>%
      dplyr::summarize(recentTime = min(indexTime))
    covariates <- merge(covariates,last,
                        by.x = c('rowId','indexTime'),
                        by.y = c('rowId','recentTime') )

    covariates <- covariates %>% dplyr::group_by(.data$rowId) %>%
      dplyr::summarize(covariateValue = mean(.data$valueAsNumber))
  }

  # add covariateID:
  covariates$covariateId <- covariateSettings$covariateId

  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = covariateSettings$covariateId,
                             covariateName = paste('Measurement during day',
                                                   covariateSettings$startDay,
                                                   'through',
                                                   covariateSettings$endDay,
                                                   'days relative to index:',
                                                   covariateSettings$covariateName
                                                   ),
                             analysisId = covariateSettings$analysisId,
                             conceptId = 0)

  analysisRef <- data.frame(analysisId = covariateSettings$analysisId,
                            analysisName = "measurement covariate",
                            domainId = "measurement covariate",
                            startDay = covariateSettings$startDay,
                            endDay = covariateSettings$endDay,
                            isBinary = "N",
                            missingMeansZero = "N")

  result <- Andromeda::andromeda(covariates = covariates,
                                 covariateRef = covariateRef,
                                 analysisRef = analysisRef)
  class(result) <- "CovariateData"
  return(result)
}


createMeasurementCovariateSettings <- function(
    covariateName, conceptSet,
    startDay=-30, endDay=0,
    aggregateMethod = 'recent',
    covariateId = 1466,
    analysisId = 466
) {

  covariateSettings <- list(covariateName=covariateName,
                            conceptSet=conceptSet,
                            startDay=startDay,
                            endDay=endDay,
                            covariateId = covariateId,
                            analysisId = analysisId,
                            aggregateMethod = aggregateMethod
  )

  attr(covariateSettings, "fun") <- "HTNHTE::getMeasurementCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}
