# 1790266: Target Cohort
# 1789829: ACEi
# 1789830: ACEi/Diuretic
# 1789832: Anti-depressants
# 1789833: ARB
# 1789834: ARB/Diuretics
# 1789835: BB/ARB
# 1789847: BB/Diuretics
# 1789848: BB
# 1789849: CCB
# 1789850: ACEi/CCB
# 1789851: ARB/CCB
# 1789852: Diuretics
# 1789836: T2DM
# 1789837: CKD
# 1789838: Sleep Apnea
# 1789839: HF
# 1789840: Hormonal Therapy
# 1789831: Aldosteronism
# 1789841: Statins
# 1789842: PPI

# Concepts Sets
# First-Line Drugs + Combo: 1884479
# other HTN meds: 1884484


#' Download cohort defintitions from ATLAS
#'
#' @details
#' This function downloads cohort definitions from ATLAS
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
extractCohortDefinitionSet <- function(saveLocation){
  cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
    cohortIds =  c(1789836, 1789837, 1789838, 1789839,
                   1789832, 1789840, 1789841, 1789842,
                   1790266, 1789829, 1789830, 1789833,
                   1789834, 1789848, 1789849, 1789852,
                   1791469, 1791470
    ),
    generateStats = TRUE,
    baseUrl = 'https://api.ohdsi.org/WebAPI'
  )
  ParallelLogger::saveSettingsToJson(
    object = cohortDefinitionSet,
    fileName = file.path(saveLocation, 'cohorts.json')
  )
  return(invisible(T))
}

#extractCohortDefinitionSet('inst')

