extractCohortDefinitionSet <- function(saveLocation){
  cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
    cohortIds =  c(1789836, 1789837, 1789838, 1789839,
                   1789832,1789840, 1789841, 1789842,
                   1789828,
                   1789829, 1789830, 1789833, 1789834, 1789835, 1789847, 1789848,
                   1789849, 1789850, 1789851, 1789852
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
# /Users/jreps/Documents/GitHub/HTN_HTE/inst
#extractCohortDefinitionSet('/Users/jreps/Documents/GitHub/HTN_HTE/inst')
