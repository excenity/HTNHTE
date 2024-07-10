# 1789828: Target Cohort
# 1789829: ACEi
# 1789830: ACEi/Diuretic
# 1789833: ARB
# 1789834: ARB/Diuretics
# 1789835: BB/ARB
# 1789847: BB/Diuretics
# 1789848: BB
# 1789849: CCB
# 1789850: ACEi/CCB
# 1789851: ARB/CCB
# 1789836: T2DM
# 1789837: CKD
# 1789838: Sleep Apnea
# 1789839: HF
# 1789832: Anti-depressants
# 1789840: Hormonal Therapy
# 1789840: Hormonal Therapy
# 1789831: Aldosteronism
# 1789841: Statins
# 1789842: PPI


extractCohortDefinitionSet <- function(saveLocation){
  cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
    cohortIds =  c(1789836, 1789837, 1789838, 1789839,
                   1789832, 1789840, 1789841, 1789842,
                   1789828, 1789829, 1789830, 1789833,
                   1789834, 1789835, 1789847, 1789848,
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
extractCohortDefinitionSet('/Users/excenity/Documents/HSIP/Research/Dissertation Project/Code/HTNHTE-package/inst')
