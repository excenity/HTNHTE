library(HTNHTE)
database <- 'jmdc'
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = keyring::key_get('dbms', 'all'),
  server = keyring::key_get('server', database),
  user = keyring::key_get('user', 'all'),
  password = keyring::key_get('password', 'all'),
  port = keyring::key_get('port', 'all')
)
generateCohorts(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = 'cdm_jmdc_v2906',
    cohortDatabaseSchema = 'scratch_jreps',
    cohortTable = 'htn_hte',
    incremental = F,
    incrementalFolder = NULL
)

data <- getData(
    cdmDatabaseSchema = 'cdm_jmdc_v2906',
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = 'scratch_jreps',
    cdmDatabaseName = database,
    cohortTable = 'htn_hte'
)


# code I played with the get the cocept id codes
codes <- read.csv('/Users/jreps/Documents/GitHub/HTN_HTE/OMOP_codes.csv')
table(codes$subtype)
codes$concept_id[codes$subtype == 'acei']
paste0(codes$concept_id[codes$type == 'htn_meds'], collapse = ',')
paste0(codes$concept_id[codes$subtype == 'bb_arb'], collapse = ',')

# comorbidities anytime prior
#T2DM, CKD, Sleep_Apnea, HF
# 1789836, 1789837, 1789838, 1789839

# concurrent_meds start in prior year
# antidepressants, hormonal_therapy, statins, PPI
#1789832,1789840, 1789841, 1789842

# labs - baseline 6 months before
# 'SBP', 'DBP','BMI', "total_cholesterol", "creatinine", "hba1c"
# 6months - 1 month to 6 months after
# 12months - 1 month to 12 months after
# 'SBP', 'DBP' - this is outcome
# 'BMI' 1 month to 6 months after

# outcomes control_6months, SBP_diff_6months



