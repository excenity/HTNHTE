# STEP0A: Data Extraction via SqlRender (for SQLite)
# Version: 04/10/24
# Status: Ready

OMOP_codes = read.csv('OMOP_codes.csv')

# create folder for results
path = getwd()
dir.create(file.path(path, 'results'))


## Concept Set Codes
htn_meds_codes_list = OMOP_codes %>% filter(type == 'htn_meds') 
htn_meds_codes_list = as.vector(htn_meds_codes_list$concept_id)

dx_codes_list = OMOP_codes %>% filter(type == 'Comorbidities')
dx_codes_list = as.vector(dx_codes_list$concept_id)

labs_codes_list = OMOP_codes %>% filter(type == 'labs_vitals')
labs_codes_list = as.vector(labs_codes_list$concept_id)

concurrent_meds_codes_list = OMOP_codes %>% filter(type == 'concurrent_meds')
concurrent_meds_codes_list = as.vector(concurrent_meds_codes_list$concept_id)

## Create Cohort
sql_cohort = 
  "
  /* Test out Cohort Creation Code */

  -- STEP 1: Find first HTN diagnosis
  
  DROP TABLE IF EXISTS first_htn;
  
  CREATE TEMPORARY TABLE first_htn AS 
  SELECT dx.person_id AS subject_id, 
  MIN(CONDITION_START_DATE) AS htn_start_date
  FROM main.condition_occurrence AS dx
  JOIN main.visit_occurrence AS visit ON visit.visit_occurrence_id = dx.visit_occurrence_id AND visit_concept_id = 9202
  WHERE dx.CONDITION_CONCEPT_ID = 320128 -- hypertension SNOMED code
  GROUP BY dx.person_id
  HAVING htn_start_date >= 2010-01-01;
  
  SELECT COUNT(*) FROM first_htn;
  
  -- STEP 2: Get patients HTN med records and find those with prescriptions prior to HTN
  
  DROP TABLE IF EXISTS early_rx;
  
  CREATE TEMPORARY TABLE early_rx AS 
  SELECT person_id AS subject_id, 
  MIN(rx.drug_exposure_start_date) AS first_drug_start_date
  FROM main.drug_exposure AS rx
  JOIN first_htn AS pt ON rx.person_id = pt.subject_id
  WHERE rx.drug_concept_id IN (@htn_meds_codes)
  AND rx.drug_exposure_start_date < pt.htn_start_date
  GROUP BY person_id;
  
  SELECT COUNT(*) FROM early_rx;
  
  -- STEP 3: Find patients that are pregnant
  
  DROP TABLE IF EXISTS pregnant_pt;
  
  CREATE TEMPORARY TABLE pregnant_pt AS 
  SELECT person_id AS subject_id, 
  CONDITION_START_DATE AS preg_date, 
  pt.htn_start_date
  FROM main.condition_occurrence AS dx
  JOIN first_htn AS pt ON dx.person_id = pt.subject_id
  WHERE dx.CONDITION_CONCEPT_ID IN (4128331, 4299535) 
  AND dx.CONDITION_START_DATE BETWEEN DATE(pt.htn_start_date, '+1 years') AND DATE(pt.htn_start_date, '-1 years');
  
  SELECT COUNT(*) FROM pregnant_pt;
  
  -- STEP 4: Find patients with HTN medication prescription within 12 months of diagnosis and get their earliest prescription date
  
  DROP TABLE IF EXISTS htn_med_prescribed;
  
  CREATE TEMPORARY TABLE htn_med_prescribed AS 
  SELECT person_id,
  htn_start_date,
  rx.drug_exposure_start_date AS htn_drug_start_date,
  rx.drug_concept_id
  FROM main.drug_exposure AS rx
  JOIN first_htn AS pt ON rx.person_id = pt.subject_id
  WHERE rx.drug_concept_id IN (@htn_meds_codes)
  AND rx.drug_exposure_start_date BETWEEN pt.htn_start_date AND DATE(pt.htn_start_date, '+1 years')
  AND subject_id NOT IN (SELECT subject_id FROM pregnant_pt)
  AND subject_id NOT IN (SELECT subject_id FROM early_rx);
  
  CREATE TEMPORARY TABLE htn_med_prescribed_earliest AS 
  SELECT person_id, 
  htn_start_date,
  MIN(htn_drug_start_date) AS min_htn_drug_start_date
  FROM htn_med_prescribed
  GROUP BY person_id;
  
  -- STEP 5: Find patients with visits a year prior to their HTN diagnosis
  
  DROP TABLE IF EXISTS prior_visit;
  
  CREATE TEMPORARY TABLE prior_visit AS 
  SELECT person_id,
  visit_start_date
  FROM main.visit_occurrence AS visit 
  JOIN first_htn AS pt ON visit.person_id = pt.subject_id
  WHERE visit.visit_start_date BETWEEN DATE(pt.htn_start_date, '-1 years') AND pt.htn_start_date;
  
  -- STEP 6: Create cohort
  
  DROP TABLE IF EXISTS cohort_interest;
  
  CREATE TABLE cohort_interest AS 
  SELECT DISTINCT 120523 AS cohort_definition_id, 
  person_id AS subject_id,
  DATE(min_htn_drug_start_date) AS cohort_start_date, 
  htn_start_date,
  DATE(htn_start_date, '+1 years') AS cohort_end_date
  FROM htn_med_prescribed_earliest 
  WHERE htn_med_prescribed_earliest.person_id IN (SELECT person_id FROM prior_visit)
  ORDER BY subject_id;
"
renderTranslateExecuteSql(conn, sql_cohort, htn_meds_codes = htn_meds_codes_list)

## feature extraction
sql_featureExtraction =
  "
    /* co-morbidities */
  
  DROP TABLE IF EXISTS comorb;
  
  CREATE TEMPORARY TABLE comorb AS
  SELECT person_id,
  condition_concept_id, 
  DATE(condition_start_date) AS condition_start_date
  FROM main.condition_occurrence AS dx
  WHERE dx.condition_concept_id IN (@dx_codes);
  
  DROP TABLE IF EXISTS comorb_cohort;
  		
  CREATE TABLE comorb_cohort AS
  SELECT * 
  FROM comorb
  WHERE comorb.person_id IN (SELECT subject_id FROM cohort_interest);	

  /* labs and vitals */ 
  
  DROP TABLE IF EXISTS labs;
  
  CREATE TEMPORARY TABLE labs AS
  SELECT person_id,
  measurement_concept_id,
  DATE(measurement_date) AS measurement_date,
  value_as_number,
  value_as_concept_id
  FROM main.measurement AS m
  WHERE m.measurement_concept_id IN (@labs_codes);
  
  DROP TABLE IF EXISTS labs_cohort;
  
  CREATE TABLE labs_cohort AS
  SELECT * 
  FROM labs
  WHERE labs.person_id IN (SELECT subject_id FROM cohort_interest);
  
  /* concurrent meds */
  
  DROP TABLE IF EXISTS concurrent_meds;
  
  CREATE TEMPORARY TABLE concurrent_meds AS
  SELECT person_id,
  drug_concept_id,
  DATE(drug_exposure_start_date) AS drug_exposure_start_date
  FROM main.drug_exposure AS rx
  WHERE rx.drug_concept_id IN (@concurrent_meds_codes);
  
  DROP TABLE IF EXISTS concurrent_meds_cohort;
  
  CREATE TABLE concurrent_meds_cohort AS
  SELECT * 
  FROM concurrent_meds
  WHERE concurrent_meds.person_id IN (SELECT subject_id FROM cohort_interest);
  
  /* HTN Meds Prescription */
  
  DROP TABLE IF EXISTS htn_meds;
  
  CREATE TEMPORARY TABLE htn_meds AS 
  SELECT person_id,
  drug_concept_id, 
  DATE(drug_exposure_start_date) AS drug_exposure_start_date
  FROM main.drug_exposure AS rx
  JOIN main.cohort_interest AS pt ON rx.person_id = pt.subject_id
  WHERE rx.drug_concept_id IN (@htn_meds_codes);
  
  DROP TABLE IF EXISTS htn_meds_cohort;
  
  CREATE TABLE htn_meds_cohort AS
  SELECT * 
  FROM htn_meds
  WHERE htn_meds.person_id IN (SELECT subject_id FROM cohort_interest);
  
  /* Demographics */
  
  DROP TABLE IF EXISTS demo;
  
  CREATE TABLE demo AS 
  SELECT person_id, 
  gender_concept_id, 
  year_of_birth, 
  race_concept_id, 
  ethnicity_concept_id
  FROM person
  WHERE person.person_id IN (SELECT subject_id FROM cohort_interest)
  "
renderTranslateExecuteSql(conn, sql_featureExtraction, htn_meds_codes = htn_meds_codes_list, dx_codes = dx_codes_list, labs_codes = labs_codes_list, concurrent_meds_codes = concurrent_meds_codes_list, )


## Extract Covariate Tables

# demographics
sql_query = "SELECT * FROM demo;"
demo = renderTranslateQuerySql(conn, sql_query, )
names(demo) = str_to_lower(names(demo))

# cohort of interest
sql_query = "SELECT subject_id,
  DATE(cohort_start_date), 
  DATE(htn_start_date)
  FROM cohort_interest;"
cohort = renderTranslateQuerySql(conn, sql_query)
names(cohort)[1:3] = c('person_id', 'cohort_start_date', 'htn_start_date')
names(cohort) = str_to_lower(names(cohort))

# concurrent medications
sql_query = "SELECT person_id,
  drug_concept_id,
  DATE(drug_exposure_start_date)
  FROM concurrent_meds_cohort;"
concurrent_meds = renderTranslateQuerySql(conn, sql_query)
names(concurrent_meds)[3] = 'drug_exposure_start_date'
names(concurrent_meds) = str_to_lower(names(concurrent_meds))

# hypertension medications
sql_query = "SELECT person_id,
  drug_concept_id, 
  DATE(drug_exposure_start_date) 
  FROM htn_meds_cohort;"
htn_meds = renderTranslateQuerySql(conn, sql_query)
names(htn_meds)[3] = 'drug_exposure_start_date'
names(htn_meds) = str_to_lower(names(htn_meds))

# labs
sql_query = "SELECT person_id,
  measurement_concept_id,
  DATE(measurement_date),
  value_as_number
  FROM labs_cohort;"
labs = renderTranslateQuerySql(conn, sql_query)
names(labs)[3] = 'measurement_date'
names(labs) = str_to_lower(names(labs))

# co-morbidities 
sql_query = "SELECT person_id,
  condition_concept_id, 
  DATE(condition_start_date) 
  FROM comorb_cohort;"
comorbidities = renderTranslateQuerySql(conn, sql_query)
names(comorbidities)[3] = 'condition_start_date'
names(comorbidities) = str_to_lower(names(comorbidities))

## CONSORT Diagram

consort_df = function(df, name)
{
  names(df) = 'count'
  df$table_name = name
  return(df)
}

first_htn_count = renderTranslateQuerySql(conn, "SELECT COUNT(DISTINCT subject_id) FROM first_htn;")
first_htn_count = consort_df(first_htn_count, 'first_htn')
early_rx_count = renderTranslateQuerySql(conn, "SELECT COUNT(DISTINCT subject_id) FROM early_rx")
early_rx_count = consort_df(early_rx_count, 'early_rx')
pregnant_pt_count = renderTranslateQuerySql(conn, "SELECT COUNT(DISTINCT subject_id) FROM pregnant_pt WHERE subject_id IN (SELECT subject_id FROM early_rx);")
pregnant_pt_count = consort_df(pregnant_pt_count, 'pregnant_pt')
htn_med_presc_early_count = renderTranslateQuerySql(conn, "SELECT COUNT(DISTINCT person_id) FROM htn_med_prescribed_earliest;")
htn_med_presc_early_count = consort_df(htn_med_presc_early_count, 'htn_med_presc_early')
prior_visit_count = renderTranslateQuerySql(conn, "SELECT COUNT(DISTINCT person_id) FROM prior_visit WHERE person_id IN (SELECT person_id FROM htn_med_prescribed_earliest);")
prior_visit_count = consort_df(prior_visit_count, 'prior_visit')

consort = rbind(first_htn_count, early_rx_count, pregnant_pt_count, htn_med_presc_early_count, prior_visit_count)

# output CONSORT table
dir.create(file.path(path, 'results/step0_datasetCreation'), showWarnings = F)
write.csv(consort, paste0(path, '/results/step0_datasetCreation/consort_df.csv'), row.names = F)
