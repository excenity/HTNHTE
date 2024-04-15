# STEP 2: Data wrangling for analytical dataset 
# Version: 04/10/24
# Status: Ready

# External Script for Data Wrangling

if (!require(data.table))
{
  install.packages("data.table")
}
if (!require(Amelia))
{
  install.packages("Amelia")
}
if (!require(mice))
{
  install.packages("mice")
}
if (!require(tableone))
{
  install.packages("tableone")
}
if (!require(caret))
{
  install.packages("caret")
}
if (!require(ggplot2))
{
  install.packages("ggplot2")
}

library(data.table)
library(tidyverse)
library(lubridate)
library(Amelia)
library(tableone)
library(mice)
library(caret)
library(cobalt)
library(ggplot2)

cohort$cohort_start_date = as.Date(cohort$cohort_start_date)

## ---- demographics

demo$age = 2024 - demo$year_of_birth # check year

# gender
demo$gender = 'M'
demo$gender[demo$gender_concept_id == 8532] = 'F'
demo$gender[demo$gender_concept_id == 8551] = 'M' 

# race
demo$race = 'White'
demo$race[demo$race_concept_id == 8515] = 'Asian/Other'
demo$race[demo$race_concept_id == 8516] = 'Black'
demo$race[demo$race_concept_id == 8515] = 'Asian/Other'
demo$race[demo$race_concept_id == 8657] = 'Asian/Other'
demo$race[demo$race_concept_id == 8557] = 'Asian/Other'
demo$race[demo$race_concept_id == 38003574] = 'Asian/Other'
demo$race[demo$race_concept_id == 38003579] = 'Asian/Other'
demo$race[demo$race_concept_id == 38003581] = 'Asian/Other'
demo$race[demo$race_concept_id == 38003584] = 'Asian/Other'
demo$race[demo$race_concept_id == 38003585] = 'Asian/Other'
demo$race[demo$race_concept_id == 38003592] = 'Asian/Other'
demo$race[demo$race_concept_id == 38003613] = 'Asian/Other'

demo$ethnicity[demo$ethnicity_concept_id == 38003563] = 'Hispanic'
demo$ethnicity[demo$ethnicity_concept_id == 38003564] = 'Non-Hispanic'
demo$ethnicity[demo$ethnicity_concept_id == 0] = 'Non-Hispanic'

demo = demo %>% select(-gender_concept_id, -race_concept_id, -ethnicity_concept_id, -year_of_birth)


## ---- Intervention Assignment (Hypertension)
# join cross-walk
htn_meds_codes = OMOP_codes %>% filter(type == 'htn_meds')
htn_meds = left_join(htn_meds, htn_meds_codes, join_by(drug_concept_id == concept_id))

# join cohort
htn_meds = inner_join(cohort, htn_meds, join_by(person_id == person_id))
htn_meds_first = htn_meds %>% filter(cohort_start_date == drug_exposure_start_date)

# spread dataset
htn_meds_first$flag = 1
htn_meds_first = htn_meds_first %>% distinct(person_id, drug_exposure_start_date, cohort_start_date, subtype, flag) %>% pivot_wider(id_cols = c('person_id', 'drug_exposure_start_date', 'cohort_start_date'), names_from = 'subtype', values_from = 'flag', values_fill = 0) 

# remove those with more than 3 classes
htn_meds_first$multiclass = htn_meds_first$diuretics + htn_meds_first$acei + htn_meds_first$arb + htn_meds_first$ccb + htn_meds_first$`beta_blockers` + htn_meds_first$acei_arb_diuretic
htn_meds_first = htn_meds_first %>% filter(multiclass <= 2)

# find acei/arb/diuretics combos
htn_meds_first$`acei_arb_diuretic` = 0
htn_meds_first$`bb_diuretics` = 0
htn_meds_first$`ccb_acei_arb` = 0

htn_meds_first$`acei_arb_diuretic`[(htn_meds_first$diuretics == 1 & htn_meds_first$acei == 1)] = 1
htn_meds_first$`acei_arb_diuretic`[(htn_meds_first$diuretics == 1 & htn_meds_first$arb == 1)] = 1
htn_meds_first$`bb_diuretics`[(htn_meds_first$beta_blockers == 1 & htn_meds_first$diuretics == 1)] = 1
htn_meds_first$`ccb_acei_arb`[(htn_meds_first$ccb == 1 & htn_meds_first$acei == 1)] = 1
htn_meds_first$`ccb_acei_arb`[(htn_meds_first$ccb == 1 & htn_meds_first$arb == 1)] = 1

# filter out other combos
htn_meds_first = htn_meds_first %>% filter(htn_meds_first$multiclass == 1 | acei_arb_diuretic == 1 | bb_diuretics == 1 | ccb_acei_arb == 1)
htn_meds_first$acei[htn_meds_first$acei_arb_diuretic == 1] = 0
htn_meds_first$arb[htn_meds_first$acei_arb_diuretic == 1] = 0
htn_meds_first$diuretics[htn_meds_first$acei_arb_diuretic == 1] = 0
htn_meds_first$beta_blockers[htn_meds_first$bb_diuretics == 1] = 0
htn_meds_first$diuretics[htn_meds_first$bb_diuretics == 1] = 0
htn_meds_first$ccb[htn_meds_first$ccb_acei_arb == 1] = 0
htn_meds_first$acei[htn_meds_first$ccb_acei_arb == 1] = 0
htn_meds_first$arb[htn_meds_first$ccb_acei_arb == 1] = 0

htn_meds_first = htn_meds_first %>% pivot_longer(cols = c("diuretics", "arb", "ccb", "acei", "beta_blockers", "acei_arb_diuretic", "bb_diuretics", "ccb_acei_arb"), names_to = 'htn_med_class', values_to = 'flag') 
htn_meds_first = htn_meds_first %>% filter(flag == 1) %>% distinct(person_id, drug_exposure_start_date, cohort_start_date, htn_med_class)

# number of prescriptions per class
htn_meds_cat_plot = ggplot(htn_meds_first, aes(x = htn_med_class)) + geom_bar() + theme_bw() 
ggsave(file.path(path, 'results/step0_datasetCreation/med_class_count.png'), htn_meds_cat_plot, width = 10, height = 5)
med_class_count = as.data.frame(table(htn_meds_first$htn_med_class))
write.csv(med_class_count, file.path(path, 'results/step0_datasetCreation/med_class_count.csv'), row.names = F)


## ---- co-morbidities

comorb_codes = OMOP_codes %>% filter(type == 'Comorbidities')
comorbidities = left_join(comorbidities, comorb_codes, join_by(condition_concept_id == concept_id))
comorbidities = comorbidities %>% distinct(person_id, condition_start_date, subtype)

comorbidities$condition_start_date = as.Date(comorbidities$condition_start_date)
comorbidities_first = as.data.frame(comorbidities %>% group_by(person_id, subtype) %>% summarise_at(vars(condition_start_date), min))
comorbidities_first = inner_join(comorbidities, comorbidities_first)

comorbidities_first = left_join(comorbidities_first, cohort)
comorbidities_first = comorbidities_first %>% filter(condition_start_date <= cohort_start_date)

comorbidities_first$flag = 1
comorbidities_first = as.data.frame(comorbidities_first %>% pivot_wider(id_cols = 'person_id', names_from = 'subtype', values_from = 'flag'))

comorbidities_first = left_join(cohort, comorbidities_first)
comorbidities_first = comorbidities_first %>% select(person_id, T2DM, CKD, Sleep_Apnea, HF)

comorbidities_first[is.na(comorbidities_first)] = 0


## ----concurrent_meds
concurrent_meds_codes = OMOP_codes %>% filter(type == 'concurrent_meds')
concurrent_meds = left_join(concurrent_meds, concurrent_meds_codes, join_by(drug_concept_id == concept_id))

concurrent_meds = concurrent_meds %>% distinct(person_id, drug_exposure_start_date, subtype)
concurrent_meds = left_join(concurrent_meds, cohort)
concurrent_meds$drug_exposure_start_date = as.Date(concurrent_meds$drug_exposure_start_date)
concurrent_meds = concurrent_meds %>% filter(drug_exposure_start_date <= cohort_start_date & drug_exposure_start_date >= cohort_start_date %m-% years(1)) 
concurrent_meds = concurrent_meds %>% distinct(person_id, subtype)

concurrent_meds$flag = 1
concurrent_meds = as.data.frame(concurrent_meds %>% pivot_wider(id_cols = 'person_id', names_from = 'subtype', values_from = 'flag'))

concurrent_meds = left_join(cohort, concurrent_meds)

concurrent_meds = concurrent_meds %>% select(person_id, antidepressants, hormonal_therapy, statins, PPI)
concurrent_meds[is.na(concurrent_meds)] = 0


## ---- baseline labs
labs = inner_join(labs, cohort)

# format dates
labs$measurement_date = as.Date(labs$measurement_date)
labs$cohort_start_date = as.Date(labs$cohort_start_date)

# filter for date range 
b_labs = labs %>% filter(measurement_date <= cohort_start_date & measurement_date >= cohort_start_date %m-% months(6)) # within 6 months

lab_codes = OMOP_codes %>% filter(type == 'labs_vitals')
b_labs = left_join(b_labs, lab_codes, join_by(measurement_concept_id == concept_id))

# find most recent labs 
b_labs_min = b_labs %>% group_by(person_id, subtype) %>% summarise_at(vars(measurement_date),  max)
b_labs_min = left_join(b_labs_min, b_labs)
b_labs_min$time_diff = as.numeric(b_labs_min$measurement_date - b_labs_min$cohort_start_date)
time_diff_plot = ggplot(b_labs_min, aes(x = time_diff)) + geom_histogram() + theme_bw() 
ggsave(file.path(path, 'results/step0_datasetCreation/labs_time_diff.png'), plot = time_diff_plot) # save plot of time diff of lab variables and prescription 
b_labs_min = b_labs_min %>% group_by(person_id, measurement_date, subtype) %>% summarise_at(vars(value_as_number), mean, na.rm = T)

# spread labs measurements
b_labs_min = b_labs_min %>% pivot_wider(id_cols = c('person_id'), names_from = 'subtype', values_from = 'value_as_number')


## ----outcome labs
bp_codes = OMOP_codes %>% filter(subtype %in% c('SBP', 'DBP'))
bp = labs %>% filter(measurement_concept_id %in% bp_codes$concept_id)
bp = left_join(bp, bp_codes, join_by(measurement_concept_id == concept_id))

bp_baseline = bp %>% filter(measurement_date <= cohort_start_date & measurement_date >= cohort_start_date %m-% months(6))
bp_6months = bp %>% filter((measurement_date > cohort_start_date %m+% months(1) & measurement_date <= cohort_start_date %m+% months(6)))
bp_12months = bp %>% filter((measurement_date > cohort_start_date %m+% months(1) & measurement_date <= cohort_start_date %m+% months(12)))

bp_baseline$time_diff = as.numeric(bp_baseline$measurement_date - bp_baseline$cohort_start_date)
bp_6months$time_diff = as.numeric(bp_6months$measurement_date - bp_6months$cohort_start_date)
bp_12months$time_diff = as.numeric(bp_12months$measurement_date - bp_12months$cohort_start_date)

# get latest measurement in each group and get averages
bp_latest = function(bp_df, name){
  bp_df_max = bp_df %>% group_by(person_id, measurement_concept_id) %>% summarise_at(vars(measurement_date), max)
  bp_df_max = inner_join(bp_df, bp_df_max)
  bp_df_max_plot = ggplot(bp_df_max, aes(x = time_diff)) + geom_histogram() + theme_bw()
  ggsave(file.path(path, name), bp_df_max_plot)
  bp_df_max = bp_df %>% group_by(person_id, subtype) %>% summarise_at(vars(value_as_number), mean, na.rm = T)
  
  bp_df_max = bp_df_max %>% pivot_wider(id_cols = 'person_id', names_from = 'subtype', values_from = 'value_as_number')
  
  return(bp_df_max)
}

bp_baseline = bp_latest(bp_baseline, 'results/step0_datasetCreation/bp_bl_timediff.png')
bp_6months = bp_latest(bp_6months, 'results/step0_datasetCreation/bp_6m_timediff.png')
bp_6months = rename(bp_6months, DBP_6months = DBP, SBP_6months = SBP)
bp_12months = bp_latest(bp_12months, 'results/step0_datasetCreation/bp_12m_timediff.png')
bp_12months = rename(bp_12months, DBP_12months = DBP, SBP_12months = SBP)


## ---- Join Datasets
final_df = left_join(demo, htn_meds_first)
final_df = left_join(final_df, comorbidities_first)
final_df = left_join(final_df, concurrent_meds)
final_df = left_join(final_df, b_labs_min)
final_df = left_join(final_df, bp_6months)
final_df = left_join(final_df, bp_12months)


## ---- remove missing records
print(nrow(final_df))
final_df = final_df %>% filter(!is.na(htn_med_class)) # remove those missing treatment assignment
print(paste('N after removing missing treatment assignmend:', nrow(final_df)))
final_df = final_df %>% filter(!is.na(SBP) & !is.na(DBP)) # remove those without baseline SBP & DBP measurements
print(paste('N after removing missing baseline BP measurement:', nrow(final_df)))
final_df = final_df %>% filter(!is.na(SBP_6months)) # remove those without outcomes data at 6 months
print(paste('N after removing missing outcome at 6 months:', nrow(final_df)))
final_df = final_df %>% filter(age > 18)
print(paste('N after removing those under age 18:', nrow(final_df)))
final_df = final_df %>% filter(SBP >= 130 | DBP >= 80)
print(paste('N after not at control at prescription:', nrow(final_df)))

final_df = final_df %>% select(-c('LDL')) # check on availability of LDL


## ---- Missingness Graphics
png(file.path(path, 'results/step0_datasetCreation/missmap.png'), width = 800, height = 600)
missmap(final_df)
dev.off()


## ---- Bounding Continuous Values and Multiple Imputation
# Bounding BMI, total cholesterol, and creatinine
final_df$BMI[final_df$BMI < 12 | final_df$BMI > 80] = NA
final_df$total_cholesterol[final_df$total_cholesterol < 100 | final_df$total_cholesterol > 300] = NA
final_df$creatinine[final_df$creatinine < 0.5  | final_df$creatinine > 5] = NA

# normalize variables
norm_vars = c('BMI', 'total_cholesterol', 'creatinine')
norm_process = preProcess(final_df %>% select(norm_vars), method = 'range')
norm_df = predict(norm_process, final_df %>% select(norm_vars))
final_df[, norm_vars] = norm_df

# multiple imputation
mice_df = mice(final_df %>% select(-c(person_id, drug_exposure_start_date, cohort_start_date)), m = 1, maxit = 1000, method = 'pmm', seed = 618, verbose = F)
png(file.path(path, 'results/step0_datasetCreation/imputation.png'), width = 800, height = 600)
densityplot(mice_df)
dev.off()

# rescale
norm_df = complete(mice_df) %>% select(norm_vars)

ranges = norm_process$ranges[2,] - norm_process$ranges[1,]
for (i in 1:length(ranges))
{
  norm_df[,i] = norm_df[,i] * ranges[i]
  norm_df[,i] = norm_df[,i] + norm_process$ranges[1, i]
}

# combine imputed data with full dataset
mice_df = complete(mice_df)
mice_df = mice_df %>% select(-norm_vars) 
mice_df = cbind(mice_df, norm_df)

final_df = cbind(final_df %>% select(c(person_id, drug_exposure_start_date, cohort_start_date)), mice_df)


## ---- label outcomes
final_df$control_6months = 0
final_df$control_6months[final_df$SBP_6months <= 130 & final_df$DBP_6months <= 80] = 1
final_df$control_12months = 0
final_df$control_12months[final_df$SBP_12months <= 130 & final_df$DBP_12months <= 80] = 1

# secondary outcome
final_df$SBP_diff_12months = final_df$SBP_12months - final_df$SBP
final_df$SBP_diff_6months = final_df$SBP_6months - final_df$SBP


## ---- Final Analytic Dataset
final_df[,c('gender', 'race', 'ethnicity', 'htn_med_class', 'HF', 'T2DM', "CKD", "Sleep_Apnea", "antidepressants", "hormonal_therapy",
            "statins", "PPI", "control_6months", "control_12months")] =
  lapply(final_df[,c('gender', 'race', 'ethnicity', 'htn_med_class', 'HF', 'T2DM', "CKD", "Sleep_Apnea", "antidepressants", "hormonal_therapy",
                     "statins", "PPI", "control_6months", "control_12months")], factor)

# all data
final_df_tab = final_df %>% select(c('htn_med_class','age', 'gender', 'race', 'ethnicity', 'HF', 'T2DM', 'CKD', 'Sleep_Apnea',"antidepressants", 
                                     "hormonal_therapy", "statins", "PPI", "SBP", "DBP", 'BMI', 'total_cholesterol', 'creatinine', 'hba1c', 'control_6months', 'SBP_diff_6months')) 
tab_cat_vars = c('gender',' race', 'ethnicity', 'HF', 'T2DM', 'CKD', 'Sleep_Apnea',"antidepressants", "hormonal_therapy", "statins", "PPI", 'control_6months')
tab_vars = c('age', 'gender', 'race', 'ethnicity','HF', 'T2DM', 'CKD', 'Sleep_Apnea',"antidepressants", "hormonal_therapy", "statins", "PPI", "SBP", "DBP", 'BMI', 'total_cholesterol', 'creatinine', 'hba1c', 'control_6months', 'SBP_diff_6months')
table1 = CreateTableOne(data = final_df_tab, strata = 'htn_med_class', test = F, vars = tab_vars, factorVars = tab_cat_vars, addOverall = T)

table1 = print(table1, smd = T)
write.csv(table1, file.path(path, 'results/step0_datasetCreation/table1.csv'), row.names = T)
