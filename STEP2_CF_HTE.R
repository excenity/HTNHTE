# STEP 2: Causal Forest for Heterogeneous Treatment Effects Analysis
# Version: 04/11/24

if (!require(grf))
{
  install.packages("grf")
}

library(data.table)
library(tidyverse)
library(grf)

## ---- Data and Analysis Prep

if (output_analytical_dataset == T)
{
  df = fread(file.path(path, 'final_df.csv'))
}

df = final_df
df = df %>% select(-c(drug_exposure_start_date, cohort_start_date, DBP_12months, SBP_12months, DBP_6months, SBP_6months))

# med class list
htn_med_list = c('acei', 'arb', 'ccb', 'diuretics', 'acei_arb_diuretic')

dir.create(file.path(path, 'results/step2_CF_analysis'))


## ---- CF Helper Function
CF_analysis = function(outcome, htn_med_class_i)
{
  if (outcome == 'at_control')
  {
    ite = fread(file.path(path, paste0('results/step1_ITE_estimation/ITE_at_control_', htn_med_class_i, '.csv')), col.names = c('person_id', 'ite'))
  } else if (outcome == 'sbp_change')
    {
      ite = fread(file.path(path, paste0('results/step1_ITE_estimation/ITE_SBP_change_', htn_med_class_i, '.csv')), col.names = c('person_id', 'ite'))
  }
  
  ## Dataset Setup
  
  df$htn_med_class[df$htn_med_class != htn_med_class_i ] = 'other'
  df$htn_med_class = factor(df$htn_med_class, levels = c('other', htn_med_class_i))
  
  # join dataset
  ite = left_join(ite, df)
  
  # format all categorical variables
  ite$gender = as.numeric(factor(ite$gender))
  ite$race = as.numeric(factor(ite$race))
  ite$ethnicity = as.numeric(factor(ite$ethnicity))
  ite$htn_med_class = as.numeric(factor(ite$htn_med_class))
  
  X = ite %>% select(-c(control_6months, control_12months, SBP_diff_12months, SBP_diff_6months, person_id, ite, htn_med_class))
  X_headers = as.data.frame(names(X))
  X = as.matrix(X)
  Y = ite$ite
  W = ite$htn_med_class
  
  ## Causal Forest Algorithm
  set.seed(618)
  
  cf_model = causal_forest(X = X, 
                           Y = Y, 
                           W = W, 
                           num.trees = 5000,
                           tune.parameters = 'all'
  )
  
  var_imp = variable_importance(cf_model)
  var_imp = as.data.frame(t(var_imp))
  names(var_imp) = X_headers$`names(X)`
  
  # calculate and visualize feature importance
  var_imp = var_imp %>% gather(key = 'variable', value = 'imp_score')
  var_imp = var_imp %>% arrange(desc('imp_score'))
  fi_plot = ggplot(var_imp, aes(reorder(x = variable, imp_score), y = imp_score)) + geom_bar(stat= 'identity') + coord_flip() + theme_bw() + ylab('variable importance') +
    xlab('variable') + ggtitle(paste0('CF Variable Importance - ', htn_med_class_i))
  ggsave(file.path(path, paste0('results/step2_CF_analysis/fi_plot_', outcome, '_', htn_med_class_i, '.png')), fi_plot) # save feature importance plot 
  print(fi_plot)
  
  ## Calculate Split Values 
  
  split_vars = cf_model$`_split_vars`
  split_values = cf_model$`_split_values`
  
  split_list = c()
  split_values_list = c()
  for (i in 1:1000)
  {
    split_list = c(split_list, unlist(split_vars[i]))
    split_values_list = c(split_values_list, unlist(split_values[i]))
  }
  
  split_values_df = data.frame(split_vars = split_list, 
                               split_values = split_values_list)
  X_headers$split_vars = 1:nrow(X_headers) - 1
  split_values_df = left_join(split_values_df, X_headers)
  table(split_values_df$`names(X)`)
  names(split_values_df) = c('split_vars_i', 'split_value', 'split_var_name')
  split_vars_list = as.data.frame(split_values_df %>% distinct(split_var_name))
  
  dir.create(file.path(path, paste0('results/step2_CF_analysis/',  outcome, '_', htn_med_class_i)))
  
  for (i in 1:nrow(split_vars_list))
  {
    split_chunk = split_values_df %>% filter(split_var_name == split_vars_list[i,])
    split_plot = ggplot(split_chunk, aes(x=split_value)) + geom_histogram() + ggtitle(split_vars_list[i,]) + theme_bw()
    ggsave(file.path(path, paste0('results/step2_CF_analysis/',  outcome, '_', htn_med_class_i, '/split_values_', i, '.png')), split_plot)
  }
  
  split_values_df = split_values_df %>% group_by(split_var_name) %>% summarise_at(vars(split_value), list(mean, sd))
  write.csv(split_values_df, file.path(path, paste0('results/step2_CF_analysis/',  outcome, '_', htn_med_class_i, '/split_values.csv')), row.names = F)
  
  return()
}

## ---- Run CF Analysis

for (i in 1:length(htn_med_list))
{
  htn_med_class_i = htn_med_list[i]
  
  # CF analysis for at control
  CF_analysis('at_control', htn_med_class_i)
  
  # CF analysis for change in SBP
  CF_analysis('sbp_change', htn_med_class_i)
  
}






