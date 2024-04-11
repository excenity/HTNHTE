# STEP 1: Individual Treatment Effect (ITE) Analysis
# Version: 04/10/24

if (!require(SuperLearner))
{
  install.packages("SuperLearner")
}
if (!require(pROC))
{
  install.packages("pROC")
}
if (!require(Metrics))
{
  install.packages("Metrics")
}

library(data.table)
library(tidyverse)
library(SuperLearner)
library(pROC)
library(Metrics)

# create results folder
dir.create(file.path(path, 'results/step1_ITE_estimation'))

## ---- Data Prep
#final_df = read.csv('/Users/excenity/Documents/HSIP/Research/Dissertation Project/Code/HTN HTE Testing/final_df.csv')
df = final_df

if (output_analytical_dataset == T)
{
  remove(final_df)  
}


# HTN med class list
htn_med_list = c('acei', 'arb', 'ccb', 'diuretics', 'acei_arb_diuretic')

# list of factors and format dataset
factor_list = c('gender', 'race', 'ethnicity','HF', 'T2DM', 'CKD', 'Sleep_Apnea',"antidepressants", "hormonal_therapy", "statins", "PPI")
df = df %>% mutate_at(factor_list, as.factor)
df$htn_med_class = as.character(df$htn_med_class)

# remove unneeded vars
df = df %>% select(-c(drug_exposure_start_date, cohort_start_date, DBP_12months, SBP_12months, DBP_6months, SBP_6months))

## ---- Estimation via Superlearner

## create learners for SL 

# elastnic net
enet = create.Learner("SL.glmnet", detailed_names = T,
                      tune = list(alpha = seq(0, 1, length.out = 5)))

# xgboost
xg.tune = list(ntrees = c(5, 10, 15),
               max_depth = c(3, 5, 8), 
               eta = c(0.05, 0.01))
xgboost.learners = create.Learner("SL.xgboost", tune = xg.tune, detailed_names = TRUE, name_prefix = "xgb")

## SuperLearner Modeling Function
step1_ite_SL = function(med_class_i, df, outcome, months = 6)
{
  # create med class comparisons 
  df_comp = df 
  df_comp$htn_med_class[df_comp$htn_med_class != htn_med_list[med_class_i]] = 'other'
  df_comp$htn_med_class = factor(df_comp$htn_med_class, levels = c('other', htn_med_list[med_class_i]))
  person_id_df = df_comp$person_id
  
  # finalize analytical dataset (X, y)
  if (months == 12)
  {
    if (outcome == 'at_control')
    {
      X = df_comp %>% select(-c(SBP_diff_12months, control_12months, control_6months, SBP_diff_6months)) 
      y = as.numeric(df_comp$control_12months)
      SL.family = 'binomial'
      SL.method = 'method.AUC'
    } else 
    {
      X = df_comp %>% select(-c(SBP_diff_12months, control_12months, control_6months, SBP_diff_6months)) 
      y = df_comp$SBP_diff_12months
      SL.family = 'gaussian'
      SL.method = 'method.NNLS'
    }
  } else if (months == 6)
  {
    if (outcome == 'at_control')
    {
      X = df_comp %>% select(-c(SBP_diff_12months, control_12months, control_6months, SBP_diff_6months)) 
      y = as.numeric(df_comp$control_6months)
      SL.family = 'binomial'
      SL.method = 'method.AUC'
    } else 
    {
      X = df_comp %>% select(-c(SBP_diff_12months, control_12months, control_6months, SBP_diff_6months)) 
      y = df_comp$SBP_diff_6months
      SL.family = 'gaussian'
      SL.method = 'method.NNLS'
    }
  }
  
  # Choose Candidate Learners and CV Params
  set.seed(618)
  
  SL.library.chosen = c("SL.mean", "SL.glm", "SL.glm.interaction", enet$names, xgboost.learners$names, "SL.ranger")
  cvControl.chosen = list(V = 3)
  
  # fit SL model
  fit.sl = SuperLearner(Y = y, 
                        X = X,
                        cvControl = cvControl.chosen,
                        SL.library = SL.library.chosen,
                        family = SL.family, 
                        method = SL.method,
                        verbose = T)
  
  # obtain predictions
  all.pred = predict(fit.sl)
  Yhat = data.frame(all.pred$pred)

  Yhat = Yhat$all.pred.pred
  
  ## Estimate ITE 
  
  # force intervention to control class 
  X_0 = X
  X_0$htn_med_class = factor('other', levels = c('other', htn_med_list[med_class_i]))
  
  pred_0 = predict(fit.sl, newdata = X_0)
  
  # force intervention to comparison class
  X_1 = X
  X_1$htn_med_class = factor(htn_med_list[med_class_i], levels = c('other', htn_med_list[med_class_i]))
  
  pred_1 = predict(fit.sl, newdata = X_1)
  
  # calculate ite
  ite = as.data.frame(pred_1$pred - pred_0$pred)
  names(ite) = 'ite'
  
  # plot predicted potential outcome 
  pred_0 = data.frame(pred = pred_0$pred)
  pred_0$assignment = 0
  pred_1 = data.frame(pred = pred_1$pred)
  pred_1$assignment = 1
  outcome_pred = rbind(pred_0, pred_1)
  
  outcome_pred$assignment = factor(outcome_pred$assignment)
  outcomes_pred_plot = ggplot(outcome_pred, aes(x = pred, group = assignment, fill = assignment)) + geom_histogram(alpha = .8, position = 'identity') + theme_bw() + xlab('Y_hat') + ylab('Frequency') +
    ggtitle(paste('Histogram of Y_hat:', outcome), subtitle = paste(htn_med_list[med_class_i]))
  ggsave(file.path(path, paste0('results/step1_ITE_estimation/outcomes_pred_plot_', htn_med_list[med_class_i], '_', outcome, '_', '.png')), outcomes_pred_plot)

  # reporting statistics
  ite_histogram = ggplot(ite, aes(x = ite)) + geom_histogram(fill = NA, color = 'black', alpha = .75) + xlab('ITE') + ylab('Frequency') + theme_bw() + geom_vline(xintercept = mean(ite$ite), color = 'red') +
    ggtitle(paste('Histogram of ITE:', outcome), subtitle = paste('Medication Class:', htn_med_list[med_class_i], '     Mean:', round(mean(ite$ite),4)))
  ggsave(file.path(path, paste0('results/step1_ITE_estimation/ite_histogram_', htn_med_list[med_class_i], '_', outcome, '_', '.png')), ite_histogram)
  print(ite_histogram)
  
  ite = cbind(person_id_df, ite)
  
  return(ite)
}


## Calculate ITE and Output Dataset

#for (i in 1:length(htn_med_list))
  for (i in 1:5)
{
  ite = step1_ite_SL(i, df, 'at_control')  
  write.csv(ite, file.path(path, paste0('results/step1_ITE_estimation/ite_at_control_', htn_med_list[i], '.csv')), row.names = F)
  ite = step1_ite_SL(i, df, 'SBP_change')  
  write.csv(ite, file.path(path, paste0('results/step1_ITE_estimation/SBP_change_', htn_med_list[i], '.csv')), row.names = F)
}
