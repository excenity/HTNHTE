# STEP 3: Targeted Maximum Likelihood Estimation (TMLE) of Medication Treatment Effects within Different Patient Populations
# Version: 04/15/24

if (!require(tmle))
{
  install.packages("tmle")
}

library(data.table)
library(tidyverse)
library(tmle)

dir.create(file.path(path, 'results/step3_TMLE_analysis'))

## ---- Create Patient Profiles Comparisons

if (output_analytical_dataset == T)
{
  final_df = fread(file.path(path, 'final_df.csv'), row.names = F)
  bmi_neg = fread(file.path(path, 'bmi_neg.csv'), row.names = F)
}

df = final_df
final_df = left_join(final_df, bmi_neg)

cont_var_names = paste0(cont_var, '_cat')

for (i in 1:length(cont_var))
{
  df$new_col = 0
  df$new_col = cut(get(cont_var[i], df), breaks = unlist(cutpoints[i]), include.lowest = T)
  names(df)[names(df) == 'new_col'] = cont_var_names[i]
}

df = df %>% unite(col = 'patient_profiles', all_of(c(cont_var_names)), remove = F)

# get list of distinct patient profiles and corresponding variable categories
patient_profile_list = df[,c('patient_profiles', cont_var_names)]
patient_profile_list = patient_profile_list %>% distinct()


## ---- TMLE Setup

### Create SL Learners

# random forest
RF.learners = create.Learner("SL.ranger", tune = list(mtry = 3, num.trees = 500))
# xgboost
tune = list(ntrees = c(5, 10, 15),
            max_depth = 2:5, 
            eta = c(0.1, 0.05, 0.01))
xgboost.learners = create.Learner("SL.xgboost", tune = tune, detailed_names = TRUE, name_prefix = "xgb")
# elastic net
enet = create.Learner("SL.glmnet", detailed_names = T, tune = list(alpha = seq(0, 1, length.out = 5)))
# list libraries 
SL.library.chosen = c("SL.mean", "SL.glm", "SL.glm.interaction", enet$names, xgboost.learners$names, "SL.ranger")


### TMLE helper function 
TMLE_patientProfile = function(df, outcome, intervention_levels)
{
  # TMLE
  set.seed(618)
  
  # prepare dataset 
  X = df %>% select(c("age", "gender", "race", "ethnicity", "T2DM", "CKD", "HF", "Sleep_Apnea", "antidepressants", "hormonal_therapy", "statins", "PPI", "DBP", "SBP", "total_cholesterol", "creatinine", "hba1c")) 
  
  if (outcome == 'at_control')
  {
    tmle_Y = df$control_6months
    tmle_family = 'binomial'
    } else if (outcome == 'SBP_change')
    { 
      tmle_Y = df$SBP_diff_6months
      tmle_family = 'gaussian'
    } else if (outcome == 'bmi_neg')
    {
      tmle_Y = df$BMI_negControl
      tmle_family = 'gaussian'
    }
  
  
  # TMLE
  tmle_fit = tmle(Y = tmle_Y, #control_6months/SBP_diff_6months
                  A = intervention_levels,
                  W = X,
                  Q.SL.library = SL.library.chosen,
                  g.SL.library = SL.library.chosen,
                  family = tmle_family, 
                  gbound = 0.05
  )
  return(tmle_fit)
}


## ---- TMLE Analysis Iterative over Patient Profiles

TMLE_analysis = function(outcome)
{
  for (med_class_i in 1:length(htn_med_list))
  {
    # create medication class specific dataset
    df_med = df 
    df_med$htn_med_class[df_med$htn_med_class != htn_med_list[med_class_i]] = 'other'
    df_med$htn_med_class = factor(df_med$htn_med_class, levels = c('other', htn_med_list[med_class_i]))
    print(paste('med_class:', htn_med_list[med_class_i]))
    
    for (patient_profile_i in 1:nrow(patient_profile_list))
    {
      # select patient profile
      df_pp = df_med %>% filter(patient_profiles == patient_profile_list$patient_profiles[patient_profile_i]) %>% select(-patient_profiles) 
      print(paste('patient_profile_index:', patient_profile_i))
      
      intervention = htn_med_list[med_class_i]
      intervention = as.numeric(as.factor(df_pp$htn_med_class), levels = c('others', intervention))-1
      print(table(intervention))
      tmle_fit = TMLE_patientProfile(df = df_pp, outcome = outcome, intervention_levels = intervention)
      
      # calculate Efficient Influence Function and Variance
      q_init = tmle_fit$Qinit$Q[,2]
      g_score = tmle_fit$g$g1W
      q_star = tmle_fit$Qstar[,2]
      q_star_avg = mean(q_star) # average of targeted Q_0* 
      
      EIF_df = data.frame(
        Y = tmle_Y,
        q_init = q_init, 
        g_score = g_score, 
        A = intervention, 
        q_star_avg = q_star_avg
      )
      names(EIF_df)[1] = 'Y'
      
      EIF_df$EIF = (EIF_df$Y - EIF_df$q_init) * EIF_df$A / EIF_df$g_score + EIF_df$q_init - EIF_df$q_star_avg
      
      if (is.null(tmle_fit$estimates$ATE$psi))
      {
        tmle_fit$estimates$ATE$psi = 0
      }
      
      EIF_std = sqrt(var(EIF_df$EIF)/nrow(EIF_df))
      
      # combine TMLE results 
      tmle_results = data.frame(
        Q1_star_avg = q_star_avg, 
        EIF_std = EIF_std, 
        num_pp = nrow(df_pp))
      tmle_results$Q1_star_lb = tmle_results$Q1_star_avg - 1.96*tmle_results$EIF_std
      tmle_results$Q1_star_ub = tmle_results$Q1_star_avg + 1.96*tmle_results$EIF_std
      tmle_results$patient_profiles = patient_profile_list[[patient_profile_i,1]]
      
      if (patient_profile_i== 1)
      {tmle_results_chunk = tmle_results} else
      {tmle_results_chunk = rbind(tmle_results_chunk, tmle_results)}
    }
    
    tmle_results_chunk$htn_med_class = htn_med_list[med_class_i]
    if (med_class_i == 1) 
    {tmle_results_df = tmle_results_chunk} else 
    {tmle_results_df = rbind(tmle_results_df, tmle_results_chunk)}
  }  
  
  write.csv(tmle_results_df, file.path(path, paste0('results/step3_TMLE_analysis/tmle_results_df_', outcome, '.csv')), row.names = F)
  
  
  ## ---- Visualize TMLE Results
  
  # tmle_results_df = read.csv('/Users/excenity/Documents/HSIP/Research/Dissertation Project/Code/EDW OMOP/tmle_results_df.csv')
  
  tmle_results_df$htn_med_class[tmle_results_df$htn_med_class == 'acei_arb_diuretic'] = 'combo'
  
  # Overview
  TMLE_plot = ggplot(tmle_results_df, aes(x = htn_med_class, y = Q1_star_avg, color = htn_med_class)) + geom_point() + geom_linerange(aes(ymin=Q1_star_lb, ymax=Q1_star_ub)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_wrap('patient_profiles') + theme_bw() + xlab('Hypertension Medication Class') + ylab('Predicted Treatment Effect')
  ggsave(file.path(path, paste0('results/step3_TMLE_analysis/TMLE_plot_', outcome, '.png')), TMLE_plot, height = 16, width = 30)
}

# run TMLE analysis across all outcomes (at 6 months)
print('Running at_control Analysis')
TMLE_analysis(outcome = 'at_control')
print('Running SBP_change Analysis')
TMLE_analysis(outcome = 'SBP_change')


print('Running neg_control Analysis')
# run TMLE analysis of the negative control variable
for (med_class_i in 1:length(htn_med_list))
{
  # create medication class specific dataset
  df_med = df 
  df_med$htn_med_class[df_med$htn_med_class != htn_med_list[med_class_i]] = 'other'
  df_med$htn_med_class = factor(df_med$htn_med_class, levels = c('other', htn_med_list[med_class_i]))
  print(paste('med_class:', htn_med_list[med_class_i]))
  
  intervention = htn_med_list[med_class_i]
  intervention = as.numeric(as.factor(df_med$htn_med_class), levels = c('others', intervention))-1
  print(table(intervention))
  tmle_fit = TMLE_patientProfile(df = df_med, outcome = 'bmi_neg', intervention_levels = intervention)
  
  # calculate Efficient Influence Function and Variance
  q_init = tmle_fit$Qinit$Q[,2]
  g_score = tmle_fit$g$g1W
  q_star = tmle_fit$Qstar[,2]
  q_star_avg = mean(q_star) # average of targeted Q_0* 
  
  EIF_df = data.frame(
    Y = df_med$BMI_negControl,
    q_init = q_init, 
    g_score = g_score, 
    A = intervention, 
    q_star_avg = q_star_avg
  )
  names(EIF_df)[1] = 'Y'
  
  EIF_df$EIF = (EIF_df$Y - EIF_df$q_init) * EIF_df$A / EIF_df$g_score + EIF_df$q_init - EIF_df$q_star_avg
  
  if (is.null(tmle_fit$estimates$ATE$psi))
  {
    tmle_fit$estimates$ATE$psi = 0
  }
  
  EIF_std = sqrt(var(EIF_df$EIF)/nrow(EIF_df))
  
  # combine TMLE results 
  tmle_results = data.frame(
    Q1_star_avg = q_star_avg, 
    EIF_std = EIF_std, 
    num_pp = nrow(df_med))
  tmle_results$Q1_star_lb = tmle_results$Q1_star_avg - 1.96*tmle_results$EIF_std
  tmle_results$Q1_star_ub = tmle_results$Q1_star_avg + 1.96*tmle_results$EIF_std
  
  tmle_results$htn_med_class = htn_med_list[med_class_i]
  if (med_class_i == 1) 
  {tmle_results_df = tmle_results} else 
  {tmle_results_df = rbind(tmle_results_df, tmle_results)}
}
  
write.csv(tmle_results_df, file.path(path, paste0('results/step3_TMLE_analysis/tmle_results_df_bmi_neg.csv')), row.names = F)







