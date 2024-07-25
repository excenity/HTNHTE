# create results folder
path = getwd()
dir.create(file.path(path, 'results/step1_ITE_estimation'), showWarnings = F)

#' Performs Q-estimation for counterfactual predictions
#'
#' @details
#' This function loads a json file with all the study cohorts
#' that were created using ATLAS and then generates them in the
#' user specified OMOP CDM database
#'
#' @param med_class_i The anti-hypertensive medication class of interest
#' @param df The analytic dataset
#' @param outcome The outcome of interest
#'
#' @return
#' The ITE predictions
#'
#' @export
## SuperLearner Modeling Function
step1_ite_SL = function(med_class_i, df, outcome)
{
  ## create learners for SL

  # elastnic net
  enet = SuperLearner::create.Learner("SL.glmnet", detailed_names = T,
                        tune = list(alpha = seq(0, 1, length.out = 5)))

  # xgboost
  xg.tune = list(ntrees = c(5, 10, 15),
                 max_depth = c(3, 5, 8),
                 eta = c(0.05, 0.01))
  xgboost.learners = SuperLearner::create.Learner("SL.xgboost", tune = xg.tune, detailed_names = TRUE, name_prefix = "xgb")

  ## create med class comparisons
  df_comp = df
  df_comp$htn_med_class[df_comp$htn_med_class != htn_med_list[med_class_i]] = 'other'
  df_comp$htn_med_class = factor(df_comp$htn_med_class, levels = c('other', htn_med_list[med_class_i]))
  person_id_df = df_comp$pid

  if (outcome == 'at_control_14090')
  {
    X = df_comp %>% select(-pid, -sbp_change, -bmi_neg, -bp_14090, -bp_13080)
    y = df_comp$bp_14090
    SL.family = 'binomial'
    SL.method = 'method.AUC'
  } else if (outcome == 'at_control_13080')
  {
    X = df_comp %>% select(-pid, -sbp_change, -bmi_neg, -bp_14090, -bp_13080)
    y = df_comp$bp_13080
    SL.family = 'binomial'
    SL.method = 'method.AUC'
  } else
  {
    X = df_comp %>% select(-pid, -sbp_change, -bmi_neg, -bp_14090, -bp_13080)
    y = df_comp$sbp_change
    SL.family = 'gaussian'
    SL.method = 'method.NNLS'
  }

  # Choose Candidate Learners and CV Params
  set.seed(618)

  SL.library.chosen = c("SL.mean", "SL.glm", "SL.glm.interaction", enet$names, xgboost.learners$names, "SL.ranger")
  cvControl.chosen = list(V = 3)

  # fit SL model
  fit.sl = SuperLearner::SuperLearner(Y = y,
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
  outcomes_pred_plot = ggplot2::ggplot(outcome_pred, aes(x = pred, group = assignment, fill = assignment)) + geom_histogram(alpha = .8, position = 'identity') + theme_bw() + xlab('Y_hat') + ylab('Frequency') +
    ggtitle(paste('Histogram of Y_hat:', outcome), subtitle = paste(htn_med_list[med_class_i]))
  ggplot2::ggsave(file.path(path, paste0('results/step1_ITE_estimation/outcomes_pred_plot_', htn_med_list[med_class_i], '_', outcome, '_', '.png')), outcomes_pred_plot, width = 6, height = 4)

  # reporting statistics
  ite_histogram = ggplot2::ggplot(ite, aes(x = ite)) + geom_histogram(fill = NA, color = 'black', alpha = .75) + xlab('ITE') + ylab('Frequency') + theme_bw() + geom_vline(xintercept = mean(ite$ite), color = 'red') +
    ggtitle(paste('Histogram of ITE:', outcome), subtitle = paste('Medication Class:', htn_med_list[med_class_i], '     Mean:', round(mean(ite$ite),4)))
  ggplot2::ggsave(file.path(path, paste0('results/step1_ITE_estimation/ite_histogram_', htn_med_list[med_class_i], '_', outcome, '_', '.png')), ite_histogram, width = 6, height = 4)
  print(ite_histogram)

  ite = cbind(person_id_df, ite)

  return(ite)
}



