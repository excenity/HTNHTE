#' Create patient profiles
#'
#' @details
#' Creates patient profiles in the analytic dataset
#'
#' @param cont_var continuous outcomes
#' @param cutpoints cutpoints for the continuous variables
#' @param df analytic dataset description
#'
#' @return
#' The ITE predictions
#'
#' @export
createPatientProfiles = function(cont_var, cutpoints, df)
{
  cont_var_names = paste0(cont_var, '_cat')

  for (i in 1:length(cont_var))
  {
    df$new_col = 0
    df$new_col = cut(get(cont_var[i], df), breaks = unlist(cutpoints[i]), include.lowest = T)
    names(df)[names(df) == 'new_col'] = cont_var_names[i]
  }

  df = df %>% tidyr::unite(
    col = 'patient_profiles',
    tidyr::all_of(c(cont_var_names)),
    remove = F
    )

  # get list of distinct patient profiles and corresponding variable categories
  patient_profile_list = df[,c('patient_profiles', cont_var_names)]
  patient_profile_list = patient_profile_list %>% dplyr::distinct()

  return(df)
}


### TMLE helper function
TMLE_patientProfile = function(df, outcome, intervention_levels)
{
  # TMLE
  set.seed(618)

  # prepare dataset
  X = df %>% dplyr::select(c("age", "gender", "race", "hispanic", "dm", "ckd", "hf", "sleep_apnea", "antidepressants", "hormonal_therapy", "statins", "ppi", "dbp", "sbp", "chol", "ldl", "creatinine", "hba1c"))

  if (outcome == 'at_control_14090')
  {
    tmle_Y = df$bp_14090
    tmle_family = 'binomial'
  } else if (outcome == 'at_control_13080')
  {
    tmle_Y = df$bp_13080
    tmle_family = 'binomial'
  } else if (outcome == 'sbp_change')
  {
    tmle_Y = df$sbp_change
    tmle_family = 'gaussian'
  } else if (outcome == 'bmi_neg')
  {
    tmle_Y = df$bmi_neg
    tmle_family = 'gaussian'
  }

  # remove variables that are all same value
  uniqueCount <- unlist(lapply(1:ncol(X), function(i) length(unique(X[,i]))))
  if(length(uniqueCount == 1) > 0){
    message('Removing columns: ', paste(colnames(X)[uniqueCount == 1], collapse = ' - '), ' as only have single value.')
    X <- X[, uniqueCount > 1]
  }

  # TMLE
  # there is a bug in tmle where IC.ATC is not defined
  #env <- environment(fun = tmle::tmle)
  #env$IC.ATC <- NULL
  tmle_fit = tmle::tmle(Y = tmle_Y, #control_6months/SBP_diff_6months
                  A = intervention_levels,
                  W = X,
                  Q.SL.library = SL.library.chosen,
                  g.SL.library = SL.library.chosen,
                  family = tmle_family,
                  gbound = 0.05
  )


  return(tmle_fit)
}


#' Performs TMLE over all patient profiles
#'
#' @details
#'
#' Performs TMLE for all patient profiles for the main outcomes
#'
#' @param outcome The outcome of interest
#' @param patient_profile_list list of patient profiles created description
#' @param htn_med_list The different treatments
#'
#' @return
#' The ITE predictions
#'
#' @export
#'
#
TMLE_analysis = function(
    outcome,
    patient_profile_list,
    htn_med_list
    )
{
  ### Create SL Learners
  #print(SL.library.chosen)

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
      df_pp = df_med %>%
        dplyr::filter(patient_profiles == patient_profile_list$patient_profiles[patient_profile_i]) %>%
        dplyr::select(-"patient_profiles")
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

      if (outcome == 'at_control_14090')
      {
        tmle_Y = df_pp$bp_14090
      } else if (outcome == 'at_control_13080')
      {
        tmle_Y = df_pp$bp_13080
      } else if (outcome == 'sbp_change')
      {
        tmle_Y = df_pp$sbp_change
      }

      EIF_df = data.frame(
        Y = tmle_Y,
        q_init = q_init,
        g_score = g_score,
        A = intervention,
        q_star_avg = q_star_avg
      )
      names(EIF_df)[1] = 'Y'

      EIF_df$EIF = (EIF_df$Y - EIF_df$q_init) * EIF_df$A / EIF_df$g_score + EIF_df$q_init - EIF_df$q_star_avg
      EIF_std = sqrt(var(EIF_df$EIF)/nrow(EIF_df))

      # combine TMLE results
      if (is.null(tmle_fit$estimates$ATE$psi))
      {
        tmle_fit$estimates$ATE$psi = 0
      }

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


  ## ---- Visualize TMLE Results

  # Overview
  TMLE_plot = ggplot2::ggplot(tmle_results_df, ggplot2::aes(x = htn_med_class, y = Q1_star_avg, color = htn_med_class)) +
    ggplot2::geom_point() +
    ggplot2::geom_linerange(ggplot2::aes(ymin=Q1_star_lb, ymax=Q1_star_ub)) +
    ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggplot2::facet_wrap('patient_profiles') +
    ggplot2::theme_bw() +
    ggplot2::xlab('Hypertension Medication Class') +
    ggplot2::ylab('Predicted Treatment Effect')

  return(
    list(
      tmleResultsDf = tmle_results_df,
      tmlePlot = TMLE_plot
    )
  )
}


#' Performs TMLE for the negative control outcome in the overall population
#'
#' @details
#' Performs TMLE for the negative control outcome in the overall population
#'
#' @param htn_med_list treatments
#'
#' @return
#' The ITE predictions
#'
#' @export
#'
TMLE_analysis_neg = function(htn_med_list)
{
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
      Y = df_med$bmi_neg,
      q_init = q_init,
      g_score = g_score,
      A = intervention,
      q_star_avg = q_star_avg
    )
    names(EIF_df)[1] = 'Y'

    EIF_df$EIF = (EIF_df$Y - EIF_df$q_init) * EIF_df$A / EIF_df$g_score + EIF_df$q_init - EIF_df$q_star_avg
    EIF_std = sqrt(var(EIF_df$EIF)/nrow(EIF_df))

    # combine TMLE results for negative control analysis
    if (is.null(tmle_fit$estimates$ATE$psi))
    {
      tmle_fit$estimates$ATE$psi = 0
    }

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

  #write.csv(tmle_results_df, file.path(path, paste0('results/step3_TMLE_analysis/tmle_results_df_bmi_neg.csv')), row.names = F)

  # Overview
  TMLE_plot = ggplot2::ggplot(tmle_results_df, ggplot2::aes(x = htn_med_class, y = Q1_star_avg, color = htn_med_class)) +
    ggplot2::geom_point() +
    ggplot2::geom_linerange(ggplot2::aes(ymin=Q1_star_lb, ymax=Q1_star_ub)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggplot2::theme_bw() +
    ggplot2::xlab('Hypertension Medication Class') +
    ggplot2::ylab('Predicted Treatment Effect')
  #ggplot2::ggsave(file.path(path, paste0('results/step3_TMLE_analysis/TMLE_plot_bmiNeg.png')), TMLE_plot, height = 8, width = 12)

  return(list(
    tmleResultsDf = tmle_results_df,
    tmlePlot = TMLE_plot
  ))
}



