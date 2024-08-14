#' Performs causal forest to find factors contributing to effect heterogeneity
#'
#' @details
#' This function loads a json file with all the study cohorts
#' that were created using ATLAS and then generates them in the
#' user specified OMOP CDM database
#'
#' @param htn_med_class_i The anti-hypertensive medication class of interest
#' @param df The analytic dataset
#'
#' @return
#' Generates the feature importance plots for each medication class and outcome, also generates split values used by the CF algorithm
#'
#' @export

CF_analysis = function(ite, htn_med_class_i, df)
{

  ## Dataset Setup

  df$htn_med_class[df$htn_med_class != htn_med_class_i] = 'other'
  df$htn_med_class = factor(df$htn_med_class, levels = c('other', htn_med_class_i))

  # join dataset
  ite = ite %>%
    dplyr::rename(pid = "person_id_df") %>%
    dplyr::inner_join(df)

  # format all categorical variables
  ite$race = as.numeric(factor(ite$race))
  ite[] = lapply(ite, as.numeric)

  X = ite %>% dplyr::select(-"pid", -"sbp_change", -"bmi_neg", -"bp_14090", -"bp_13080", -"ite", -"htn_med_class")
  X_headers = as.data.frame(names(X))
  X = as.matrix(X)
  Y = ite$ite
  W = ite$htn_med_class

  ## Causal Forest Algorithm
  set.seed(618)

  cf_model = grf::causal_forest(X = X,
                           Y = Y,
                           W = W,
                           num.trees = 1000,
                           tune.parameters = 'all'
  )

  var_imp = grf::variable_importance(cf_model)
  var_imp = as.data.frame(t(var_imp))
  names(var_imp) = X_headers$`names(X)`

  # calculate and visualize feature importance
  var_imp = var_imp %>% tidyr::gather(key = 'variable', value = 'imp_score')
  var_imp = var_imp %>% dplyr::arrange(desc('imp_score'))
  fi_plot = ggplot2::ggplot(var_imp, ggplot2::aes(reorder(x = variable, imp_score), y = .data$imp_score)) +
    ggplot2::geom_bar(stat= 'identity') +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::ylab('variable importance') +
    ggplot2::xlab('variable') +
    ggplot2::ggtitle(paste0('CF Variable Importance - ', htn_med_class_i))
  #ggplot2::ggsave(file.path(path, paste0('results/step2_CF_analysis/fi_plot_', outcome, '_', htn_med_class_i, '.png')), fi_plot, width = 6, height = 8) # save feature importance plot
  #print(fi_plot)

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
  split_values_df = dplyr::left_join(split_values_df, X_headers)
  table(split_values_df$`names(X)`)
  names(split_values_df) = c('split_vars_i', 'split_value', 'split_var_name')
  split_vars_list = as.data.frame(split_values_df %>% distinct(split_var_name))

  #dir.create(file.path(path, paste0('results/step2_CF_analysis/',  outcome, '_', htn_med_class_i)))

  split_plot <- list()
  for (i in 1:nrow(split_vars_list))
  {
    split_chunk = split_values_df %>% filter(split_var_name == split_vars_list[i,])
    split_plot[[length(split_plot) + 1]] = ggplot2::ggplot(split_chunk, ggplot2::aes(x=split_value)) +
      ggplot2::geom_histogram() +
      ggplot2::ggtitle(split_vars_list[i,]) +
      ggplot2::theme_bw()
    #ggplot2::ggsave(file.path(path, paste0('results/step2_CF_analysis/',  outcome, '_', htn_med_class_i, '/split_values_', i, '.png')), split_plot, width = 8, height = 6)
  }

  split_values_df = split_values_df %>% group_by(split_var_name) %>% summarise_at(vars(split_value), list(mean, sd))
  #write.csv(split_values_df, file.path(path, paste0('results/step2_CF_analysis/',  outcome, '_', htn_med_class_i, '/split_values.csv')), row.names = F)

  return(
    list(
      splitValuesDf = split_values_df,
      splitPlot = split_plot,
      fiPlot = fi_plot
    )
  )
}
