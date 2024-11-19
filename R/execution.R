#' Executes the study analysis given database details
#'
#' @details
#' Specify the database connection and schema details and then the study will execute
#'
#' @param connectionDetails connection details to the OMOP CDM database
#' @param cdmDatabaseSchema The schema with the OMOP CDM database
#' @param cohortDatabaseSchema A schema with read/write access that the cohort table will be written to
#' @param cohortTable The name of the cohort table
#' @param outputpath The location to save results to
#' @param generateCohorts Whether to run SQL that creates the cohorts
#' @param extractingData Whether to extract the study data using the cohorts
#' @param runStatisticsAnalysis Whether to run the statistical analysis
#' @param runTreatmentEffects Whether to run the treatment effects
#'
#' @return
#' The cohorts are generated into the specified cohort schema and table
#'
#' @export
#'
executeStudy <- function(
    connectionDetails,
    cdmDatabaseSchema,
    cohortDatabaseSchema,
    cohortTable = 'htn_hte',
    outputpath,
    generateCohorts = T,
    extractingData = T,
    runStatisticsAnalysis = T,
    runTreatmentEffects = F,
    htn_med_list = c('acei', 'arb', 'ccb', 'diuretic', 'ccb_combo', 'diuretic_combo'),
    cont_var = c('sbp', 'ldl', 'age', 'bmi'),
    cutpoints = list(c(0, 150, 300),
                     c(0 ,100, 300),
                     c(0, 65, 150),
                     c(0, 30, 100))
){

  if(generateCohorts){
    message('Generating cohorts for study')
    generateCohorts(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable
    )
  }


  #### ANALYTIC DATASET GENERATION ####
  if(extractingData){
    message('Extracting data')
    message('Part 1: downloading')
    df = HTNHTE::getData(
      cdmDatabaseSchema = cdmDatabaseSchema,
      connectionDetails = connectionDetails,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = 'htn_hte'
    )
    message('Part 2: processing')
    df = HTNHTE::generateAnalyticDataset(
      data = df,
      outputpath = outputpath
    )

    if(!dir.exists(outputpath)){
      dir.create(outputpath, recursive = T)
    }
    saveRDS(df, file.path(outputpath, 'data.rds'))
  }


  #### STATISTICAL ANALYSIS ####
  if(runStatisticsAnalysis){
    if(!file.exists(file.path(outputpath, 'data.rds'))){
      message('No data - please run with extractingData = T first')
    } else{
      df <- readRDS(file.path(outputpath, 'data.rds'))
      ## PART 1: Explore patient characteristics that contribute to HTE
      # STEP 1: Individual Treatment Effects (ITE) Estimation via G-estimation
      # STEP 2: Causal Forest to Identify Factors Contributing Most to Heterogeneity
      # get model for each unique(df$htn_med_class)
      htnMedClasses <- unique(df$htn_med_class)
      for(htnMedClass in htnMedClasses){
        for(type in c('at_control_14090', 'at_control_13080', 'sbp_change')){
          message(paste0('running stat analysis part 1 for ', type))
          if(!dir.exists(file.path(outputpath,htnMedClass, type))){
            dir.create(file.path(outputpath,htnMedClass,type), recursive = T)
          }
          ite <- step1_ite_SL(htnMedClass, df, type)
          write.csv(
            x = ite$ite,
            file = file.path(outputpath,htnMedClass, type,'ite.csv')
          )
          ggplot2::ggsave(
            filename = file.path(outputpath,htnMedClass, type,'outcomesPredPlot.png'),
            plot = ite$outcomesPredPlot,
            width = 6,
            height = 4
          )
          ggplot2::ggsave(
            filename = file.path(outputpath,htnMedClass, type,'iteHistogram.png'),
            plot = ite$iteHistogram,
            width = 6,
            height = 4
          )
          message(paste0('running stat analysis part 2 for ', type))
          cf <- CF_analysis(ite$ite, htnMedClass, df)
          write.csv(
            x = cf$splitValuesDf,
            file = file.path(outputpath,htnMedClass, type,'splitValuesDf.csv')
          )
          # add split plot as multiple pdf?
          ggplot2::ggsave(
            filename = file.path(outputpath,htnMedClass, type,'fiPlot.png'),
            plot = cf$fiPlot,
            width = 6,
            height = 4
          )
        }
      }
    }
  }

  # final step
  if(runTreatmentEffects){

    if(!file.exists(file.path(outputpath, 'data.rds'))){
      message('No data - please run with extractingData = T first')
    } else {
      df <- readRDS(file.path(outputpath, 'data.rds'))

    # create patient profiles
    df = createPatientProfiles(cont_var, cutpoints, df)
    patient_profile_list = df %>% dplyr::distinct(patient_profiles)

    # TMLE Analysis for Main Outcomes
    for(outcome in c('at_control_14090', 'at_control_13080','sbp_change')){
      result <- TMLE_analysis(
        df = df,
        outcome = outcome,
        patient_profile_list = patient_profile_list,
        htn_med_list = htn_med_list
      )
      if(!dir.exists(file.path(outputpath,'tmle_results_df'))){
        dir.create(file.path(outputpath,'tmle_results_df'), recursive = T)
      }
      if(!dir.exists(file.path(outputpath,'tmle_plots'))){
        dir.create(file.path(outputpath,'tmle_plots'), recursive = T)
      }
      write.csv(
        x = result$tmleResultsDf,
        file = file.path(outputpath,'tmle_results_df', paste0(outcome, '.csv')), row.names = F)
      ggplot2::ggsave(
        filename = file.path(outputpath,'tmle_plots', paste0(outcome, '.png')),
        result$tmlePlot,
        height = 16,
        width = 30
      )
    }


    # TMLE Analysis for Negative Control Outcome
    result <- TMLE_analysis_neg(unique(df$htn_med_class))
    write.csv(
      x = result$tmleResultsDf,
      file = file.path(outputpath,'tmle_results_df', paste0(outcome, '_bmi_neg.csv')), row.names = F)
    ggplot2::ggsave(
      filename = file.path(outputpath,'tmle_plots', paste0(outcome, '_bmi_neg.png')),
      result$tmlePlot,
      height = 16,
      width = 30
    )
    }
  }

}
