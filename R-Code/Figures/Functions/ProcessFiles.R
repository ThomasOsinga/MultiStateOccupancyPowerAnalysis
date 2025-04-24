process_files <- function(file_list, state_type) {
  summary_results_list <- lapply(file_list, readRDS)
  names(summary_results_list) <- basename(file_list)
  
  all_model_data <- list()
  model_identifier <- 0
  survey_index <- 1
  for (file_index in 1:length(summary_results_list)) {
    model_simulations <- clean_list(summary_results_list[[file_index]])
    for (survey_index in 1:length(model_simulations[[1]])) {
      for (model_index in 1:length(model_simulations[[1]][[1]])) {
        for (simulation_index in 1:length(model_simulations[[1]][[1]][[model_index]])) {
          summary_data <- as.data.frame(model_simulations[[1]][[survey_index]][[model_index]][[simulation_index]][["summary"]])
          Survey <- model_simulations[[2]][[survey_index]][[model_index]][[simulation_index]]$nsurveys
          Sites <- model_simulations[[2]][[survey_index]][[model_index]][[simulation_index]]$nsites
          param_names <- summary_data$parameter
          modeltype <- ifelse(any(grepl("p22", param_names)), "Multi-state", "Basic")
          covariates <- any(grepl("^beta1(\\[|$)", param_names))
       
          model_identifier <- model_identifier + 1
          summary_data_enhanced <- summary_data %>%
            mutate(model = model_index,
                   Survey = Survey,
                   NrSites = Sites,
                   covariates = covariates,
                   ModelType = modeltype,
                   sim = simulation_index,
                   model_identifier = model_identifier)  
          
          list_name <- paste("Model", model_index, "NrSurvey", Survey, "NrSites", Sites, "Sim", sep = "_")
          if (!exists(list_name, where = all_model_data)) {
            all_model_data[[list_name]] <- summary_data_enhanced
          } else {
            all_model_data[[list_name]] <- rbind(all_model_data[[list_name]], summary_data_enhanced)
          }
        }
      }
    }
  }
  return(bind_rows(all_model_data))
}

