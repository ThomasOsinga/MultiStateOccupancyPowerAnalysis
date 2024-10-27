process_files <- function(file_list, state_type) {
  summary_results_list <- lapply(file_list, readRDS)
  names(summary_results_list) <- basename(file_list)
  
  all_model_data <- list()
  
  for (file_index in 1:length(summary_results_list)) {
    model_simulations <- clean_list(summary_results_list[[file_index]])
    
    for (survey_index in 1:1) {
      for (model_index in 1:length(model_simulations[[1]][[survey_index]])) {
        for (simulation_index in 1:length(model_simulations[[1]][[survey_index]][[model_index]])) {
          summary_data <- as.data.frame(model_simulations[[1]][[survey_index]][[model_index]][[simulation_index]][["summary"]])
          
          detection_prob <- c("n35", "n70", "n140", "n280")[survey_index]
          
          summary_data_enhanced <- summary_data %>%
            mutate(model = model_index,
                   Survey = detection_prob,
                   ModelType = state_type)  # Add ModelType to distinguish multistate vs singlestate
          
          list_name <- paste("Model", model_index, "Survey", detection_prob, "Sim", sep = "_")
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
