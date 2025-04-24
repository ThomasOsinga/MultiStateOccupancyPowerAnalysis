library(dplyr)
library(tidyr)


Diagnostic <- function(df) {
  df %>%
    group_by(parameter, model, Survey, NrSites, ModelType, covariates) %>%
    filter(!is.na(truth)) %>%
    summarise(
      RMSE         = sqrt(mean((mean - truth)^2)),
      Bias         = mean(mean - truth),
      mean_mean    = mean(mean),
      mean_sd      = mean(sd),
      mean_lower   = mean(lower),
      mean_upper   = mean(upper),
      Coverage     = mean(lower <= truth & upper >= truth),
      cov          = (mean_sd / mean_mean) * 100,  # Coefficient of Variation in %
      truth        = mean(truth),
      .groups      = "drop"
    )
}




analyze_convergence <- function(
    Diagnostics,
    metrics            = c("Bias", "RMSE"),  # Which metrics to consider
    pivot_cols         = c("RMSE", "Bias", "Coverage", "cov"),
    rel_change_thresh  = 0.1,                # Threshold for rel_change
    abs_value_thresh   = 0.05,               # Alternative threshold for Value
    rel_change_sd_thresh = 0.1,              # Threshold for rel_change_sd
    final_model        = 10,                 # Which model index is considered 'final'
    stable_summarise   = TRUE,               # Whether to compute mean & sd after convergence
    abs_sd_thresh      =  0.1
) {
  
  # 1) Pivot the Diagnostics to long format
  long_df <- Diagnostics %>%
    pivot_longer(
      cols = all_of(pivot_cols),  # columns to pivot
      names_to = "Metric",
      values_to = "Value"
    )
  
  # 2) Compute scaled metrics, relative changes, and a convergence flag
  #    We'll do it per (parameter, Survey, NrSites, Metric) group
  convergence_df <- long_df %>%
    filter(Metric %in% metrics) %>%
    group_by(parameter, Survey, NrSites, Metric, ModelType, covariates) %>%
    mutate(
      Value_final = Value[model == final_model],
      sd_final    = mean_sd[model == final_model],
      rel_change_sd      = abs(mean_sd - sd_final) / sd_final,
      rel_change         = abs(abs(Value) - abs(Value_final)) / 
        abs(Value_final + 1e-8),  
      # Converge if rel_change < 0.1 or abs(Value) < 0.05 AND rel_change_sd < 0.1
      Converged_FromFinal = ifelse(abs(Value) < abs_value_thresh, (rel_change_sd < rel_change_sd_thresh | (mean_sd < abs_sd_thresh)), 
                                   (abs(rel_change) < rel_change_thresh | (abs(rel_change_sd) < rel_change_sd_thresh)))) %>% 
    ungroup()

  #IF value < abs_value_thresh(1) THEN rel_change_sd < rel_change_sd_thresh OR mean_sd < abs_sd_thresh 
  # If we are talking about a probability - CHECK if rel_change_SD ir smaller than the threshold Or that mean_sd is smaller than abs_sd_thresh
  
  
  # 3) Earliest convergent model for each (parameter, NrSites, Metric)
  first_convergence <- convergence_df %>%
    group_by(parameter, Survey, NrSites, Metric, ModelType, covariates) %>%
    summarise(
      first_model_converged = {
        converged_models <- unique(model[Converged_FromFinal])
        
        if (1 %in% converged_models) {
          if (2 %in% converged_models) {
            1
          } else {
            min(converged_models[converged_models > 1], na.rm = TRUE)
          }
        } else {
          min(converged_models, na.rm = TRUE)
        }
      },
      .groups = "drop"
    )
  
  
  # 4) Pivot wider so we get columns for each metric's first convergence
  #    e.g., first_converged_Bias, first_converged_RMSE
  first_convergence_wide <- first_convergence %>%
    pivot_wider(
      names_from  = Metric,
      values_from = first_model_converged,
      names_prefix = "first_converged_"
    )
  
  # 5) Combine (Bias, RMSE) into one "all converged"
  #    We'll use pmax() so we only consider it converged once it's past both thresholds
  first_convergence_all <- first_convergence_wide %>%
    group_by(parameter, Survey, NrSites, ModelType, covariates) %>%
    mutate(
      first_model_converged_all = max(first_converged_Bias, na.rm = TRUE)
  )%>%
     #if either is NA, we consider "all" to be NA -> meaning never converged for both
    mutate(
      first_model_converged_all = if_else(
        is.na(first_converged_Bias),
        NA_integer_,
        first_model_converged_all
      )
    )
  
  # 6) Join these "all converged" indices back to the main dataset
  post_convergence_df <- convergence_df %>%
    left_join(first_convergence_all, by = c("parameter", "NrSites", "Survey", "ModelType", "covariates"))
  
  # 7) (Optional) Summarize stable region (model >= first_converged_all)
  if (stable_summarise) {
    end_dif <- post_convergence_df %>%
      filter(Metric == "Bias",
             model == final_model) %>% 
        # Summaries of "stable" region
      group_by(parameter, NrSites, Survey, ModelType, covariates) %>% 
      mutate(
        Bias = Value,
        meanSD = mean_sd,
        CI_97.5 = mean_upper,
        CI_2.5 = mean_lower) %>% 
    ungroup()
        # Maybe coverage or other metrics you'd like
  } else {
    end_dif <- NULL
  }
  
  # 8) Return a list with the final data + summary
  return(list(
    post_convergence_df = post_convergence_df,
    end_dif             = end_dif
  ))
}