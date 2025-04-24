library(dplyr)
library(tidyr)


Diagnostic <- function(df) {
  df %>%
    group_by(parameter, model, Survey, NrSites, ModelType, occupancy) %>%
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
    stable_summarise   = TRUE                # Whether to compute mean & sd after convergence
) {
  
  # 1) Pivot the Diagnostics to long format
  long_df <- Diagnostics %>%
    pivot_longer(
      cols = pivot_cols,  # columns to pivot
      names_to = "Metric",
      values_to = "Value"
    )
  
  # 2) Compute scaled metrics, relative changes, and a convergence flag
  #    We'll do it per (parameter, Survey, NrSites, Metric) group
  convergence_df <- long_df %>%
    filter(Metric %in% metrics) %>%
    group_by(parameter, Survey, NrSites, Metric, ModelType, occupancy) %>%
    mutate(
      Value_scaled = as.numeric(scale(Value)[, 1]),
      sd_scaled    = as.numeric(scale(mean_sd)[, 1]),
      Value_final_scaled = Value_scaled[model == final_model],
      sd_final_scaled    = sd_scaled[model == final_model],
      rel_change_sd      = abs(sd_scaled - sd_final_scaled) / abs(sd_final_scaled),
      rel_change         = abs(Value_scaled - Value_final_scaled) / 
        abs(Value_final_scaled + 1e-8),  # avoid /0
      # Converge if rel_change < 0.1 or abs(Value) < 0.05 AND rel_change_sd < 0.1
      Converged_FromFinal = ((rel_change < rel_change_thresh  &
                                rel_change_sd < rel_change_sd_thresh) ) 
    ) %>%
    ungroup()
  
  # 3) Earliest convergent model for each (parameter, NrSites, Metric)
  first_convergence <- convergence_df %>%
    group_by(parameter, Survey, NrSites, Metric, ModelType, occupancy) %>%
    filter(Converged_FromFinal == TRUE) %>%
    summarise(
      first_model_converged = min(model),
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
    mutate(
      first_model_converged_all = pmax(first_converged_Bias, first_converged_RMSE, na.rm = TRUE)
    ) %>%
    # if either is NA, we consider "all" to be NA -> meaning never converged for both
    mutate(
      first_model_converged_all = if_else(
        is.na(first_converged_Bias) | is.na(first_converged_RMSE),
        NA_integer_,
        first_model_converged_all
      )
    )
  
  # 6) Join these "all converged" indices back to the main dataset
  post_convergence_df <- convergence_df %>%
    left_join(first_convergence_all, by = c("parameter", "NrSites", "Survey", "ModelType", "occupancy"))
  
  # 7) (Optional) Summarize stable region (model >= first_converged_all)
  if (stable_summarise) {
    end_dif <- post_convergence_df %>%
      filter(Metric == "Bias",
             model == final_model) %>% 
      group_by(parameter) %>% 
      summarise(
        # Summaries of "stable" region
        Bias = Value,
        meanSD = mean_sd,
        CI_97.5 = mean_upper,
        CI_2.5 = mean_lower
        # Maybe coverage or other metrics you'd like
      )
  } else {
    end_dif <- NULL
  }
  
  # 8) Return a list with the final data + summary
  return(list(
    post_convergence_df = post_convergence_df,
    end_dif             = end_dif
  ))
}