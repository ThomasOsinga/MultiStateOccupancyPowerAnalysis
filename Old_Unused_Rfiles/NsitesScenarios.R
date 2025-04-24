## WorkingDirectory = MultiStateOccupancyModelsPowerAnalysis - RepositoryFolder

Basic_60 <- "PosteriorEstimates/NsiteScenarios/BasicModels/60Sites"
Basic_120 <- "PosteriorEstimates/NsiteScenarios/BasicModels/120Sites"
Basic_240 <- "PosteriorEstimates/NsiteScenarios/BasicModels/240Sites"
Multi_60 <- "PosteriorEstimates/NsiteScenarios/MultiStateModels/60Sites"
Multi_120 <- "PosteriorEstimates/NsiteScenarios/MultiStateModels/120Sites"
Multi_240 <- "PosteriorEstimates/NsiteScenarios/MultiStateModels/240Sites"
multistate_files  <- "Outputs/lowoccupancy"
source("R-Code/Figures/ProcessFiles.r")
source("R-Code/Figures/CleanList.r")
source("R-Code/Figures/Diagnostics.r")
library(cowplot)
library(tidyverse)
# Process singlestate files (60 and 120 sites)
Basic_files_60 <- list.files(path = Basic_60, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
Basic_files_120 <- list.files(path = Basic_120, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
Basic_files_240 <- list.files(path = Basic_240, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
# Process multistate files (60 and 120 sites)
multistate_files_60 <- list.files(path = Multi_60, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
multistate_files_120 <- list.files(path = Multi_120, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
multistate_files_240 <- list.files(path = Multi_240, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
multistate_files <- list.files(path = multistate_files, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
# Process files and add a column for number of sites and model type (multistate or singlestate)
Basic_data_60 <- process_files(Basic_files_60, "Basic") %>%
  mutate(NrSites = 60)

Basic_data_120 <- process_files(Basic_files_120, "Basic") %>%
  mutate(NrSites = 120)
Basic_data_240 <- process_files(Basic_files_240, "Basic") %>%
  mutate(NrSites = 240)

multistate_data_60 <- process_files(multistate_files_60, "Multi-state") %>%
  mutate(NrSites = 60)

multistate_data_120 <- process_files(multistate_files_120, "Multi-state") %>%
  mutate(NrSites = 120)

multistate_data_240 <- process_files(multistate_files_240 , "Multi-state") %>%
  mutate(NrSites = 240)
multistate_data <- process_files(multistate_files , "Multi-state") 

# Combine all the data into a single dataframe
combined_data <- bind_rows(Basic_data_60, Basic_data_120, 
                           Basic_data_240, multistate_data_60, 
                           multistate_data_120, multistate_data_240)
combined_data <- multistate_data
Diagnostics <- Diagnostic(combined_data)

Convergence <-analyze_convergence(Diagnostics = Diagnostics ,  
                                  metrics            = c("Bias", "RMSE"),  # Which metrics to consider
                                  pivot_cols         = c("RMSE", "Bias", "Coverage", "cov"),
                                  rel_change_thresh  = 0.1,                # Threshold for rel_change
                                  abs_value_thresh   = 0.05,               # Alternative threshold for Value
                                  rel_change_sd_thresh = 0.1,              # Threshold for rel_change_sd
                                  final_model        = 8,                 # Which model index is considered 'final'
                                  stable_summarise   = TRUE)

#Merge First model converged with combined dataset
ConvergedValue <- Convergence[[1]] %>% 
  select(c(parameter,NrSites,Survey, model, ModelType, first_model_converged_all))
filtered_data <- combined_data %>%
  left_join(ConvergedValue, by = c("parameter", "NrSites", "Survey", "model", 'ModelType')) %>% 
  filter(parameter %in% c("p","p1","p22", "p33","p21","p31","p32", "beta1","beta0[1,1]","beta0[2,1]",
                        "beta0[3,1]","beta1[1,1]","beta1[2,1]","beta1[3,1]","beta0", "beta0"))  
  #filter(parameter %in% target_parameters)

p_values <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9)
# Second plot for `p` and `p22`
plot_beta0 <- filtered_data %>%
  filter(parameter %in% c("beta0", "beta0[1,1]")) %>%  # Filter for `p` and `p22`
  mutate(ModelP = factor(paste("", p_values[as.numeric(model)]), levels = paste("", p_values))) %>%
  #mutate(parameter = factor(parameter, levels = parameter_levels)) %>% 
  mutate(Survey = factor(Survey)) %>%
  #mutate(xintercept = sapply(threshold_value, function(tv) which(p_values == tv))) %>% 
  #mutate(decimal_count = sapply(threshold_value, nchar),  # Count the number of characters in threshold value
  #x_offset = decimal_count * 0.38 + 0.5 ) %>%  # Offset text position based on number of decimals
  ggplot(aes(x = ModelP, y = mean)) +
  #geom_point(position=position_jitter(width=0.3))+
  geom_errorbar(aes(ymin = truth, ymax = truth), linetype = "dashed", color = "#ff0000", linewidth = 0.5) +
  geom_vline(aes(xintercept = first_model_converged_all), linetype = "dashed", color = "black", size=0.5) +
  #geom_text(aes(x = xintercept + x_offset, y = 10, label = paste("p =", threshold_value)), 
  #color = "black", size = 4, angle = 0, hjust = 1) +  
  facet_grid(NrSites~ parameter + ModelType, scales = "free_y") +  # Facet by Survey, Parameter, and ModelType
  geom_smooth(aes(x = as.numeric(ModelP)), color = "black", size = 0.5, se = FALSE) +  # Smooth line for mean
  geom_smooth(aes(x = as.numeric(ModelP), y = upper), color = "grey", linetype = "dashed", size = 1, se = FALSE) +  # Upper smooth line
  geom_smooth(aes(x = as.numeric(ModelP), y = lower), color = "grey", linetype = "dashed", size = 1, se = FALSE) +  # Lower smooth line
  #coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Detection Probability", y = "Posterior mean") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 9*1.5), 
        axis.text.y = element_text(size = 9*1.5), 
        axis.title = element_text(size = 10*1.5),
        strip.text.x = element_text(size = 7*1.5), 
        strip.text.y = element_text(size = 7*1.5), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  guides(color = "none", shape = "none")  # Remove the legend for color and shape

# Save the combined plot
ggsave(filename="Figure3_Maintext.png", plot = combined_plot, unit="cm", width = 25, height = 35, dpi=600, scale=1,
       path ="Outputs/NsitesSimulation/Graph/")

############# Appendix Plots
model_convergence <- filtered_data %>%
  group_by(model_identifier) %>%
  mutate(
    MCMCconverged = !any(
      parameter %in% c("p","p1","p22", "p33","p21","p31","p32", "beta1","beta0[1,1]","beta0[2,1]","beta0[3,1]","beta1[1,1]","beta1[2,1]","beta1[3,1]","beta0", "beta0")
      & rhat > 1.1 | n.eff < 200
    )
  ) %>%
  ungroup()


model_convergence %>%
  #filter(!parameter %in% c("deviance","beta1","beta0[1,1]","beta0[2,1]","beta0[3,1]","beta1[1,1]","beta1[2,1]","beta1[3,1]","beta0", "beta0", 
  #"n.occ[1]","n.occ[2]","n.occ[3]","n.occ[4]", "mean.Omega1","mean.Omega2","mean.Omega3","mean.Omega4", "mean.psi", 
  #"mean.R1", "mean.R2")) %>% 
  #filter(parameter %in% c("p","p1","p22", "p33","p21","p31","p32")) %>% 
  filter(parameter %in% c("beta1","beta0[1,1]","beta0[2,1]","beta0[3,1]","beta1[1,1]","beta1[2,1]","beta1[3,1]", "beta0")) %>% 
  mutate(ModelP = factor(paste("", p_values[as.numeric(model)]), levels = paste("", p_values))) %>%
  #mutate(parameter = factor(parameter, levels = parameter_levels_p)) %>% 
  mutate(Survey = factor(Survey)) %>%
  ggplot(aes(x = ModelP, y = mean)) +
  geom_point(aes(x= as.factor(ModelP), color = ifelse(MCMCconverged==FALSE, "orange", "#333333"), 
                 shape = factor(ifelse(MCMCconverged==FALSE, "High Rhat", "Normal Rhat"))), position=position_jitter(width=0.3), size=0.9) + 
  scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +
  scale_color_identity() +
  facet_grid(NrSites+ Survey ~ parameter + ModelType, scales = "free_y") +  # Facet by Survey, Parameter, and ModelType
  geom_smooth(
    data = subset(smooth_data, MCMCconverged == TRUE),
    aes(x = as.numeric(ModelP), y = mean),
    color = "blue",
    size = 1.5,
    se = FALSE
  )+
  geom_errorbar(aes(ymin = truth, ymax = truth), linetype = "dashed", color = "#ff0000", linewidth = 0.5) +
  #coord_cartesian(ylim = c(0, 1)) +
  geom_vline(aes(xintercept = first_model_converged_all), linetype = "dashed", color = "black", size=0.5) +
  #geom_hline(aes(yintercept=0), col='blue', linetype='dashed') +
  labs(x = "Detection Probability", y = "Posterior mean") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 9*1.5), 
        axis.text.y = element_text(size = 9*1.5), 
        axis.title = element_text(size = 10*1.5),
        strip.text.x = element_text(size = 7*1.5), 
        strip.text.y = element_text(size = 7*1.5), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  guides(color = "none", shape = "none")  # Remove the legend for color and shape
# Save the combined plot
ggsave(filename="AppendixBetaParameters.png", unit="cm", width = 35, height = 20, dpi=600, scale=1,
       path ="Outputs/NsitesSimulation/Graph/")



main_data <- model_convergence %>%
  filter(parameter %in% c("beta1", "beta0[1,1]", "beta0[2,1]", "beta0[3,1]",
                          "beta1[1,1]", "beta1[2,1]", "beta1[3,1]", "beta0")) %>% 
  mutate(ModelP = factor(paste("", p_values[as.numeric(model)]), 
                         levels = paste("", p_values)),
         Survey = factor(Survey))

# Prepare subset for geom_smooth()
smooth_data <- main_data %>%
  filter(MCMCconverged == TRUE)
