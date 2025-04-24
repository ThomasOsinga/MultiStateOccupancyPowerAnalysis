## WorkingDirectory = MultiStateOccupancyModelsPowerAnalysis - RepositoryFolder
####  
#Cowplot: V1.1.3
#Tidyverse: V2.0.0####
for (pkg in c("cowplot", "tidyverse")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

source("R-code/Figures/ProcessFiles.r")
source("PosteriorEstimates/Plotting/CleanList.r")
source("R-code/Figures/Diagnostics2.r")

multistate_files  <- "C:/Users/Thoma/Documents/2025/SimulationsOccupancy/DifferentOccupancy"
multistate_files  <- "~/Manuscripts/Manuscript Detection Probabilities/Revision/Revised modelCode/IntermediateResults/MultiState"
multistate_files <- list.files(path = multistate_files, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
# Process files and add a column for number of sites and model type (multistate or singlestate)
multistate_data <- process_files(multistate_files , "Multi-state") 

combined_data <- multistate_data
Diagnostics <- Diagnostic(combined_data)

Convergence <-analyze_convergence(Diagnostics = Diagnostics ,  
                                  metrics            = c("Bias", "RMSE"),  # Which metrics to consider
                                  pivot_cols         = c("RMSE", "Bias", "Coverage", "cov"),
                                  rel_change_thresh  = 0.05,                # Threshold for rel_change
                                  abs_value_thresh   = 0.05,               # Alternative threshold for Value
                                  rel_change_sd_thresh = 0.05,              # Threshold for rel_change_sd
                                  final_model        = 9,                 # Which model index is considered 'final'
                                  stable_summarise   = TRUE)

#Merge First model converged with combined dataset
ConvergedValue <- Convergence[[1]] %>% 
  select(c(parameter,NrSites,Survey, model, ModelType, first_model_converged_all, occupancy))
filtered_data <- combined_data %>%
  left_join(ConvergedValue, by = c("parameter", "NrSites", "Survey", "model", 'ModelType', 'occupancy')) %>% 
  filter(parameter %in% c("p","p1","p22", "p33","p21","p31","p32", "psi", "R1", "R2", "beta0[1,1]","beta0[2,1]","beta0[3,1]","beta1[1,1]","beta1[2,1]","beta1[3,1]"))  
#filter(parameter %in% target_parameters)

p_values <- c(0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9)
############# Appendix Plots
model_convergence <- filtered_data %>%
  mutate(has_n_eff = "n.eff" %in% names(.data)) %>% 
  group_by(model_identifier) %>%
  mutate(
    MCMCconverged = !any(
      parameter %in% c("p", "p1", "p22", "p33", "p21", "p31", "p32",
                       "mean.psi", "psi", "R1", "R2", "beta0[1,1]","beta0[2,1]","beta0[3,1]","beta1[1,1]","beta1[2,1]","beta1[3,1]") &
        (rhat > 1.1 | ifelse(has_n_eff, (n.eff < 200), rhat > 1.1))
    )
  ) %>%
  ungroup()

parameter_labels <- c(
  "psi"          = "psi",
  "beta0"        = "beta[0]",
  "beta0[1,1]"   = "beta[0[psi]]",
  "beta0[2,1]"   = "beta[0[R[1]]]",
  "beta0[3,1]"   = "beta[0[R[2]]]",
  "beta1"        = "beta[1]",
  "beta1[1,1]"   = "beta[1[psi]]",
  "beta1[2,1]"   = "beta[1[R[1]]]",
  "beta1[3,1]"   = "beta[1[R[2]]]",
  "R1"           = "R[1]",
  "R2"           = "R[2]",
  "p"            = "p",
  "p1"           = "p^{1*\",\"*1}",
  "p21"          = "p^{2*\",\"*1}",
  "p22"          = "p^{2*\",\"*2}", 
  "p31"          = "p^{3*\",\"*1}",
  "p32"          = "p^{3*\",\"*2}",
  "p33"          = "p^{3*\",\"*3}"
)

# Plot with dynamic xintercepts based on threshold_value
(NoPointsWithLines <- filtered_data %>%
    #filter(parameter %in% c("R2"), covariates==FALSE) %>% 
    #filter(parameter %in% c("beta0[1,1]","beta1[2,1]")) %>% 
    #filter(parameter %in% c("beta1[2,1]")) %>% 
    filter(parameter %in% c('beta0',"beta0[1,1]","beta0[2,1]","beta0[3,1]","beta1[2,1]", 'beta1'),
          covariates==TRUE) %>%  
    filter(
      NrSites %in% c(60),
      Survey %in% c(7, 14, 35, 70, 140, 280, 500, 1000),
    ) %>%
    #filter(parameter %in% c("p","p21","psi", "R1", "R2")) %>% 
    mutate(ModelP = factor(paste("", p_values[as.numeric(model)]), levels = paste("", p_values))) %>%
    mutate(xintercept = sapply(first_model_converged_all, function(tv) which(p_values == tv))) %>% 
    mutate(
      decimal_count = sapply(p_values[first_model_converged_all], nchar),
      x_offset      = as.numeric(decimal_count) * 0.38 - 1.5
    ) %>% 
  # Offset text position based on number of decimals
    ggplot(aes(x = ModelP, y = mean)) +
    scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +        
    geom_errorbar(aes(ymin = truth, ymax = truth), linetype = "dashed", color = "#ff0000", linewidth = 1) +
    geom_vline(aes(xintercept = first_model_converged_all), linetype = "dashed", color = "black", size=0.5) +
    geom_text(aes(x = first_model_converged_all + x_offset, y = (max(mean) - 0.1*max(mean)), label = paste("p =", p_values[first_model_converged_all])), 
              color = "black", size = 4, angle = 0, hjust = 1) +
    facet_grid(Survey ~ parameter + ModelType + NrSites,
               labeller = labeller(parameter = as_labeller(parameter_labels, label_parsed))) +  # Facet by Survey, Parameter, and ModelType
    geom_smooth(aes(x = as.numeric(ModelP)), color = "black", size = 1, se = FALSE) +  # Smooth line for mean
    geom_smooth(aes(x = as.numeric(ModelP), y = upper), color = "grey", linetype = "dashed", size = 1, se = FALSE) +  # Upper smooth line, light blue dashed
    geom_smooth(aes(x = as.numeric(ModelP), y = lower), color = "grey", linetype = "dashed", size = 1, se = FALSE) +  # Lower smooth line, light blue dashed
    #coord_cartesian(xlim = c(0, 0.9)) +
    labs(x = "Detection Probability", 
         y = "Posterior mean") +  
    theme_classic() +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 9*1.5), 
          axis.text.y = element_text(size = 9*1.5), 
          axis.title = element_text(size = 10*1.5),
          strip.text.x = element_text(size = 7*1.5), 
          strip.text.y = element_text(size = 7*1.5), 
          panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
    guides(color = "none", shape = "none") ) # Remove the legend for color and shape


ggsave(filename="Figure2_Maintext.png", unit="cm", width = 25, height = 16,dpi=600, scale=1,
       path ="Outputs/NsurveySimulation/Graph/")


# Modify the filtered_data to use the factor levels for parameter
filtered_data <- filtered_data %>%
  mutate(parameter = factor(parameter))


############# Appendix Plots
model_convergence <- filtered_data %>%
  mutate(has_n_eff = "n.eff" %in% names(filtered_data)) %>% 
  group_by(model_identifier) %>%
  mutate(
    MCMCconverged = !any(
      parameter %in% c("p", "p1", "p22", "p33", "p21", "p31", "p32",
                       "mean.psi", "psi", "R1", "R2") &
        (rhat > 1.1 | ifelse(has_n_eff, (n.eff < 200), rhat > 1.1))
    )
  ) %>%
  ungroup()

# Plot with dynamic xintercepts based on threshold_value
(NoPointsWithLines <- model_convergence %>%
    #filter(parameter %in% c("psi")) %>% 
    filter(parameter %in% c("R1")) %>% 
    #filter(parameter %in% c("p22","p33","p21","p31")) %>% 
    #filter(parameter %in% c("psi", "R1", "R2", "beta0", "beta0[1,1]")) %>% 
    mutate(ModelP = factor(paste("", p_values[as.numeric(model)]), levels = paste("", p_values))) %>%
    mutate(Survey = factor(Survey)) %>%
    ggplot(aes(x = ModelP, y = mean)) +
    geom_point(aes(x= as.factor(ModelP), color = ifelse(MCMCconverged==FALSE, "orange", "#333333"), 
                   shape = factor(ifelse(MCMCconverged==FALSE, "High Rhat", "Normal Rhat"))),
               position=position_jitter(width=0.3), size=0.9) + 
    scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +
    scale_color_identity() +
    geom_vline(aes(xintercept = first_model_converged_all), linetype = "dashed", color = "black", size=0.5) +
    geom_errorbar(aes(ymin = truth, ymax = truth), linetype = "dashed", color = "#ff0000", linewidth = 1) +
    facet_grid(Survey + occupancy ~ parameter + ModelType+ NrSites ,
               labeller = labeller(parameter = as_labeller(parameter_labels, label_parsed))) +
    geom_smooth(aes(x = as.numeric(ModelP)), color = "blue", size = 1, se = FALSE) +  # Smooth line for mean
    #coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Detection Probability", 
         y = "Posterior mean") +  
    theme_classic() +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 9*1.5), 
          axis.text.y = element_text(size = 9*1.5), 
          axis.title = element_text(size = 10*1.5),
          strip.text.x = element_text(size = 7*1.5), 
          strip.text.y = element_text(size = 7*1.5), 
          panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
    guides(color = "none", shape = "none") ) # Remove the legend for color and shape


ggsave(filename="AppendixS1FigureS2.png", unit="cm", width = 25, height = 16,dpi=600, scale=1)


x <- readRDS("~/Manuscripts/Manuscript Detection Probabilities/Revision/Revised modelCode/IntermediateResults/lowoccupancy/SummaryResults_Multi-state_NoCov_Nsites60_Nsurveys7occ_0.1_session1_sub1_20250128103105.rds")
