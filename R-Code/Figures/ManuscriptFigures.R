# Package versions:
### Tidyverse v2.0.0
renv::snapshot()
Ylibrary(tidyverse)
library(cowplot)

source("R-Code/Figures/Functions/ProcessFiles.r")
source("R-Code/Figures/Functions/CleanList.r")
source("R-Code/Figures/Functions/Diagnostics.r")


# FirstSubmitModels
FirstSubmit_Nsite_Basic <- "PosteriorEstimates/FirstSubmitModels/NsiteScenarios/BasicModels"
FirstSubmit_Nsite_Multi <- "PosteriorEstimates/FirstSubmitModels/NsiteScenarios/MultiStateModels"
FirstSubmit_Nsurvey_Basic <- "PosteriorEstimates/FirstSubmitModels/NsurveyScenarios/BasicModels"
FirstSubmit_Nsurvey_Multi <- "PosteriorEstimates/FirstSubmitModels/NsurveyScenarios/MultiStateModels"
Nsurvey_Basic_Additive <- "PosteriorEstimates/FirstSubmitModels/NsurveyScenarios/BasicModels/p0.005"
### Revision data ###
RevisionMS_path <- "~/MANUSC~1/MANUSC~1/GITREP~1/MULTIS~2/POSTER~1/HPC_OU~1/MULTIS~1/"
#HPCMS_path <- "PosteriorEstimates/RevisionModels/HPC_Output/MultiState_Cov100Sites_35_Surveys"
HPC_Basic <- "C:/Users/Thoma/OneDrive/DOCUME~1/MANUSC~1/MANUSC~1/GITREP~1/MULTIS~2/POSTER~1/HPC_OU~1/BASIC_~1/"
HPC_Nsurvey <- "C:/Users/Thoma/OneDrive/DOCUME~1/MANUSC~1/MANUSC~1/GITREP~1/MULTIS~2/POSTER~1/HPC_OU~1/7_14_F~1/"


# List all .rds files from each folder
files_Nsite_Basic <- list.files(FirstSubmit_Nsite_Basic, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
files_Nsite_Multi <- list.files(FirstSubmit_Nsite_Multi, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
files_Nsurvey_Basic <- list.files(FirstSubmit_Nsurvey_Basic, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
files_Nsurvey_Basic_Additive <- list.files(Nsurvey_Basic_Additive, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
files_Nsurvey_Multi <- list.files(FirstSubmit_Nsurvey_Multi, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
files_RevisionMS <- list.files(path = RevisionMS_path, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
#files_HPCMS <- list.files(HPCMS_path, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
files_HPCBasic <- list.files(HPC_Basic,pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
files_HPC_Nsurvey <- list.files(HPC_Nsurvey, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
# Process files with appropriate model label
data_Nsite_Basic <- process_files(files_Nsite_Basic, "Basic")
data_Nsite_Multi <- process_files(files_Nsite_Multi, "Multi-state")
data_Nsurvey_Basic <- process_files(files_Nsurvey_Basic, "Basic")
data_Nsurvey_Basic_Additive <- process_files(files_Nsurvey_Basic_Additive)
data_Nsurvey_Multi <- process_files(files_Nsurvey_Multi, "Multi-state")
data_RevisionMS <- process_files(files_RevisionMS, "Multi-state")
#data_HPCMS <- process_files(files_HPCMS, "Multi-state")
data_HPCBasic <- process_files(files_HPCBasic, "Multi-state")
data_HPC_Nsurvey <- process_files(files_HPC_Nsurvey) #contains both multi and basic models
data_Nsurvey_Basic <- data_Nsurvey_Basic  %>%
  filter(model != 2)
data_Nsurvey_Basic_Additive$model[data_Nsurvey_Basic_Additive$model == 1] <- 2
# Combine all processed data into one
Combined_data <- bind_rows(
  data_Nsite_Basic,
  data_Nsite_Multi,
  data_Nsurvey_Basic,
  data_Nsurvey_Basic_Additive,
  data_Nsurvey_Multi,
  data_RevisionMS,
  #data_HPCMS,
  data_HPCBasic,
  data_HPC_Nsurvey
)
#unload from environment to reduce RAM
rm(list = setdiff(ls(), c("Combined_data", "Diagnostic", "analyze_convergence"))) 
# Define thresholds for evaluation
p_values <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9)
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
# Run diagnostics
Diagnostics <- Diagnostic(Combined_data)
Convergence <- analyze_convergence(Diagnostics = Diagnostics ,  
                                  metrics            = c("Bias", "RMSE"),  # Which metrics to consider
                                  pivot_cols         = c("RMSE", "Bias", "Coverage", "cov"),
                                  rel_change_thresh  = 0.10,                # Threshold for rel_change
                                  abs_value_thresh   = 0.10,               # Alternative threshold for Value
                                  rel_change_sd_thresh = 0.10,              # Threshold for rel_change_sd
                                  final_model        = 10,                 # Which model index is considered 'final'
                                  stable_summarise   = TRUE,
                                  abs_sd_thresh = 0.10)              

#Merge First model converged with combined dataset
ConvergedValue <- Convergence[[1]] %>% 
  select(c(parameter,NrSites,Survey, model, ModelType, first_model_converged_all, covariates, truth)) %>% 
  distinct()
filtered_data <- Combined_data %>%
  left_join(ConvergedValue, by = c("parameter", "NrSites", "Survey", "model", 'ModelType', "covariates", "truth")) %>% 
  mutate(parameter = ifelse(parameter == "mean.psi", "psi", parameter))  %>% 
  mutate(ModelType = ifelse(ModelType == "Multi-state", "Multistate", ModelType))



##### FIGURE 2 --- Survey difference --- No Covariates 7, 14, 35, 70, 140, 280 surveys, 60 sites
(NoPointsWithLines_Figure2 <- filtered_data  %>%           
  # 1) Filter data
  filter(
    covariates==FALSE,
         NrSites %in% c(60),
         Survey %in% c(7, 14, 35, 70, 140, 280),
         parameter %in% c("psi","R1", "p", "p22")
         #parameter %in% c("p22", "p32")
         ) %>%
  # 2) Factor manipulations (as before)
  mutate(ModelP = factor(paste("", p_values[as.numeric(model)]), levels = paste("", p_values))) %>%
  mutate(xintercept = sapply(first_model_converged_all, function(tv) which(p_values == tv))) %>% 
  mutate(decimal_count = sapply(p_values[first_model_converged_all], nchar),  # Count the number of characters in threshold value
         x_offset = ifelse(first_model_converged_all >= 5,as.numeric(decimal_count) * 0.38 - 1.5,  as.numeric(decimal_count) * 0.38 + 2.4)) %>%  # Offset text position based on number of decimals
  # 4) Plot
    ggplot(aes(x = ModelP, y = mean)) +
      geom_errorbar(aes(ymin = truth, ymax = truth), linetype = "dashed", color = "#ff0000", linewidth = 1) +
      geom_vline(aes(xintercept = first_model_converged_all), linetype = "dashed", color = "black", linewidth=0.5) +
      geom_text(aes(x = first_model_converged_all + x_offset, y = (max(mean) - 0.1*max(mean)), label = paste("p =", p_values[first_model_converged_all])), 
                color = "black", size = 4, angle = 0, hjust = 1) +
    
  # 5) Facet so that columns are NrSites, rows are Survey
    facet_grid(Survey ~ parameter + ModelType + NrSites,
               labeller = labeller(parameter = as_labeller(parameter_labels, label_parsed))) +
  
  # Smooth lines
    geom_smooth(aes(x = as.numeric(ModelP)), color = "black", linewidth = 1, se = FALSE) +  # Smooth line for mean
    geom_smooth(aes(x = as.numeric(ModelP), y = upper), color = "grey", linetype = "dashed", linewidth = 1, se = FALSE) +  # Upper smooth line, light blue dashed
    geom_smooth(aes(x = as.numeric(ModelP), y = lower), color = "grey", linetype = "dashed", linewidth = 1, se = FALSE) +  # Lower smooth line, light blue dashed

  # Remainder
  #coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Detection Probability", 
       y = "Posterior mean") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 70, hjust = 1, size = 9 * 1.5),
    axis.text.y = element_text(size = 9 * 1.5),
    axis.title  = element_text(size = 10 * 1.5),
    strip.text.x = element_text(size = 7 * 1.5),
    strip.text.y = element_text(size = 7 * 1.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  guides(color = "none", shape = "none"))

ggsave(plot = NoPointsWithLines_Figure2, filename="Figure2_Maintext.png", unit="cm", width = 25, height = 16,dpi=600, scale=1,
       path ="Outputs/Graph/")

##### FIGURE 3 --- Site difference --- Covariates 7, 14, 35 surveys, 60, 120, 240, 500, 1000 sites
(NoPointsWithLines_Figure3 <-filtered_data  %>%
    # 1) Filter data
    filter(
      covariates==TRUE,
           NrSites %in% c(60, 120, 240, 500, 1000),
           Survey %in% c(35),
           #parameter %in% c("psi", "R1")
           parameter %in% c("beta0","beta1","beta1[1,1]", "beta0[1,1]", "beta1[2,1]")) %>%
    # 2) Factor manipulations (as before)
    mutate(ModelP = factor(paste("", p_values[as.numeric(model)]), levels = paste("", p_values))) %>%
    mutate(xintercept = sapply(first_model_converged_all, function(tv) which(p_values == tv))) %>% 
    mutate(decimal_count = sapply(p_values[first_model_converged_all], nchar),  # Count the number of characters in threshold value
           x_offset = ifelse(first_model_converged_all >= 5,as.numeric(decimal_count) * 0.38 - 1.5,  as.numeric(decimal_count) * 0.38 + 1.7)) %>%  # Offset text position based on number of decimals
    # 4) Plot
    ggplot(aes(x = ModelP, y = mean)) +
    geom_errorbar(aes(ymin = truth, ymax = truth), linetype = "dashed", color = "#ff0000", linewidth = 1) +
    geom_vline(aes(xintercept = first_model_converged_all), linetype = "dashed", color = "black", linewidth=0.5) +
    geom_text(aes(x = first_model_converged_all + x_offset, y = (max(mean) - 0.1*max(mean)), label = paste("p =", p_values[first_model_converged_all])), 
              color = "black", size = 4, angle = 0, hjust = 1) +
    
    # 5) Facet so that columns are NrSites, rows are Survey
    facet_grid(Survey + NrSites ~ parameter + ModelType,
               labeller = labeller(parameter = as_labeller(parameter_labels, label_parsed))) +
    # Smooth lines
    geom_smooth(aes(x = as.numeric(ModelP)), color = "black", linewidth = 1, se = FALSE) +  # Smooth line for mean
    geom_smooth(aes(x = as.numeric(ModelP), y = upper), color = "grey", linetype = "dashed", linewidth = 1, se = FALSE) +  # Upper smooth line, light blue dashed
    geom_smooth(aes(x = as.numeric(ModelP), y = lower), color = "grey", linetype = "dashed", linewidth = 1, se = FALSE) +  # Lower smooth line, light blue dashed
    
    
    # Remainder
    coord_cartesian(ylim = c(-5, 20)) +
    labs(x = "Detection Probability", 
         y = "Posterior mean") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 70, hjust = 1, size = 9 * 1.5),
      axis.text.y = element_text(size = 9 * 1.5),
      axis.title  = element_text(size = 10 * 1.5),
      strip.text.x = element_text(size = 7 * 1.5),
      strip.text.y = element_text(size = 7 * 1.5),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    ) +
    guides(color = "none", shape = "none"))

ggsave(plot=NoPointsWithLines_Figure3,filename="Figure3_Maintext.png", unit="cm", width = 25, height = 16,dpi=600, scale=1,
       path ="Outputs/Graph/")

##### FIGURE 4 --- Many Sites Covariates --- 7, 14, 35surveys, 500, 1000 sites, MULTISTATE Only
(NoPointsWithLines_Figure4_1  <-filtered_data  %>%           
    # 1) Filter data
    filter(covariates ==T,
      ModelType == "Multi-state",
           NrSites %in% c(500, 1000),
           Survey %in% c(7, 14, 35),
           #parameter %in% c("beta1[1,1]", "beta0[1,1]")
           parameter %in% c("p22", "p33")
    ) %>%
    # 2) Factor manipulations (as before)
    mutate(ModelP = factor(paste("", p_values[as.numeric(model)]), levels = paste("", p_values))) %>%
    mutate(xintercept = sapply(first_model_converged_all, function(tv) which(p_values == tv))) %>% 
    mutate(decimal_count = sapply(p_values[first_model_converged_all], nchar),  # Count the number of characters in threshold value
           x_offset = ifelse(first_model_converged_all >= 5,as.numeric(decimal_count) * 0.38 - 1.5,  as.numeric(decimal_count) * 0.38 + 1.7)) %>%  # Offset text position based on number of decimals
    # 4) Plot
    ggplot(aes(x = ModelP, y = mean)) +
    geom_errorbar(aes(ymin = truth, ymax = truth), linetype = "dashed", color = "#ff0000", linewidth = 1) +
    geom_vline(aes(xintercept = first_model_converged_all), linetype = "dashed", color = "black", linewidth=0.5) +
    geom_text(aes(x = first_model_converged_all + x_offset, y = (max(mean) - 0.1*max(mean)), label = paste("p =", p_values[first_model_converged_all])), 
              color = "black", size = 4, angle = 0, hjust = 1) +
    
    # 5) Facet so that columns are NrSites, rows are Survey
    facet_grid(Survey ~ parameter + ModelType + NrSites,
               labeller = labeller(parameter = as_labeller(parameter_labels, label_parsed))) +
    # Smooth lines
    geom_smooth(aes(x = as.numeric(ModelP)), color = "black", linewidth = 1, se = FALSE) +  # Smooth line for mean
    geom_smooth(aes(x = as.numeric(ModelP), y = upper), color = "grey", linetype = "dashed", linewidth = 1, se = FALSE) +  # Upper smooth line, light blue dashed
    geom_smooth(aes(x = as.numeric(ModelP), y = lower), color = "grey", linetype = "dashed", linewidth = 1, se = FALSE) +  # Lower smooth line, light blue dashed
    
    
    # Remainder
    #coord_cartesian(ylim = c(-5, 20)) +
    labs(x = "Detection Probability", 
         y = "Posterior mean") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 70, hjust = 1, size = 9 * 1.5),
      axis.text.y = element_text(size = 9 * 1.5),
      axis.title  = element_text(size = 10 * 1.5),
      strip.text.x = element_text(size = 7 * 1.5),
      strip.text.y = element_text(size = 7 * 1.5),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    ) +
    guides(color = "none", shape = "none"))

ggsave(plot=NoPointsWithLines_Figure4,filename="Figure4_Maintext.png", unit="cm", width = 25, height = 16,dpi=600, scale=1,
       path ="Outputs/Graph/")


# Combine the two plots
(combined_plot <- plot_grid(NoPointsWithLines_Figure4_1, NoPointsWithLines_Figure4_2, ncol = 1, align = "v"))
# Save the combined plot
ggsave(filename="Figure4_Maintext.png", plot = combined_plot, unit="cm", width = 25, height = 35, dpi=600, scale=1,
       path ="Outputs/Graph/")
################
################## POINTS #################
##################

############# Appendix Plots
model_convergence <- filtered_data %>%
  filter(!is.na(truth)) %>% 
  mutate(has_n_eff = case_when(n.eff == NA ~ FALSE, .default = TRUE)) %>% 
  group_by(model_identifier) %>%
  mutate(
       MCMCconverged = !any(
         parameter %in% c("beta0","beta0[1,1]", "beta0[2,1]","beta0[3,1]",
                              "beta1" ,"beta1[1,1]", "beta1[2,1]",
                              "beta1[3,1]",  "p", "p11", "p22", "p33", "p21", "p31", "p32",
                       "mean.psi", "psi", "R1", "R2") &
        (rhat > 1.1)
    )
  ) %>%
  ungroup()

##### FIGURE 2 --- Survey difference --- No Covariates 7, 14, 35, 70, 140, 280 surveys, 60 sites
(Appendix1_FigureNoCovs_p <- model_convergence  %>%     
    group_by(model_identifier) %>% 
    # 1) Filter data
    filter(
      covariates==FALSE,
      NrSites %in% c(60),
      Survey %in% c(7, 14, 35, 70, 140, 280),
      #parameter %in% c("psi","R1","R2")
      parameter %in% c("p22","p21", "p33", "p31","p11", "p32")
    ) %>%
    # 2) Factor manipulations (as before)
    mutate(ModelP = factor(paste("", p_values[as.numeric(model)]), levels = paste("", p_values))) %>%
    mutate(xintercept = sapply(first_model_converged_all, function(tv) which(p_values == tv))) %>% 
    mutate(decimal_count = sapply(p_values[first_model_converged_all], nchar),  # Count the number of characters in threshold value
           x_offset = ifelse(first_model_converged_all >= 5,as.numeric(decimal_count) * 0.38 - 1.5,  as.numeric(decimal_count) * 0.38 + 2.4)) %>%  # Offset text position based on number of decimals
    # 4) Plot
    ggplot(aes(x = ModelP, y = mean)) +
    geom_errorbar(aes(ymin = truth, ymax = truth), linetype = "dashed", color = "#ff0000", linewidth = 1) +
    geom_vline(aes(xintercept = first_model_converged_all), linetype = "dashed", color = "black", linewidth=0.5) +
    geom_text(aes(x = first_model_converged_all + x_offset, y = (max(mean) - 0.1*max(mean)), label = paste("p =", p_values[first_model_converged_all])), 
              color = "black", size = 4, angle = 0, hjust = 1) +
    
    # 5) Facet so that columns are NrSites, rows are Survey
    facet_grid(Survey ~ parameter + ModelType + NrSites,
               labeller = labeller(parameter = as_labeller(parameter_labels, label_parsed))) +
    
    # Smooth lines

    geom_point(aes(x= as.factor(ModelP), color = ifelse(rhat > 1.1, "orange", "#333333"), 
                   shape = factor(ifelse(rhat > 1.1, "High Rhat", "Normal Rhat"))), position=position_jitter(width=0.3), size=0.9) + 
    scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +
    scale_color_identity() +
    geom_smooth(aes(x = as.numeric(ModelP),y=mean), col="blue")+
    # Remainder
    #coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Detection Probability", 
         y = "Posterior mean") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 70, hjust = 1, size = 9 * 1.5),
      axis.text.y = element_text(size = 9 * 1.5),
      axis.title  = element_text(size = 10 * 1.5),
      strip.text.x = element_text(size = 7 * 1.5),
      strip.text.y = element_text(size = 7 * 1.5),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    ) +
    guides(color = "none", shape = "none"))

ggsave(plot = Appendix1_FigureNoCovs_state, filename="FigureA1S1.png", unit="cm", width = 25, height = 16,dpi=600, scale=1,
       path ="Outputs/Graph/")
ggsave(plot = Appendix1_FigureNoCovs_p, filename="FigureA1S2.png", unit="cm", width = 25, height = 16,dpi=600, scale=1,
       path ="Outputs/Graph/")

##### FIGURE 3 --- Site difference --- Covariates 7, 14, 35 surveys, 60, 120, 240, 500, 1000 sites
(Appendix1_FigureNoCovs_state <-model_convergence  %>%
    # 1) Filter data
    filter(
      covariates==TRUE,
      NrSites %in% c(60, 120, 240, 500, 1000),
      Survey %in% c(35),
      parameter %in% c("p22","p21", "p33", "p31","p11", "p32")) %>% 
      #parameter %in% c("beta0","beta1","beta1[1,1]", "beta0[1,1]", "beta1[2,1]")) %>%
    # 2) Factor manipulations (as before)
    mutate(ModelP = factor(paste("", p_values[as.numeric(model)]), levels = paste("", p_values))) %>%
    mutate(xintercept = sapply(first_model_converged_all, function(tv) which(p_values == tv))) %>% 
    mutate(decimal_count = sapply(p_values[first_model_converged_all], nchar),  # Count the number of characters in threshold value
           x_offset = ifelse(first_model_converged_all >= 5,as.numeric(decimal_count) * 0.38 - 1.5,  as.numeric(decimal_count) * 0.38 + 1.7)) %>%  # Offset text position based on number of decimals
    # 4) Plot
    ggplot(aes(x = ModelP, y = mean)) +
    geom_errorbar(aes(ymin = truth, ymax = truth), linetype = "dashed", color = "#ff0000", linewidth = 1) +
    geom_vline(aes(xintercept = first_model_converged_all), linetype = "dashed", color = "black", linewidth=0.5) +
    geom_text(aes(x = first_model_converged_all + x_offset, y = (max(mean) - 0.1*max(mean)), label = paste("p =", p_values[first_model_converged_all])), 
              color = "black", size = 4, angle = 0, hjust = 1) +
    
    # 5) Facet so that columns are NrSites, rows are Survey
    facet_grid(Survey + NrSites ~ parameter + ModelType,
               labeller = labeller(parameter = as_labeller(parameter_labels, label_parsed))) +
    # Smooth lines
    geom_point(aes(x= as.factor(ModelP), color = ifelse(rhat > 1.1, "orange", "#333333"), 
                   shape = factor(ifelse(rhat > 1.1, "High Rhat", "Normal Rhat"))), position=position_jitter(width=0.3), size=0.9) + 
    scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +
    scale_color_identity() +
    geom_smooth(aes(x = as.numeric(ModelP),y=mean), col="blue")+
    
    # Remainder
    #coord_cartesian(ylim = c(-5, 20)) +
    labs(x = "Detection Probability", 
         y = "Posterior mean") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 70, hjust = 1, size = 9 * 1.5),
      axis.text.y = element_text(size = 9 * 1.5),
      axis.title  = element_text(size = 10 * 1.5),
      strip.text.x = element_text(size = 7 * 1.5),
      strip.text.y = element_text(size = 7 * 1.5),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    ) +
    guides(color = "none", shape = "none"))

ggsave(plot=Appendix1_FigureNoCovs_state,filename="AppendixS1S4.png", unit="cm", width = 25, height = 16,dpi=600, scale=1,
       path ="Outputs/Graph/")

##### FIGURE 4 --- Many Sites Covariates --- 7, 14, 35surveys, 500, 1000 sites, MULTISTATE Only
(AppendixS5_FigureNoCovs_state  <-model_convergence  %>%           
    # 1) Filter data
    filter(covariates ==T,
           ModelType == "Multi-state",
           NrSites %in% c(500, 1000),
           Survey %in% c(7, 14),
          parameter %in% c("p22","p21", "p33", "p31","p11", "p32")) %>% 
          #parameter %in% c("beta0","beta1","beta1[1,1]", "beta0[1,1]", "beta1[2,1]")) %>%
    # 2) Factor manipulations (as before)
    mutate(ModelP = factor(paste("", p_values[as.numeric(model)]), levels = paste("", p_values))) %>%
    mutate(xintercept = sapply(first_model_converged_all, function(tv) which(p_values == tv))) %>% 
    mutate(decimal_count = sapply(p_values[first_model_converged_all], nchar),  # Count the number of characters in threshold value
           x_offset = ifelse(first_model_converged_all >= 5,as.numeric(decimal_count) * 0.38 - 1.5,  as.numeric(decimal_count) * 0.38 + 1.7)) %>%  # Offset text position based on number of decimals
    # 4) Plot
    ggplot(aes(x = ModelP, y = mean)) +
    geom_errorbar(aes(ymin = truth, ymax = truth), linetype = "dashed", color = "#ff0000", linewidth = 1) +
    geom_vline(aes(xintercept = first_model_converged_all), linetype = "dashed", color = "black", linewidth=0.5) +
    geom_text(aes(x = first_model_converged_all + x_offset, y = (max(mean) - 0.1*max(mean)), label = paste("p =", p_values[first_model_converged_all])), 
              color = "black", size = 4, angle = 0, hjust = 1) +
    
    # 5) Facet so that columns are NrSites, rows are Survey
    facet_grid(Survey ~ parameter + ModelType + NrSites,
               labeller = labeller(parameter = as_labeller(parameter_labels, label_parsed))) +
    # Smooth lines
    geom_point(aes(x= as.factor(ModelP), color = ifelse(rhat > 1.1, "orange", "#333333"), 
                   shape = factor(ifelse(rhat > 1.1, "High Rhat", "Normal Rhat"))), position=position_jitter(width=0.3), size=0.9) + 
    scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +
    scale_color_identity() +
    geom_smooth(aes(x = as.numeric(ModelP),y=mean), col="blue")+
    
    # Remainder
    #coord_cartesian(ylim = c(-5, 20)) +
    labs(x = "Detection Probability", 
         y = "Posterior mean") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 70, hjust = 1, size = 9 * 1.5),
      axis.text.y = element_text(size = 9 * 1.5),
      axis.title  = element_text(size = 10 * 1.5),
      strip.text.x = element_text(size = 7 * 1.5),
      strip.text.y = element_text(size = 7 * 1.5),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    ) +
    guides(color = "none", shape = "none"))

ggsave(plot=AppendixS5_FigureNoCovs_state,filename="S1S5.png", unit="cm", width = 35, height = 16,dpi=600, scale=1,
       path ="Outputs/Graph/")


# Combine the two plots
(combined_plot <- plot_grid(NoPointsWithLines_Figure4_1, NoPointsWithLines_Figure4_2, ncol = 1, align = "v"))
# Save the combined plot
ggsave(filename="Figure4_Maintext.png", plot = combined_plot, unit="cm", width = 25, height = 35, dpi=600, scale=1,
       path ="Outputs/Graph/")
