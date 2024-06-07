clean_list <- function(x) {
  if (is.list(x)) {
    # Remove NULL elements
    x <- Filter(Negate(is.null), x)
    # Recursively apply the cleaning function to non-NULL elements
    x <- lapply(x, clean_list)
  }
  return(x)
}

model_simulations <- list()
### ## first 5 models
model_simulations <- readRDS('C:/Users/Thoma/OneDrive - Sveriges lantbruksuniversitet/Thesis documents/Multi_state_occu/detection_Simulations_1.rds')
## 200 sites ##
model_simulations <- readRDS('C:/Users/Thoma/OneDrive - Sveriges lantbruksuniversitet/Thesis documents/SimulationFolderThomas/Output/SummaryResults_200sites.rds')

###
file_directory <- "C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Data/Results/oneCov4States_Henjo"
file_list <- list.files(path = file_directory, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)






######       4 state 1 cov big sim                       ###############
# Load all files into a list
summary_results_list <- lapply(file_list, readRDS)
names(summary_results_list) <- basename(file_list)
names(summary_results_list)
all_model_data <- list()
#model_simulations<-clean_list(summary_results_list[[1]])
# Process each summary result file
for (file_index in 1:length(summary_results_list)) {
  model_simulations <- clean_list(summary_results_list[[file_index]])
  # Loop through each survey
  for (survey_index in 1:1) {
    # Loop through each model in the survey
    for (model_index in 1:length(model_simulations[[1]][[survey_index]])) {
      # Loop through each simulation in the model
      for (simulation_index in 1:length(model_simulations[[1]][[survey_index]][[model_index]])) {
        # Extract the summary data frame
        summary_data <- as.data.frame(model_simulations[[1]][[survey_index]][[model_index]][[simulation_index]][["summary"]])
        
        # Detect detection probability information based on survey_index
        detection_prob <- c("n35")[survey_index]  # Adjust if more surveys
        
        # Enhance the data frame with model, survey, and simulation information
        summary_data_enhanced <- summary_data %>%
          mutate(Model = model_index,
                 Survey = detection_prob)
        
        # Append to the list using a unique identifier
        list_name <- paste("Model", model_index, "Survey", detection_prob, "Sim", sep="_")
        if (!exists(list_name, where = all_model_data)) {
          all_model_data[[list_name]] <- summary_data_enhanced
        } else {
          all_model_data[[list_name]] <- rbind(all_model_data[[list_name]], summary_data_enhanced)
        }
      }
    }
  }
}
combined_data <- bind_rows(all_model_data)
combined_data <- unique(combined_data)
####### Code for single model ####
  model_simulations <- clean_list(model_simulations[[1]])
library(tidyverse)
library(gridExtra)  # For arranging multiple plots
# Initialize a list to store data from all simulations, models, and surveys
all_model_data <- list()
# Loop through each survey
for (survey_index in 1:length(model_simulations)) {
  # Loop through each model in the survey
  for (model_index in 1:length(model_simulations[[survey_index]])) {
    # Loop through each simulation in the model
    for (simulation_index in 1:length(model_simulations[[survey_index]][[model_index]])) {
      # Extract the summary data frame
      summary_data <- as.data.frame(model_simulations[[survey_index]][[model_index]][[simulation_index]][["summary"]])
      
      # Detect detection probability information based on survey_index
      detection_prob <- c("n35")[survey_index]  # Adjust if more surveys
      
      # Enhance the data frame with model, survey, and simulation information
      summary_data_enhanced <- summary_data %>%
        mutate(Model = model_index,
               Survey = detection_prob,
               Simulation = simulation_index, 
               ModelSurvey = paste("Model", model_index, "Survey", detection_prob))
      
      # Append to the list using a unique identifier
      list_name <- paste("Model", model_index, "Survey", detection_prob, "Sim", simulation_index, sep="_")
      if (!exists(list_name, where = all_model_data)) {
        all_model_data[[list_name]] <- summary_data_enhanced
      } else {
        all_model_data[[list_name]] <- rbind(all_model_data[[list_name]], summary_data_enhanced)
      }
    }
  }
}
# Combine all data frames into a single data frame
combined_data <- bind_rows(all_model_data)
######
head(combined_data)


  ###### 60 sites ####
OneCov_alldata <- combined_data[combined_data$Survey == "n35", ]
p_values <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9)
(mean_plotP_60 <- OneCov_alldata %>% 
  mutate(ModelP = factor(paste("p =", p_values[Model]), levels = paste("p =", p_values))) %>% 
  filter(!parameter %in% c('beta0[1,1]', 'beta0[2,1]','beta0[3,1]','beta1[1,1]','beta1[2,1]','beta1[3,1]','deviance', "mean.psi", "mean.R1", 
                           "mean.R2", "n.occ[1]", "n.occ[2]", 
                           "n.occ[3]", "n.occ[4]", 'mean.Omega1', 'mean.Omega2', 'mean.Omega3', 'mean.Omega4'))%>%
  
  ggplot(aes(x = as.numeric(ModelP), y = mean)) +
  geom_point(aes(x= as.factor(ModelP), color = ifelse(rhat > 1.1, "orange", "#333333"), 
                 shape = factor(ifelse(rhat > 1.1, "High Rhat", "Normal Rhat"))), position=position_jitter(width=0.3), size=0.9) + 
  scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +        
  scale_color_identity() +
  geom_segment(aes(x = Model - 0.5, 
                   xend = as.numeric(as.factor(ModelP)) + 0.5, y = truth, yend = truth),
                   color = "red", linetype = "dashed") +
  geom_smooth(col = "blue") + 
  facet_wrap(~parameter, ncol=3) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x="Detection Probability",
       y="Posterior mean") +
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 9), axis.text.y = element_text(size = 9), 
        axis.title = element_text(size=10),
        strip.text.x = element_text(size=7), legend.position = "none")) # Enhance facet label readability

ggsave(filename="Omegaps_mean_60.png", unit="cm", width = 25, height = 16,dpi=600, scale=0.7,
       path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/")

## SD PLOT
(sd_plotP_60 <- OneCov_alldata %>% 
  mutate(ModelP = factor(paste("p =", p_values[Model]), levels = paste("p =", p_values))) %>% 
  filter(!parameter %in% c('beta0[1,1]', 'beta0[2,1]','beta0[3,1]','beta1[1,1]','beta1[2,1]','beta1[3,1]','deviance', "mean.psi", "mean.R1", 
                           "mean.R2", "n.occ[1]", "n.occ[2]", 
                           "n.occ[3]", "n.occ[4]", 'mean.Omega1', 'mean.Omega2', 'mean.Omega3', 'mean.Omega4'))%>% 
  
  ggplot(aes(x = as.numeric(ModelP), y = sd)) +
  geom_point(aes(x= as.factor(ModelP), color = ifelse(rhat > 1.1, "orange", "#333333"), 
                 shape = factor(ifelse(rhat > 1.1, "High Rhat", "Normal Rhat"))), position=position_jitter(width=0.3), size=0.9) + 
  scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +        
  scale_color_identity() +
  geom_smooth(col = "blue") + 
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~parameter, ncol=3) +
  labs(x="Detection Probability",
       y="Posterior standard deviation") +
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 9), axis.text.y = element_text(size = 9), 
        axis.title = element_text(size=10),
        strip.text.x = element_text(size=7), legend.position = "none")) # Enhance facet label readability

ggsave(filename="Omegaps_sd_60.png", unit="cm", width = 25, height = 16,dpi=600, scale=0.7,
       path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/")


################ Regression parameters estimations ##############
# Calculate average upper and lower bounds for each model and parameter
(mean_plotB_60 <- OneCov_alldata %>% 
  mutate(ModelP = factor(paste("p =", p_values[Model]), levels = paste("p =", p_values))) %>% 
  filter(parameter %in% c('beta0[1,1]', 'beta0[2,1]','beta0[3,1]','beta1[1,1]','beta1[2,1]','beta1[3,1]'))%>% 
  ggplot(aes(x = Model, y = mean)) +
  geom_point(aes(x= as.factor(ModelP), color = ifelse(rhat > 1.1, "orange", "#333333"), 
                 shape = factor(ifelse(rhat > 1.1, "High Rhat", "Normal Rhat"))), position=position_jitter(width=0.3), size=0.9) + 
  scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +        

  scale_color_identity() +
  geom_segment(aes(x = as.numeric(as.factor(Model)) - 0.5, 
                   xend = as.numeric(as.factor(Model)) + 0.5, y = truth, yend = truth),
               color = "red", linetype = "dashed") +
  geom_smooth(col = "blue") + 
  facet_wrap(~parameter, ncol=3) +
  labs(x="Detection Probability",
       y="Posterior mean") +
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 9), axis.text.y = element_text(size = 9), 
        axis.title = element_text(size=10),
        strip.text.x = element_text(size=7), legend.position = "none")) # # Enhance facet label readability

ggsave(filename="Beta_means_60.png",  unit="cm", width = 25, height = 16,dpi=600, scale=0.7,
       path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/")


(sd_plotB_60 <- OneCov_alldata %>% 
  mutate(ModelP = factor(paste("p =", p_values[Model]), levels = paste("p =", p_values))) %>% 
  filter(parameter %in% c('beta0[1,1]', 'beta0[2,1]','beta0[3,1]','beta1[1,1]','beta1[2,1]','beta1[3,1]'))%>% 
  ggplot(aes(x = Model, y = sd)) +
  geom_point(aes(x= as.factor(ModelP), color = ifelse(rhat > 1.1, "orange", "#333333"), 
                 shape = factor(ifelse(rhat > 1.1, "High Rhat", "Normal Rhat"))), position=position_jitter(width=0.3), size=0.9) + 
  scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +        
  scale_color_identity() +
  geom_smooth(col = "blue") + 
  facet_wrap(~parameter, scales = "free") +
  labs(
       x = "Detection probability", y = "Posterior standard deviation ") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none"))

### save
ggsave(filename="Beta_SD_60.png", unit="cm", width = 25, height = 16,dpi=600, scale=0.7,
       path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/")


###combined mean and SD
combined_plot_60_P <- (mean_plotP_60 / sd_plotP_60) + plot_annotation(tag_levels = "a")
ggsave(combined_plot_60_P, filename="mean+SD_60_P.png", unit="cm", width = 30, height = 36,dpi=600, scale=0.7,
       path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/4stateonecov/")

combined_plot_60_betas <- (mean_plotB_60 / sd_plotB_60) + plot_annotation(tag_levels = "a")
ggsave(combined_plot_60_betas, filename="mean+SD_60_betas.png", unit="cm", width = 30, height = 36,dpi=600, scale=0.7,
       path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/4stateonecov/")

#### 200 sites ####
OneCov_alldata_200 <- combined_data[combined_data$Survey == "n35", ]
p_values <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9)
(mean_plotP_200 <- OneCov_alldata_200 %>% 
    mutate(ModelP = factor(paste("p =", p_values[Model]), levels = paste("p =", p_values))) %>% 
    filter(!parameter %in% c('beta0[1,1]', 'beta0[2,1]','beta0[3,1]','beta1[1,1]','beta1[2,1]','beta1[3,1]','deviance', "mean.psi", "mean.R1", 
                             "mean.R2", "n.occ[1]", "n.occ[2]", 
                             "n.occ[3]", "n.occ[4]", 'mean.Omega1', 'mean.Omega2', 'mean.Omega3', 'mean.Omega4'))%>%
    
    ggplot(aes(x = as.numeric(ModelP), y = mean)) +
    geom_point(aes(x= as.factor(ModelP), color = ifelse(rhat > 1.1, "orange", "#333333"), 
                   shape = factor(ifelse(rhat > 1.1, "High Rhat", "Normal Rhat"))), position=position_jitter(width=0.3), size=0.9) + 
    scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +        
    scale_color_identity() +
    geom_segment(aes(x = Model - 0.5, 
                     xend = as.numeric(as.factor(ModelP)) + 0.5, y = truth, yend = truth),
                 color = "red", linetype = "dashed") +
    geom_smooth(col = "blue") + 
    facet_wrap(~parameter, ncol=3) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x="Detection Probability",
         y="Posterior mean") +
    theme_classic()+
    theme(axis.text.x = element_text(angle=45, hjust=1, size = 9), axis.text.y = element_text(size = 9), 
          axis.title = element_text(size=10),
          strip.text.x = element_text(size=7), legend.position = "none")) # Enhance facet label readability

ggsave(filename="Omegaps_mean_200.png", unit="cm", width = 25, height = 16,dpi=600, scale=0.7,
       path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/")


combined_plot_200_P <- (mean_plotB_200 / sd_plotB_200) + plot_annotation(tag_levels = "a")
ggsave(combined_plot_200_betas, filename="mean+SD_200_P.png", unit="cm", width = 30, height = 36,dpi=600, scale=0.7,
       path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/4stateonecov/200/")


## SD PLOT
(sd_plotP_200 <- OneCov_alldata_200 %>% 
    mutate(ModelP = factor(paste("p =", p_values[Model]), levels = paste("p =", p_values))) %>% 
    filter(!parameter %in% c('beta0[1,1]', 'beta0[2,1]','beta0[3,1]','beta1[1,1]','beta1[2,1]','beta1[3,1]','deviance', "mean.psi", "mean.R1", 
                             "mean.R2", "n.occ[1]", "n.occ[2]", 
                             "n.occ[3]", "n.occ[4]", 'mean.Omega1', 'mean.Omega2', 'mean.Omega3', 'mean.Omega4'))%>% 
    
    ggplot(aes(x = as.numeric(ModelP), y = mean)) +
    geom_point(aes(x= as.factor(ModelP), color = ifelse(rhat > 1.1, "orange", "#333333"), 
                   shape = factor(ifelse(rhat > 1.1, "High Rhat", "Normal Rhat"))), position=position_jitter(width=0.3), size=0.9) + 
    scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +        
    scale_color_identity() +
    geom_segment(aes(x = Model - 0.5, 
                     xend = as.numeric(as.factor(ModelP)) + 0.5, y = truth, yend = truth),
                 color = "red", linetype = "dashed") +
    geom_smooth(col = "blue") + 
    facet_wrap(~parameter, ncol=3) +
    labs(x="Detection Probability",
         y="Posterior standard deviation") +
    theme_classic()+
    theme(axis.text.x = element_text(angle=45, hjust=1, size = 9), axis.text.y = element_text(size = 9), 
          axis.title = element_text(size=10),
          strip.text.x = element_text(size=7), legend.position = "none")) # Enhance facet label readability

ggsave(filename="Omegaps_sd_200.png", unit="cm", width = 25, height = 16,dpi=600, scale=0.7,
       path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/")


################ Regression parameters estimations ##############
# Calculate average upper and lower bounds for each model and parameter
(mean_plotB_200 <- OneCov_alldata_200 %>% 
   mutate(ModelP = factor(paste("p =", p_values[Model]), levels = paste("p =", p_values))) %>% 
   filter(parameter %in% c('beta0[1,1]', 'beta0[2,1]','beta0[3,1]','beta1[1,1]','beta1[2,1]','beta1[3,1]'))%>% 
   ggplot(aes(x = Model, y = mean)) +
   geom_point(aes(x= as.factor(ModelP), color = ifelse(rhat > 1.1, "orange", "#333333"), 
                  shape = factor(ifelse(rhat > 1.1, "High Rhat", "Normal Rhat"))), position=position_jitter(width=0.3), size=0.9) + 
   scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +        
   
   scale_color_identity() +
   geom_segment(aes(x = as.numeric(as.factor(Model)) - 0.5, 
                    xend = as.numeric(as.factor(Model)) + 0.5, y = truth, yend = truth),
                color = "red", linetype = "dashed") +
   geom_smooth(col = "blue") + 
   facet_wrap(~parameter, ncol=3) +
   labs(x="Detection Probability",
        y="Posterior mean") +
   theme_classic()+
   theme(axis.text.x = element_text(angle=45, hjust=1, size = 9), axis.text.y = element_text(size = 9), 
         axis.title = element_text(size=10),
         strip.text.x = element_text(size=7), legend.position = "none")) # # Enhance facet label readability

ggsave(filename="Beta_means_200.png",  unit="cm", width = 25, height = 16,dpi=600, scale=0.7,
       path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/")


(sd_plotB_200 <- OneCov_alldata_200 %>% 
    mutate(ModelP = factor(paste("p =", p_values[Model]), levels = paste("p =", p_values))) %>% 
    filter(parameter %in% c('beta0[1,1]', 'beta0[2,1]','beta0[3,1]','beta1[1,1]','beta1[2,1]','beta1[3,1]'))%>% 
    ggplot(aes(x = Model, y = sd)) +
    geom_point(aes(x= as.factor(ModelP), color = ifelse(rhat > 1.1, "orange", "#333333"), 
                   shape = factor(ifelse(rhat > 1.1, "High Rhat", "Normal Rhat"))), position=position_jitter(width=0.3), size=0.9) + 
    scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +        
    scale_color_identity() +
    geom_smooth(col = "blue") + 
    facet_wrap(~parameter, scales = "free") +
    labs(
      x = "Detection probability", y = "Posterior standard deviation ") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "none"))

### save
ggsave(filename="Beta_SD_200.png", unit="cm", width = 25, height = 16,dpi=600, scale=0.7,
       path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/")


###combined mean and SD
combined_plot_200_P <- (mean_plotP_200 / sd_plotP_200) + plot_annotation(tag_levels = "a")
ggsave(combined_plot_200_P, filename="mean+SD_200_P.png", unit="cm", width = 30, height = 36,dpi=600, scale=0.7,
       path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/4stateonecov/")

combined_plot_200_betas <- (mean_plotB_200 / sd_plotB_200) + plot_annotation(tag_levels = "a")
ggsave(combined_plot_200_betas, filename="mean+SD_200_betas.png", unit="cm", width = 30, height = 36,dpi=600, scale=0.7,
 
             path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/4stateonecov/200s")
#### Model diagnostics #### ####
#### For nested list ####

process_model <- function(model) {
  # Initialize an empty dataframe to collect all simulations data
  all_sims_data <- data.frame()
  
  # Loop through each simulation in the model
  for (sim in model) {
    # Extract the summary list, which contains the statistics
    sim_data <- sim$summary
    
    # Convert the list to a dataframe
    sim_df <- data.frame(
      parameter = sim_data$parameter,
      mean = sim_data$mean,
      sd = sim_data$sd,
      lower = sim_data$lower,
      upper = sim_data$upper,
      truth = sim_data$truth
    )
    
    # Bind this simulation's data to the main dataframe
    all_sims_data <- rbind(all_sims_data, sim_df)
  }
  
  # Now, calculate statistics for each parameter
  statistics_df <- all_sims_data %>%
    group_by(parameter) %>%
    summarise(
      RMSE = sqrt(mean((mean - truth)^2)),
      Bias = mean(mean - truth),
      mean_mean = mean(mean),  # Mean of the means
      mean_sd = mean(sd),
      mean_lower = mean(lower),  # Mean of the lower confidence limits
      mean_upper = mean(upper), 
      Coverage = mean(lower <= truth & upper >= truth),# Mean of the standard deviation
      cov = (mean_sd / mean_mean) * 100,
      truth = mean(truth),
      .groups = 'drop'
    )
  
  return(statistics_df)
}

## 60sites
results2 <- lapply(summary_results_list[[1]][[1]][[1]][[1]][[1]], process_model) 
## 200sites
results2 <- lapply(model_simulations[[1]], process_model)


###### for big data frame ####
process_models <- function(all_sims_data) {
  # Assuming all_sims_data is already a data frame with all models' simulations included.
  
  # Calculate statistics for each parameter across all models
  statistics_df <- all_sims_data %>%
    group_by(parameter) %>%
    summarise(
      RMSE = sqrt(mean((mean - truth)^2)),
      Bias = mean(mean - truth),
      mean_mean = mean(mean),  # Average of the mean estimates
      mean_sd = mean(sd),      # Average standard deviation
      mean_lower = mean(lower),  # Average of the lower confidence limits
      mean_upper = mean(upper),  # Average of the upper confidence limits
      Coverage = mean(lower <= truth & upper >= truth),  # Proportion of times the truth is within the CI
      cov = (mean_sd / mean_mean) * 100,  # Coefficient of Variation
      truth = mean(truth),  # Average of the true values
      .groups = 'drop'
    )
  
  return(statistics_df)
}

# Example usage
# Assuming 'Model_1_Survey_n35_Sim' is your dataframe for all simulations
results2 <- lapply(all_model_data, process_models)
# Combine all model data frames into one with an identifier for each model
all_models_df <- bind_rows(lapply(seq_along(results2), function(i) {
  cbind(Model = paste("Model", i), results2[[i]])
}), .id = "ModelID")
p_values <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9)

all_models_df %>% 
  mutate(ModelP = factor(paste("p =", p_values[as.numeric(ModelID)]), levels = paste("p =", p_values))) %>% 
  filter(!parameter %in% c('deviance', "mean.psi", "mean.R1", 
                                           "mean.R2", "n.occ[1]", "n.occ[2]", 
                                           "n.occ[3]", "n.occ[4]", 'mean.Omega1', 'mean.Omega2', 'mean.Omega3', 'mean.Omega4')) %>% 
ggplot(aes(x=ModelP, y= mean_mean, group = parameter)) +
  geom_line(color="black", linewidth = 0.8) +  
  geom_line(aes(y= mean_lower),color="black", linetype = "dashed") +  # Using color to differentiate models
  geom_line(aes(y= mean_upper),color="black", linetype = "dashed") + #
  geom_errorbar(aes(ymin=truth, ymax=truth), linetype="dashed", color="#ff0000", linewidth=0.8)+
  facet_wrap(~parameter, scales= 'free_y', ncol=3) +  # Create a separate plot for each parameter
  labs(x="Detection Probability",
       y="Mean posterior estimates") +
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 9), axis.text.y = element_text(size = 9), 
        axis.title = element_text(size=10),
        strip.text.x = element_text(size=7), legend.position = "none")

ggsave(filename="Mean_Mean_oneCov.png", unit="cm", width = 25, height = 20,dpi=600, scale=0.7,
       path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/4stateonecov/")




metrics <- c("RMSE", "cov", "Coverage", "Bias")
metric <- metrics[[3]]
for (metric in metrics) { 

all_models_df %>% 
  mutate(ModelP = factor(paste("p =", p_values[as.numeric(ModelID)]), levels = paste("p =", p_values))) %>% 
  filter(!parameter %in% c('deviance', "mean.psi", "mean.R1", 
                           "mean.R2", "n.occ[1]", "n.occ[2]", 
                           "n.occ[3]", "n.occ[4]", 'mean.Omega1', 'mean.Omega2', 'mean.Omega3', 'mean.Omega4')) %>% 
  ggplot(aes(x=ModelP, y= !!sym(metric), group = parameter)) +
  geom_col(color="black") +  
  facet_wrap(~parameter, scales= 'free_y', ncol=3) +  # Create a separate plot for each parameter
  labs(x="Detection Probability",
       y=paste0(metric)) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 9), axis.text.y = element_text(size = 9), 
        axis.title = element_text(size=10),
        strip.text.x = element_text(size=7), legend.position = "none")


ggsave(filename=paste0(metric,"_oneCov_200.png"), unit="cm", width = 25, height = 16,dpi=600, scale=0.7,
       path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/4stateonecov/")


}

