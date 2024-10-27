## WorkingDirectory = MultiStateOccupancyModelsPowerAnalysis - RepositoryFolder

library(cowplot)
library(tidyverse)
source("R-Code/Figures/ProcessFiles.r")
source("R-Code/Figures/CleanList.r")
Multistatefile <- "PosteriorEstimates/NsurveyScenarios/MultiStateModels"
basicfile <- "PosteriorEstimates/NsurveyScenarios/BasicModels"
basicfile_p0.005 <- "PosteriorEstimates/NsurveyScenarios/BasicModels/p0.005"
# Function to process each file set (for multistate and singlestate)



# Process multistate and singlestate files
multistate_files <- list.files(path = Multistatefile, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
basic_files <- list.files(path = basicfile, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
basic_p0.005_files <- list.files(path = basicfile_p0.005, pattern = "^SummaryResults.*\\.rds$", full.names = TRUE)
# Process the files and add 'multistate' or 'singlestate' to differentiate
multistate_data <- process_files(multistate_files, "Multi-state")
#The Basic Model dataset contains 1 faulty model (p0.005 is missing and replaced with 0.05) 
#The following code fixed this error
Basic_data <- process_files(basic_files, "Basic") 
basic_clean <- Basic_data  %>%
  filter(model != 2)
basic_p0.005_data <- process_files(basic_p0.005_files, "Basic")
basic_p0.005_data$model[basic_p0.005_data$model == 1] <- 2

# Combine both datasets
combined_data <- bind_rows(multistate_data, basic_clean, basic_p0.005_data)

# Specify the parameters you're interested in for multistate and singlestate

target_parameters <- c( "p","p22","psi")#"psi", "R1", "R2")
survey_levels <- c("n35", "n70", "n140", "n280")  # Surveys ordered from n35 to n280
# Filter the data for the 4 surveys and the target parameters
filtered_data <- combined_data %>%
  filter(Survey %in% c("n35", "n70", "n140", "n280"), 
         parameter %in% target_parameters)

# Define the desired order for the parameters
parameter_levels <- c( "p","p22","psi",  "R1", "R2")

p_values <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9)
# Modify the filtered_data to use the factor levels for parameter
filtered_data <- filtered_data %>%
  mutate(parameter = factor(parameter, levels = parameter_levels))

# Extract the necessary columns and create a new column for the threshold values
flatline_df <- filtered_data %>%
  select(Survey, parameter, ModelType) %>%  # Select relevant columns
  distinct() %>%  # Keep distinct combinations of Survey, parameter, and ModelType
  mutate(threshold_value = c(0.2, 0.2, 0.2, 0.1, 0.1, 0.05, 0.05, 0.001, 0.01, 0.1, 0.005, 0.1, 0.001, 0.1, 0.001, 0.05))  # Add an empty column for threshold values
flatline_df <- flatline_df %>%
  mutate(parameter = case_when(
    parameter == "psi_Basic" & ModelType == "Basic" ~ "psi",
    parameter == "psi_Multi-state" & ModelType == "Multi-state" ~ "psi",
    TRUE ~ parameter  # Keep all other parameters unchanged
  ))
# Merge the flatline points into your filtered data
filtered_data <- filtered_data %>%
  left_join(flatline_df, by = c("Survey", "parameter", 'ModelType'))

# Plot with dynamic xintercepts based on threshold_value
(NoPointsWithLines <- filtered_data %>%
  mutate(ModelP = factor(paste("", p_values[as.numeric(model)]), levels = paste("", p_values))) %>%
  mutate(parameter = factor(parameter, levels = parameter_levels)) %>% 
  mutate(Survey = factor(Survey, levels = survey_levels)) %>%
  mutate(xintercept = sapply(threshold_value, function(tv) which(p_values == tv))) %>% 
  mutate(decimal_count = sapply(threshold_value, nchar),
         x_offset = decimal_count*0.38+1.7 ) %>%  # Multiply by a constant to control the offset
  ggplot(aes(x = ModelP, y = mean)) +
    #geom_boxplot()+
  #geom_point(aes(shape = factor(ifelse(rhat > 1.1, "High Rhat", "Normal Rhat"))), 
             #color = "black",  
             #position = position_jitter(width = 0.3), size = 0.9) + 
  scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +        
  geom_errorbar(aes(ymin = truth, ymax = truth), linetype = "dashed", color = "#ff0000", linewidth = 1) +
  geom_vline(aes(xintercept = xintercept), linetype = "dashed", color = "black", size=0.7) +
   #Add text with threshold value and adjust based on the number of decimal places
  geom_text(aes(x = xintercept + x_offset, y = 0.95, label = paste("p =", threshold_value)), 
            color = "black", size = 4, angle = 0, hjust = 1) +  
  facet_grid(Survey ~ parameter + ModelType) +  # Facet by Survey, Parameter, and ModelType
  geom_smooth(aes(x = as.numeric(ModelP)), color = "black", size = 1, se = FALSE) +  # Smooth line for mean
  geom_smooth(aes(x = as.numeric(ModelP), y = upper), color = "grey", linetype = "dashed", size = 1, se = FALSE) +  # Upper smooth line, light blue dashed
  geom_smooth(aes(x = as.numeric(ModelP), y = lower), color = "grey", linetype = "dashed", size = 1, se = FALSE) +  # Lower smooth line, light blue dashed
  coord_cartesian(ylim = c(0, 1)) +
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
  mutate(parameter = factor(parameter, levels = parameter_levels))


############## Appendix
target_parameters <- c("psi", "R1", "R2")
parameter_levels <- c("psi", "R1", "R2")
parameter_levels <- c("p","p1", "p21", "p31", "p22", "p32", "p33")
target_parameters <- c("p","p1", "p21", "p31", "p22", "p32", "p33","psi", "R1", "R2") 

parameter_levels_p <- c("p","p1","p22","p33","p21","p31","p32")
# Plot with dynamic xintercepts based on threshold_value
(NoPointsWithLines <- combined_data %>%
    filter(parameter %in% c("p","p1","p22","p33","p21","p31","p32")) %>% 
    #filter(parameter %in% c("psi", "R1", "R2")) %>% 
    mutate(ModelP = factor(paste("", p_values[as.numeric(model)]), levels = paste("", p_values))) %>%
    mutate(parameter = factor(parameter, levels = parameter_levels_p)) %>% 
    mutate(Survey = factor(Survey, levels = survey_levels)) %>%
    ggplot(aes(x = ModelP, y = mean)) +
    #geom_boxplot()+
    geom_point(aes(x= as.factor(ModelP), color = ifelse(rhat > 1.1, "orange", "#333333"), 
                   shape = factor(ifelse(rhat > 1.1, "High Rhat", "Normal Rhat"))), position=position_jitter(width=0.3), size=0.9) + 
    scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +
    scale_color_identity() +
    geom_errorbar(aes(ymin = truth, ymax = truth), linetype = "dashed", color = "#ff0000", linewidth = 1) +
    facet_grid(Survey ~ parameter + ModelType) +  # Facet by Survey, Parameter, and ModelType
    geom_smooth(aes(x = as.numeric(ModelP)), color = "blue", size = 1, se = FALSE) +  # Smooth line for mean
    coord_cartesian(ylim = c(0, 1)) +
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


ggsave(filename="AppendixS1FigureS2.png", unit="cm", width = 25, height = 16,dpi=600, scale=1,
       path ="Outputs/NsurveySimulation/Graph/")
