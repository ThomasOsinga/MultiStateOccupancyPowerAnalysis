
library(tidyverse)

setwd("C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/")

##### No Covariates
readRDS("CaseStudyResults/Weekly_Moose_100k.rds")
# Define file paths
file_paths_4StateNoCovs <- list(
  ree_daily_nocovs = "CaseStudyResults/Ree_100k.rds",
  ree_weekly_nocovs = "CaseStudyResults/Weekly_Ree_100k.rds",
  moose_daily_nocovs = "CaseStudyResults/Moose_100k.rds",
  moose_weekly_nocovs = "CaseStudyResults/Weekly_Moose_100k.rds"
)

# Define names to update and new names
names_to_update <- c("Omega[1]", "Omega[2]", "Omega[3]", "Omega[4]")
new_names <- c("Unoccupied", "Occupied w/o Calves", "Occupied w/ Calf", "Occupied w/ >1 calf")

# Function to load data, update names, and create combined plot
process_and_plot_combined <- function(daily_path, weekly_path, plot_title, y_limits) {
  daily_data <- readRDS(daily_path)
  weekly_data <- readRDS(weekly_path)
  
  daily_data <- as.data.frame(daily_data)
  weekly_data <- as.data.frame(weekly_data)
  
  daily_data$parameter <- rownames(daily_data)
  weekly_data$parameter <- rownames(weekly_data)
  
  # # Replace old names with new descriptive names
  # for (i in seq_along(names_to_update)) {
  #   daily_data$parameter[daily_data$parameter == names_to_update[i]] <- new_names[i]
  #   weekly_data$parameter[weekly_data$parameter == names_to_update[i]] <- new_names[i]
  # }
  # 
  daily_data$parameter <- factor(daily_data$parameter, levels = unique(daily_data$parameter))
  weekly_data$parameter <- factor(weekly_data$parameter, levels = unique(weekly_data$parameter))
  
  daily_data$`Survey Length` <- "Daily"
  weekly_data$`Survey Length` <- "Weekly"
  
  combined_data <- rbind(daily_data, weekly_data)
  
  # Create combined plot
  plot <- combined_data %>%
    filter(!parameter %in% c('Omega[1]', 'Omega[2]','Omega[3]','Omega[4]', 'deviance', "n.occ[1]", "n.occ[2]", "n.occ[3]", "n.occ[4]")) %>%
    ggplot(aes(x = parameter, color = `Survey Length`, shape = `Survey Length`)) +
    geom_point(aes(y = mean), position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`, color = `Survey Length`), width = 0.2, position = position_dodge(width = 0.5)) +
    coord_cartesian(ylim = y_limits) +
    scale_color_manual(values = c("Daily" = "black", "Weekly" = "darkred")) +
    scale_color_manual(values = c("black", "darkred")) +
    labs(y = "Posterior mean", x = "Parameter") +
    theme_classic()+
    theme(axis.text.x = element_text(angle=55, hjust=1, size = 9), axis.text.y = element_text(size = 9), 
          axis.title = element_text(size=10),
          strip.text.x = element_text(size=7), legend.position = "none")
  
  ggsave(filename = paste0("CaseStudyResults/", plot_title, ".png"), plot = plot)
  
  return(plot)
}

# Loop through each pair of daily and weekly file paths and create combined plots with appropriate y-axis limits
file_pairs <- list(
  ree = list(daily = file_paths_4StateNoCovs$ree_daily_nocovs, weekly = file_paths_4StateNoCovs$ree_weekly_nocovs),
  moose = list(daily = file_paths_4StateNoCovs$moose_daily_nocovs, weekly = file_paths_4StateNoCovs$moose_weekly_nocovs)
)

for (name in names(file_pairs)) {
  y_limits <- c(0, 1)
  plot <- process_and_plot_combined(file_pairs[[name]]$daily, file_pairs[[name]]$weekly, paste(name, "No Covariates"), y_limits)
  assign(paste0(name, "_plot"), plot)
}

# Combine the plots using patchwork
combined_plot <- (
  (moose_plot | ree_plot)
) + 
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(tag_levels = list(c("Moose", "Roe deer"))) &
  theme(legend.position = "bottom")

  
ggsave(filename = "CaseStudyResults/Combined_Plots_4statesnocovs.png", plot = combined_plot , width = 26, height = 12, scale=0.4, dpi=600)

#### Covariates

# Load required libraries
library(dplyr)
library(ggplot2)

# Define file paths
file_paths_4StateCovs <- list(
  ree_daily_covs = "CaseStudyResults/Ree_100k_covs.rds",
  ree_weekly_covs = "CaseStudyResults/Ree_100k_covs.rds",
  moose_daily_covs = "CaseStudyResults/Moose_100k_covs.rds",
  moose_weekly_covs = "CaseStudyResults/Weekly_Moose_100k_covs.rds"
)

# Function to load data, update names, and create combined plots
process_and_plot_combined <- function(daily_path, weekly_path, plot_title) {
  daily_data <- readRDS(daily_path)
  weekly_data <- readRDS(weekly_path)
  
  daily_data <- as.data.frame(daily_data)
  weekly_data <- as.data.frame(weekly_data)
  
  daily_data$parameter <- rownames(daily_data)
  weekly_data$parameter <- rownames(weekly_data)
  
  daily_data$parameter <- factor(daily_data$parameter, levels = unique(daily_data$parameter))
  weekly_data$parameter <- factor(weekly_data$parameter, levels = unique(weekly_data$parameter))
  
  daily_data$`Survey Length` <- "Daily"
  weekly_data$`Survey Length` <- "Weekly"
  
  combined_data <- rbind(daily_data, weekly_data)
  
  # Separate beta and p parameters
  beta_data <- combined_data %>% filter(grepl("^beta", parameter))
  p_data <- combined_data %>% filter(grepl("^p|psi|R1|R2", parameter))
  
  # Create combined beta plot
  beta_plot <- beta_data %>%
    ggplot(aes(x = parameter, color = `Survey Length`, shape = `Survey Length`)) +
    geom_point(aes(y = mean), position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0.2, position = position_dodge(width = 0.5), color = "black") +

    scale_color_manual(values = c("black", "darkred")) +
    labs(y = "Posterior mean", x = "Parameter") +
    theme_classic()+
    theme(axis.text.x = element_text(angle=55, hjust=1, size = 9), axis.text.y = element_text(size = 9), 
          axis.title = element_text(size=10),
          strip.text.x = element_text(size=7), legend.position = "none") # # Enhance facet label readability
  
  ggsave(filename = paste0("CaseStudyResults/", plot_title, "_Beta.png"), plot = beta_plot)
  
  # Create combined p plot
  p_plot <- p_data %>%
    ggplot(aes(x = parameter, color = `Survey Length`, shape = `Survey Length`)) +
    geom_point(aes(y = mean), position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0.2, position = position_dodge(width = 0.5), color = "black") +
    coord_cartesian(ylim = c(0, 1)) +

    scale_color_manual(values = c("black", "darkred")) +
    labs(y = "Posterior mean", x = "Parameter") +
    theme_classic()+
    theme(axis.text.x = element_text(angle=55, hjust=1, size = 9), axis.text.y = element_text(size = 9), 
          axis.title = element_text(size=10),
          strip.text.x = element_text(size=7), legend.position = "none") # # Enhance facet label readability
  
  ggsave(filename = paste0("CaseStudyResults/", plot_title, "_P.png"), plot = p_plot)
  
  return(list(beta_plot = beta_plot, p_plot = p_plot))
}

# Loop through each pair of daily and weekly file paths and create combined plots
file_pairs <- list(
  ree = list(daily = file_paths_4StateCovs$ree_daily_covs, weekly = file_paths_4StateCovs$ree_weekly_covs),
  moose = list(daily = file_paths_4StateCovs$moose_daily_covs, weekly = file_paths_4StateCovs$moose_weekly_covs)
)

for (name in names(file_pairs)) {
  plots <- process_and_plot_combined(file_pairs[[name]]$daily, file_pairs[[name]]$weekly, paste(name, "Covariates"))
  print(plots$beta_plot)
  print(plots$p_plot)
}











# Loop through each pair of daily and weekly file paths and create combined plots
file_pairs <- list(
  ree = list(daily = file_paths$ree_daily_covs, weekly = file_paths$ree_weekly_covs),
  moose = list(daily = file_paths$moose_daily_covs, weekly = file_paths$moose_weekly_covs)
)

for (name in names(file_pairs)) {
  plots <- process_and_plot_combined(file_pairs[[name]]$daily, file_pairs[[name]]$weekly, paste(name, "Covariates"))
  assign(paste0(name, "_beta_plot"), plots$beta_plot)
  assign(paste0(name, "_p_plot"), plots$p_plot)
}

# Combine the plots using patchwork
combined_plot <- (
  (moose_beta_plot / moose_p_plot) | (ree_beta_plot / ree_p_plot)
) + 
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(tag_levels = 'a') &
  theme(legend.position = "bottom")


# Add column titles manually using wrap_elements
moose_title <- wrap_elements(grid::textGrob("Moose", gp = grid::gpar(fontsize = 9, fontface = "bold")))
ree_title <- wrap_elements(grid::textGrob("Roe Deer", gp = grid::gpar(fontsize = 9, fontface = "bold")))
final_plot <- (moose_title / combined_plot[[1]]) + plot_layout(heights = c(1,10), guides = "collect") & 
  theme(legend.position = "none") | (ree_title / combined_plot[[2]]) + plot_layout(heights = c(1,10), guides = "collect") &  theme(legend.position = "right")





# Save the combined plot
ggsave(filename = "CaseStudyResults/Combined_Plots_4statescovs.png", plot = final_plot, width = 16, height = 12, scale=0.5, dpi=600)





#######################################################
######################################################
################# 3 states #######################
#######################################################
######################################################
readRDS(moose_weekly_nocovs)
##### No Covariates
# Define file paths
file_paths_3StateNoCovs <- list(
  ree_daily_nocovs = "CaseStudyResults/Ree_D_3State.rds",
  ree_weekly_nocovs = "CaseStudyResults/Ree_W_3State.rds",
  moose_daily_nocovs = "CaseStudyResults/Moose_D_3State.rds",
  moose_weekly_nocovs = "CaseStudyResults/Moose_W_3State.rds"
)

# Define names to update and new names
names_to_update <- c("Omega[1]", "Omega[2]", "Omega[3]")
new_names <- c("Unoccupied", "Occupied w/o Calves", "Occupied w/ Calf")

# Function to load data, update names, and create combined plot
process_and_plot_combined <- function(daily_path, weekly_path, plot_title, y_limits) {
  daily_data <- readRDS(daily_path)
  weekly_data <- readRDS(weekly_path)
  
  daily_data <- as.data.frame(daily_data)
  weekly_data <- as.data.frame(weekly_data)
  
  daily_data$parameter <- rownames(daily_data)
  weekly_data$parameter <- rownames(weekly_data)
  
  # # Replace old names with new descriptive names
  # for (i in seq_along(names_to_update)) {
  #   daily_data$parameter[daily_data$parameter == names_to_update[i]] <- new_names[i]
  #   weekly_data$parameter[weekly_data$parameter == names_to_update[i]] <- new_names[i]
  # }
  # 
  daily_data$parameter <- factor(daily_data$parameter, levels = unique(daily_data$parameter))
  weekly_data$parameter <- factor(weekly_data$parameter, levels = unique(weekly_data$parameter))
  
  daily_data$`Survey Length` <- "Daily"
  weekly_data$`Survey Length` <- "Weekly"
  
  combined_data <- rbind(daily_data, weekly_data)
  
  # Create combined plot
  plot <- combined_data %>%
    filter(!parameter %in% c('Omega[1]', 'Omega[2]','Omega[3]', 'deviance', "n.occ[1]", "n.occ[2]", "n.occ[3]", "n.occ[4]")) %>%
    ggplot(aes(x = parameter, color = `Survey Length`, shape = `Survey Length`)) +
    geom_point(aes(y = mean), position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`, color = `Survey Length`), width = 0.2, position = position_dodge(width = 0.5)) +
    coord_cartesian(ylim = y_limits) +
    scale_color_manual(values = c("Daily" = "black", "Weekly" = "darkred")) +
    scale_color_manual(values = c("black", "darkred")) +
    labs(y = "Posterior mean", x = "Parameter") +
    theme_classic()+
    theme(axis.text.x = element_text(angle=55, hjust=1, size = 9), axis.text.y = element_text(size = 9), 
          axis.title = element_text(size=10),
          strip.text.x = element_text(size=7), legend.position = "none")
  
  ggsave(filename = paste0("CaseStudyResults/", plot_title, ".png"), plot = plot)
  
  return(plot)
}

# Loop through each pair of daily and weekly file paths and create combined plots with appropriate y-axis limits
file_pairs <- list(
  ree = list(daily = file_paths_3StateNoCovs$ree_daily_nocovs, weekly = file_paths_3StateNoCovs$ree_weekly_nocovs),
  moose = list(daily = file_paths_3StateNoCovs$moose_daily_nocovs, weekly = file_paths_3StateNoCovs$moose_weekly_nocovs)
)

for (name in names(file_pairs)) {
  y_limits <- c(0, 1)
  plot <- process_and_plot_combined(file_pairs[[name]]$daily, file_pairs[[name]]$weekly, paste(name, "No Covariates"), y_limits)
  assign(paste0(name, "_plot"), plot)
}

# Combine the plots using patchwork
combined_plot <- (
  (moose_plot | ree_plot)
) + 
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(tag_levels = list(c("Moose", "Roe deer"))) &
  theme(legend.position = "bottom")


ggsave(filename = "CaseStudyResults/3stateCombined_Plots_nocovs.png", plot = combined_plot , width = 26, height = 12, scale=0.3, dpi=600)

#### Covariates

# Load required libraries
library(dplyr)
library(ggplot2)
readRDS(moose_daily_covs)
# Define file paths
file_paths_3StateCovs <- list(
  ree_daily_covs = "CaseStudyResults/Ree_D_3State_covs.rds",
  ree_weekly_covs = "CaseStudyResults/Ree_W_3State_covs.rds",
  moose_daily_covs = "CaseStudyResults/Moose_D_3State_covs.rds",
  moose_weekly_covs = "CaseStudyResults/Moose_W_3State_covs.rds"
)

# Function to load data, update names, and create combined plots
process_and_plot_combined <- function(daily_path, weekly_path, plot_title) {
  daily_data <- readRDS(daily_path)
  weekly_data <- readRDS(weekly_path)
  
  daily_data <- as.data.frame(daily_data)
  weekly_data <- as.data.frame(weekly_data)
  
  daily_data$parameter <- rownames(daily_data)
  weekly_data$parameter <- rownames(weekly_data)
  
  daily_data$parameter <- factor(daily_data$parameter, levels = unique(daily_data$parameter))
  weekly_data$parameter <- factor(weekly_data$parameter, levels = unique(weekly_data$parameter))
  
  daily_data$`Survey Length` <- "Daily"
  weekly_data$`Survey Length` <- "Weekly"
  
  combined_data <- rbind(daily_data, weekly_data)
  
  # Separate beta and p parameters
  beta_data <- combined_data %>% filter(grepl("^beta", parameter))
  p_data <- combined_data %>% filter(grepl("^p11|p2", parameter))
  
  # Create combined beta plot
  beta_plot <- beta_data %>%
    ggplot(aes(x = parameter, color = `Survey Length`, shape = `Survey Length`)) +
    geom_point(aes(y = mean), position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0.2, position = position_dodge(width = 0.5), color = "black") +
    
    scale_color_manual(values = c("black", "darkred")) +
    labs(y = "Posterior mean", x = "Parameter") +
    theme_classic()+
    theme(axis.text.x = element_text(angle=55, hjust=1, size = 9), axis.text.y = element_text(size = 9), 
          axis.title = element_text(size=10),
          strip.text.x = element_text(size=7), legend.position = "none") # # Enhance facet label readability
  
  ggsave(filename = paste0("CaseStudyResults/", plot_title, "_Beta.png"), plot = beta_plot)
  
  # Create combined p plot
  p_plot <- p_data %>%
    ggplot(aes(x = parameter, color = `Survey Length`, shape = `Survey Length`)) +
    geom_point(aes(y = mean), position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0.2, position = position_dodge(width = 0.5), color = "black") +
    coord_cartesian(ylim = c(0, 1)) +
    
    scale_color_manual(values = c("black", "darkred")) +
    labs(y = "Posterior mean", x = "Parameter") +
    theme_classic()+
    theme(axis.text.x = element_text(angle=55, hjust=1, size = 9), axis.text.y = element_text(size = 9), 
          axis.title = element_text(size=10),
          strip.text.x = element_text(size=7), legend.position = "none") # # Enhance facet label readability
  
  ggsave(filename = paste0("CaseStudyResults/", plot_title, "_P.png"), plot = p_plot)
  
  return(list(beta_plot = beta_plot, p_plot = p_plot))
}

# Loop through each pair of daily and weekly file paths and create combined plots
file_pairs <- list(
  ree = list(daily = file_paths_3StateCovs$ree_daily_covs, weekly = file_paths_3StateCovs$ree_weekly_covs),
  moose = list(daily = file_paths_3StateCovs$moose_daily_covs, weekly = file_paths_3StateCovs$moose_weekly_covs)
)

for (name in names(file_pairs)) {
  plots <- process_and_plot_combined(file_pairs[[name]]$daily, file_pairs[[name]]$weekly, paste(name, "Covariates"))
  print(plots$beta_plot)
  print(plots$p_plot)
}











# Loop through each pair of daily and weekly file paths and create combined plots
file_pairs <- list(
  ree = list(daily = file_paths$ree_daily_covs, weekly = file_paths$ree_weekly_covs),
  moose = list(daily = file_paths$moose_daily_covs, weekly = file_paths$moose_weekly_covs)
)

for (name in names(file_pairs)) {
  plots <- process_and_plot_combined(file_pairs[[name]]$daily, file_pairs[[name]]$weekly, paste(name, "Covariates"))
  assign(paste0(name, "_beta_plot"), plots$beta_plot)
  assign(paste0(name, "_p_plot"), plots$p_plot)
}

# Combine the plots using patchwork
combined_plot <- (
  (moose_beta_plot / moose_p_plot) | (ree_beta_plot / ree_p_plot)
) + 
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(tag_levels = 'a') &
  theme(legend.position = "bottom")


# Add column titles manually using wrap_elements
moose_title <- wrap_elements(grid::textGrob("Moose", gp = grid::gpar(fontsize = 9, fontface = "bold")))
ree_title <- wrap_elements(grid::textGrob("Roe Deer", gp = grid::gpar(fontsize = 9, fontface = "bold")))
final_plot <- (moose_title / combined_plot[[1]]) + plot_layout(heights = c(1,10), guides = "collect") & 
  theme(legend.position = "none") | (ree_title / combined_plot[[2]]) + plot_layout(heights = c(1,10), guides = "collect") &  theme(legend.position = "right")





# Save the combined plot
ggsave(filename = "CaseStudyResults/Combined_Plots_3statescovs.png", plot = final_plot, width = 16, height = 12, scale=0.5, dpi=600)


