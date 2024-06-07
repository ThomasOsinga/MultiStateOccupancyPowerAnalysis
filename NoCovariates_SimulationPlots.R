model_simulations <- list()
model_simulations[[1]] <- readRDS('C:/Users/Thoma/OneDrive - Sveriges lantbruksuniversitet/Thesis documents/Multi_state_occu/detection_Simulations_1.rds')
model_simulations[[2]] <- readRDS('C:/Users/Thoma/OneDrive - Sveriges lantbruksuniversitet/Thesis documents/Multi_state_occu/detection_Simulations_2.rds')

lapply(model_simulations, function(x) lapply(x, length))  # Should return the number of simulations per model in each sublist

# Combine the simulations for corresponding models
combined_simulations <- lapply(1:length(model_simulations[[1]]), function(i) {
  # Combine the simulations from each list for the ith model
  c(model_simulations[[1]][[i]], model_simulations[[2]][[i]])
})

library(tidyverse)
library(patchwork)
######### Make 1 big list
model_simulation <- combined_simulations
all_summaries <- lapply(seq_along(model_simulation), function(model_index) {
  lapply(model_simulation[[model_index]], function(summary_df) {
    summary_df$model <- model_index
    return(summary_df)
  })
})

all_summaries <- do.call(rbind, do.call(rbind, all_summaries))
p_values <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9)
  # Filter data for the current parameter using the correct scalar value in the loop
  (mean_plot <-  all_summaries %>% 
    mutate(ModelP = factor(paste("p =", p_values[as.numeric(model)]), levels = paste("p =", p_values))) %>% 
    filter(!parameter %in% c('deviance', "mean.psi", "mean.R1", 
                             "mean.R2", "n.occ[1]", "n.occ[2]", 
                             "n.occ[3]", "n.occ[4]", 'Theta[1,1]', 'Theta[1,2]',
                             'Theta[1,3]', 'Theta[1,4]', 'Theta[2,1]', 'Theta[2,2]', 
                             'Theta[2,3]', 'Theta[2,4]', 'Theta[3,1]', 'Theta[3,2]', 
                             'Theta[3,3]', 'Theta[3,4]', 'Theta[4,1]', 'Theta[4,2]'
                             , 'Theta[4,3]', 'Theta[4,4]', 'Omega[1]', 'Omega[2]', 'Omega[3]', 'Omega[4]')) %>% 
  ggplot(aes(x = model , y = mean)) +
    geom_point(aes(x= as.factor(ModelP), color = ifelse(rhat > 1.1, "orange", "#333333"), 
                   shape = factor(ifelse(rhat > 1.1, "High Rhat", "Normal Rhat"))), position=position_jitter(width=0.3), size=0.9) + 
    scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +        
    #geom_boxplot(aes(x= as.factor(model))) +
    geom_errorbar(aes(ymin=truth, ymax=truth), linetype="dashed", color="#ff0000", linewidth=1)+
    scale_color_identity() +
    #geom_vline(xintercept = 5, linetype="dashed")+
    geom_smooth(color="#0073e6", size=1)+
    facet_wrap(~parameter)+
    coord_cartesian(ylim = c(0, 1)) +
    labs(x="Detection Probability",
         y="Posterior mean") +
    theme_classic()+
    theme(axis.text.x = element_text(angle=45, hjust=1, size = 9), axis.text.y = element_text(size = 9), 
          axis.title = element_text(size=10),
          strip.text.x = element_text(size=7), legend.position = "none")) # Enhance facet label readability
    
  ggsave(filename="4StateNoCovs.Mean.png", unit="cm", width = 25, height = 16,dpi=600, scale=0.7,
         path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/4statenocov/")
  
   
  # Create boxplot for the current parameter
  p_values <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9)
  # Filter data for the current parameter using the correct scalar value in the loop
  (sd_plot <-  all_summaries %>% 
      mutate(ModelP = factor(paste("p =", p_values[as.numeric(model)]), levels = paste("p =", p_values))) %>% 
      filter(!parameter %in% c('deviance', "mean.psi", "mean.R1", 
                               "mean.R2", "n.occ[1]", "n.occ[2]", 
                               "n.occ[3]", "n.occ[4]", 'Theta[1,1]', 'Theta[1,2]',
                               'Theta[1,3]', 'Theta[1,4]', 'Theta[2,1]', 'Theta[2,2]', 
                               'Theta[2,3]', 'Theta[2,4]', 'Theta[3,1]', 'Theta[3,2]', 
                               'Theta[3,3]', 'Theta[3,4]', 'Theta[4,1]', 'Theta[4,2]'
                               , 'Theta[4,3]', 'Theta[4,4]', 'Omega[1]', 'Omega[2]', 'Omega[3]', 'Omega[4]')) %>% 
      ggplot(aes(x = model , y = sd)) +
      geom_point(aes(x= as.factor(ModelP), color = ifelse(rhat > 1.1, "orange", "#333333"), 
                     shape = factor(ifelse(rhat > 1.1, "High Rhat", "Normal Rhat"))), position=position_jitter(width=0.3), size=0.9) + 
      scale_shape_manual(values = c("Normal Rhat" = 1, "High Rhat" = 2)) +        
      #geom_boxplot(aes(x= as.factor(model))) +
      #geom_errorbar(aes(ymin=truth, ymax=truth), linetype="dashed", color="#ff0000", width=1)+
      scale_color_identity() +
      #geom_vline(xintercept = 5, linetype="dashed")+
      geom_smooth(color="#0073e6", size=1)+
      facet_wrap(~parameter, ncol=3)+
     # geom_text("a)")+
      coord_cartesian(ylim = c(0, 1)) +
      labs(x="Detection Probability",
           y="Posterior standard deviation") +
      theme_classic()+
      theme(axis.text.x = element_text(angle=45, hjust=1, size = 9), axis.text.y = element_text(size = 9), 
            axis.title = element_text(size=10),
            strip.text.x = element_text(size=7), legend.position = "none")) # Enhance facet label readability
  
  ggsave(filename="4StateNoCovs.SD.png", unit="cm", width = 25, height = 16,dpi=600, scale=0.7,
         path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/4statenocov/")
  
  
  combined_plot <- (mean_plot / sd_plot) + plot_annotation(tag_levels = "a")
  
  ggsave(combined_plot, filename="mean+SD.png", unit="cm", width = 30, height = 36,dpi=600, scale=0.7,
         path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/4statenocov/")
  
######### Model diagnistics

  # Example function to process each model
  process_model <- function(model) {
    # Initialize an empty dataframe to collect all simulations data
    all_sims_data <- data.frame()
    
    # Loop through each simulation in the model
    for (sim in model) {
      # Extract the summary list, which contains the statistics
      sim_data <- sim
      
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
        truth = mean(truth),
        cov = (mean_sd / mean_mean) * 100,
        .groups = 'drop'
      )
    
    return(statistics_df)
  }
  
  results2 <- lapply(combined_simulations, process_model)

  # Combine all model data frames into one with an identifier for each model
  all_models_df <- bind_rows(lapply(seq_along(results2), function(i) {
    cbind(Model = paste("Model", i), results2[[i]])
  }), .id = "ModelID")

  
  # Plot RMSE for the specific parameter across models
 
  p_values <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9)
  all_models_df %>% 
    mutate(ModelP = factor(paste("p =", p_values[as.numeric(ModelID)]), levels = paste("p =", p_values))) %>% 
    filter(!parameter %in% c('deviance', "mean.psi", "mean.R1", 
                        "mean.R2", "n.occ[1]", "n.occ[2]", 
                        "n.occ[3]", "n.occ[4]", 'Theta[1,1]', 'Theta[1,2]',
                        'Theta[1,3]', 'Theta[1,4]', 'Theta[2,1]', 'Theta[2,2]', 
                        'Theta[2,3]', 'Theta[2,4]', 'Theta[3,1]', 'Theta[3,2]', 
                        'Theta[3,3]', 'Theta[3,4]', 'Theta[4,1]', 'Theta[4,2]'
                        , 'Theta[4,3]', 'Theta[4,4]', 'Omega[1]', 'Omega[2]', 'Omega[3]', 'Omega[4]')) %>% 
    ggplot(aes(x=ModelP, y= mean_mean, group = parameter)) +
    #geom_col(aes(y= cov),color="black",) +  # Using color to differentiate models
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
  
  ggsave(filename="Mean_Mean_NoCovs.png", unit="cm", width = 25, height = 16,dpi=600, scale=0.7,
         path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/4statenocov/")

  
  
  
  

  library(ggplot2)
  library(patchwork)
  
  metrics <- c("RMSE", "cov", "Coverage", "Bias")
  
  for (metric in metrics) { 
    all_models_df %>% 
      mutate(ModelP = factor(paste("p =", p_values[as.numeric(ModelID)]), levels = paste("p =", p_values))) %>% 
      filter(!parameter %in% c('deviance', "mean.psi", "mean.R1", 
                               "mean.R2", "n.occ[1]", "n.occ[2]", 
                               "n.occ[3]", "n.occ[4]", 'Theta[1,1]', 'Theta[1,2]',
                               'Theta[1,3]', 'Theta[1,4]', 'Theta[2,1]', 'Theta[2,2]', 
                               'Theta[2,3]', 'Theta[2,4]', 'Theta[3,1]', 'Theta[3,2]', 
                               'Theta[3,3]', 'Theta[3,4]', 'Theta[4,1]', 'Theta[4,2]'
                               , 'Theta[4,3]', 'Theta[4,4]', 'Omega[1]', 'Omega[2]', 'Omega[3]', 'Omega[4]')) %>% 
      ggplot(aes(x=ModelP, y= !!sym(metric), group = parameter)) +
      geom_col(color="black") +  
      #geom_line(aes(y= mean_lower),color="black", linetype = "dashed") +  # Using color to differentiate models
      #geom_line(aes(y= mean_upper),color="black", linetype = "dashed") + #
      #geom_errorbar(aes(ymin=truth, ymax=truth), linetype="dashed", color="#ff0000", linewidth=0.8)+
      facet_wrap(~parameter, scales= 'free_y', ncol=3) +  # Create a separate plot for each parameter
      labs(x="Detection Probability",
           y=paste0(metric)) +
      theme_classic()+
      theme(axis.text.x = element_text(angle=45, hjust=1, size = 9), axis.text.y = element_text(size = 9), 
            axis.title = element_text(size=10),
            strip.text.x = element_text(size=7), legend.position = "none")
    
    
    ggsave(filename=paste0(metric,"_noCov.png"), unit="cm", width = 25, height = 16,dpi=600, scale=0.7,
           path ="C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Report/Results/4statenocov/")
    
    
  }
  