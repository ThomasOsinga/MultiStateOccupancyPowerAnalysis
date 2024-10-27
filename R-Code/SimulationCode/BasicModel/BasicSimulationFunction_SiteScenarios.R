#Dont run these - Only there to be called from IndexFile
generate_true_states <- function(Covariate, M, nsimulations, beta0, beta1) {
  
  for (i in 1:M) {
  psi_logit <- beta0 + beta1 * Covariate[i]
  psi[i] <- plogis(psi_logit)
  }
  
  # Generate presence/absence data (the truth)
  true_states <- matrix(nrow = M, ncol = nsimulations)
  true_psi <- matrix(nrow = M, ncol = nsimulations)
  for (sim in 1:nsimulations) {
    for (i in 1:M) {
      true_states[i, sim] <- rbinom(n = 1, size = 1, prob = psi[i])  # R has no Bernoulli
      true_psi[i, sim] <- psi[i]
    }
  }
  return(list(true_states =  true_states, mean.psi = mean(psi), true_psi = true_psi, Covariate = Covariate, beta0 = beta0, beta1 = beta1))
}


simulate_occupancy_data <- function(M = 97, J = 91, 
                                    p = 0.05,
                                    show.plot = TRUE, simulationN, SimulationParameters) {
  
  # Covariate generation
  parameters <- c("p", "mean.psi", "beta0",  "beta1")
  # Compute Omega matrix based on logistic regression of occupancy probabilities
  PSI <- SimulationParameters$true_psi
  mean.psi <- mean(PSI) 
  values <- c(p, mean.psi, SimulationParameters$beta0, SimulationParameters$beta1)
  param_df <- data.frame(parameter = parameters, truth = values)
  true.states <- SimulationParameters$true_states[,simulationN] 
  # Vectorized occupancy data generation
  prob_matrix <- matrix(rep(true.states * p, J), nrow = M, byrow = FALSE)
  y_simulated <- matrix(rbinom(M * J, 1, prob_matrix), nrow = M, ncol = J)
  
  # Optionally plot results
  if (show.plot) {
    par(mfrow = c(1, 1))
    plot(Covariate, PSI, main = "Psi  vs Simulated Covariate", xlab = "Simulated Covariate", ylab = "Probability", ylim=c(0,1))
  }
  
  x <- list(true.states = true.states, y = y_simulated, mean.psi = mean.psi, Covariate = Covariate, p = p, PSI = PSI, nsites=M, nsurveys=J, param_df = param_df)
  return(x)
}




run_and_store_models <- function(nsurveyModels, nDetecmodels, nSims, bdata_list, 
                                 param_df, model_file, inits, 
                                 params_to_monitor, n.adapt = 3000, n.chains = 3,
                                 n.iter= 20000, n.burnin=5000, n.thin=3, parallel=TRUE, model=FALSE) {
  
  # Initialize storage structure for models and summaries
  simulations <- vector("list", nsurveyModels)
  for (survey_index in seq_len(nsurveyModels)) {
    simulations[[survey_index]] <- vector("list", nDetecmodels)
    for (detection_index in seq_len(nDetecmodels)) {
      simulations[[survey_index]][[detection_index]] <- vector("list", nSims)
    }
  }
  # Loop through each model configuration
  for (survey_index in 1:nsurveyModels) {
    for (detection_index in 1:nDetecmodels){
      # Loop through each simulation for the current model
      for (simulation_index in 1:nSims) {
        tic("sleeping")
        print("falling asleep...")
        # Setup data for the current model
        current_data <- bdata_list[[survey_index]][[detection_index]][[simulation_index]]
        
        # Run JAGS model
        jags.model <- jags(current_data, inits = inits, parameters.to.save = params_to_monitor,
                           model.file = model_file, n.adapt = n.adapt, n.chains = n.chains, n.iter = n.iter,
                           n.burnin = n.burnin, n.thin = n.thin, parallel = parallel)
        
        # Store the model output in the structure
        # simulations[[survey_index]][[detection_index]][[simulation_index]] <- jags.model
        if (model == TRUE) {
          simulations_models <- jags.model
        }
        else {
          simulations_models <- NULL
        }
        # Optionally, summarize and store model results
        simulation_summary <- as.data.frame(summary(jags.model))
        # Append additional statistics if needed, such as truth values matching
        truth_values <- param_df[[survey_index]][[detection_index]][[simulation_index]][[1]]$truth[match(rownames(simulation_summary), param_df[[survey_index]][[detection_index]][[simulation_index]][[1]]$parameter)]
        
        simulation_summary$truth <- truth_values
        
        # Store summarized results as well (or separately if required)
        simulations[[survey_index]][[detection_index]][[simulation_index]] <- list(
          summary = data.frame(
            parameter = rownames(simulation_summary),
            mean = simulation_summary[,"mean"],
            sd = simulation_summary[,"sd"],
            lower = simulation_summary[,"2.5%"],
            upper = simulation_summary[,"97.5%"],
            rhat = simulation_summary[,'Rhat'],
            truth = truth_values
          ), simulations_models
        )
        print("...waking up")
        toc()
      }
    }
  }
  
  return(simulations)
  
}

run_complete_simulation <- function(nsurveysvec, p,
                                    nsimulation, n.adapt, n.iter, n.burnin, n.thin, n.chains,
                                    parallel=TRUE, model_file, M, Covariate = Covariate, beta0, beta1) {
  
  # Stores the true states for all sites across all simulations
  SimulationParameters <- generate_true_states(M,nsimulation, beta0, beta1,  Covariate = Covariate)
  
  # Initialize storage structures
  bdata <- vector("list", length(nsurveysvec))
  params_df <- vector("list", length(nsurveysvec))
  psi <- vector("list", length(nsurveysvec))
  
  # Set up simulations based on survey vectors and detection probabilities
  # Set up simulations based on survey vectors and detection probabilities
  for (j in seq_along(nsurveysvec)) {
    bdata[[j]] <- vector("list", length(p))
    params_df[[j]] <- vector("list", length(p))
    psi[[j]] <- vector("list", length(p))
    
    for (i in seq_along(p)) {
      bdata[[j]][[i]] <- vector("list", nsimulation)
      params_df[[j]][[i]] <- vector("list", nsimulation)
      psi[[j]][[i]] <- vector("list", nsimulation)
      
      for (sim in 1:nsimulation){
        simulationN <- sim
        bdata[[j]][[i]][[sim]] <- simulate_occupancy_data(M = M, J = nsurveysvec[j],
                                                          p=p[i], simulationN = simulationN, show.plot = FALSE, SimulationParameters = SimulationParameters)
        params_df[[j]][[i]][[sim]] <- bdata[[j]][[i]][[sim]][names(bdata[[j]][[i]][[sim]]) %in% c("param_df")]
        psi[[j]][[i]][[sim]] <-  bdata[[j]][[i]][[sim]][names(bdata[[j]][[i]][[sim]]) %in% c("p", "mean.psi")]
        bdata[[j]][[i]][[sim]]  <- bdata[[j]][[i]][[sim]][!names(bdata[[j]][[i]][[sim]]) %in% c("p", "mean.psi", "param_df")]
      }
    } 
  }
  
  
  # Parameters for the model
  params <- c("p", "mean.psi", "beta0", "beta1") 
  
  # Define initial states
  zst <- rep(1, M)
  inits <- function(){list(z = zst)}
  # print(zst)
  
  # Run models
  
  
  results <- run_and_store_models(
    nsurveyModels = length(nsurveysvec), 
    nDetecmodels = length(p),
    nSims = nsimulation,  
    bdata_list = bdata,  
    model_file = model_file,
    inits = inits, 
    params_to_monitor = params,
    param_df = params_df,
    n.adapt = n.adapt, 
    n.chains = n.chains,
    n.iter = n.iter, 
    n.burnin = n.burnin, 
    n.thin = n.thin, 
    parallel = parallel
    )
  
  
  # Return results
  return(list(results, bdata, psi, params_df))
}

