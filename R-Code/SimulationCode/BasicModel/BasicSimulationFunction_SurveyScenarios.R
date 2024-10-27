#Dont run these - Only there to be called from IndexFile
generate_true_states <- function(M, psi, nsimulations) {
  
  # Generate presence/absence data (the truth)
  true_states <- matrix(nrow = M, ncol = nsimulations)
  true_psi <- matrix(nrow = M, ncol = nsimulations)
  for (sim in 1:nsimulations) {
    for (i in 1:M) {
  true_states[i, sim] <- rbinom(n = 1, size = 1, prob = psi)  # R has no Bernoulli
  true_psi[i, sim] <- psi
    }
  }
  return(list(true_states =  true_states, psi=psi))
}


simulate_occupancy_data <- function(M = 97, J = 91, 
                                    p = 0.05,
                                    show.plot = FALSE, simulationN, SimulationParameters) {

  # Covariate generation
  parameters <- c("p", "psi")
  # Compute Omega matrix based on logistic regression of occupancy probabilities

  values <- c(p, SimulationParameters$psi)
  param_df <- data.frame(parameter = parameters, truth = values)
  true.states <- SimulationParameters$true_states[,simulationN] 
  # Vectorized occupancy data generation
  prob_matrix <- matrix(rep(true.states * p, J), nrow = M, byrow = FALSE)
  y_simulated <- matrix(rbinom(M * J, 1, prob_matrix), nrow = M, ncol = J)
  
  # Optionally plot results
  if (show.plot) {
    par(mfrow = c(2, 2))
    plot(Fdens, Omega[, 1], main = "Psi (Unoccupied) vs Fdens", xlab = "Forest Density", ylab = "Probability", ylim=c(0,1))
    plot(Fdens, Omega[, 2], main = "Psi (Occupied without calves) vs Fdens", xlab = "Forest Density", ylab = "Probability", ylim=c(0,1))
    plot(Fdens, Omega[, 3], main = "Psi (Occupied with one calf) vs Fdens", xlab = "Forest Density", ylab = "Probability", ylim=c(0,1))
    plot(Fdens, Omega[, 4], main = "Psi (Occupied with >1 calves) vs Fdens", xlab = "Forest Density", ylab = "Probability", ylim=c(0,1))
  }
  
  x <- list(true.states = true.states, y = y_simulated, p = p, psi = psi, nsites=M, nsurveys=J, param_df = param_df)
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
  my_function <- function() {
    assign("x", x + 1, envir = globalenv())  # Assign x + 1 to `x` in the global environment
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
        my_function()
        
        # Open connection to log.txt file for appending
        sink("Simplelog.txt", append = TRUE)
        
        # Write the print statements to the log file
        print(paste("models ran", x))
        print(paste("Survey index", survey_index))
        print(paste("Detection index", detection_index))
        print(paste("Simulation index", simulation_index))
        print(Sys.time())
        # Close the connection to the file
        sink()
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
                                    parallel=TRUE, model_file, M, psi) {
  
  # Stores the true states for all sites across all simulations
  SimulationParameters <- generate_true_states(M, psi,nsimulation)
  
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
        psi[[j]][[i]][[sim]] <-  bdata[[j]][[i]][[sim]][names(bdata[[j]][[i]][[sim]]) %in% c("p", "psi")]
        bdata[[j]][[i]][[sim]]  <- bdata[[j]][[i]][[sim]][!names(bdata[[j]][[i]][[sim]]) %in% c("p", "psi", "param_df")]
      }
    } 
  }

  
  # Parameters for the model
  params <- c("p", "psi") 
  
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
    parallel = parallel)
  
  
  # Return results
  return(list(results, bdata, psi, params_df))
}

