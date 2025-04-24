#Dont run these - Only there to be called from IndexFile
# Don't run these - Only to be called from IndexFile
generate_true_states <- function(
    M = NULL, 
    nsimulations = NULL,
    covariates = NULL,
    # If no covariates, we use this constant psi:
    psi = NULL,
    # If covariates=TRUE, we use logistic with beta0 + beta1 * Covariate[i]:
    beta0 = NULL,
    beta1 = NULL,
    Covariate = NULL
) {
  # We'll store the occupancy probability for each site and sim in a matrix
  psi_vec <- matrix(0, nrow = M, ncol = 1)
    if (covariates) {
      # If covariates are used, calculate Omega based on logistic regression
      for (i in 1:M) {
        p_occ <- plogis(beta0 + beta1 * Covariate[i])
        psi_vec[i] <- p_occ
      }
      } else {
        # constant probability
        p_occ <- psi
        psi_vec[,] <- p_occ
      }
  true_states <- matrix(nrow = M, ncol = nsimulations)
  for (sim in 1:nsimulations) {
    for (i in 1:M) {
      # Draw presence/absence from a Bernoulli:
      true_states[i, sim] <- rbinom(n = 1, size = 1, prob = psi_vec[i])
    }
  }

  
  return(list(
    true_states = true_states, 
    psi_values  = psi_vec,       # the per-site occupancy probability used
    Covariate   = Covariate,     # the covariate vector if used
    covariates  = covariates,    # TRUE or FALSE
    beta0       = beta0, 
    beta1       = beta1,
    psi         = psi            # the constant psi if covariates=FALSE
  ))
}


simulate_occupancy_data <- function(M = 97, J = 91, 
                                    p,
                                    show.plot = FALSE, simulationN, SimulationParameters, Covariate) {
  
  # Covariate generation
  parameters <- c("p", "mean.psi", "beta0",  "beta1")
  # Compute Omega matrix based on logistic regression of occupancy probabilities
  psi_truth <- SimulationParameters$psi
  mean.psi <- mean(SimulationParameters$psi_values) 
  values <- c(p, mean.psi, SimulationParameters$beta0, SimulationParameters$beta1)
  param_df <- data.frame(parameter = parameters, truth = values, simulated_psi = mean.psi, covariates = SimulationParameters$covariates)
  true.states <- SimulationParameters$true_states[,simulationN] 
  # Vectorized occupancy data generation
  prob_matrix <- matrix(rep(true.states * p, J), nrow = M, byrow = FALSE)
  y_simulated <- matrix(rbinom(M * J, 1, prob_matrix), nrow = M, ncol = J)
  
  # Optionally plot results
  if (show.plot & SimulationParameters$covariates) {
    par(mfrow = c(1, 1))
    plot(Covariate, SimulationParameters$psi_values, main = "Psi  vs Simulated Covariate", xlab = "Simulated Covariate", ylab = "Probability", ylim=c(0,1))
  }
  
  x <- list(true.states = true.states, y = y_simulated, mean.psi = mean.psi, Covariate = Covariate, p = p, psi_truth = psi_truth, nsites=M, nsurveys=J, param_df = param_df)
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
    assign("ModelNr", ModelNr + 1, envir = globalenv())  # Assign ModelNr + 1 to `ModelNr` in the global environment
  }
  # Loop through each model configuration
  for (survey_index in 1:nsurveyModels) {
    for (detection_index in 1:nDetecmodels){
      # Loop through each simulation for the current model
      for (simulation_index in 1:nSims) {

      
 
        # Setup data for the current model
        current_data <- bdata_list[[survey_index]][[detection_index]][[simulation_index]]
        
        # Run JAGS model
        jags.model <- jags(current_data, inits = inits, parameters.to.save = params_to_monitor,
                           model.file = model_file, n.adapt = n.adapt, n.chains = n.chains, n.iter = n.iter,
                           n.burnin = n.burnin, n.thin = n.thin, parallel = parallel)
        print(Sys.time())
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
            n.eff = simulation_summary[,"n.eff"],
            overlap0 = simulation_summary[,"overlap0"],
            f = simulation_summary[,"f"],
            truth = truth_values
          ), simulations_models
        )
      }
    }
  }
  
  return(simulations)
  
}

run_complete_simulation <- function(nsurveysvec, p,
                                    nsimulation, n.adapt, n.iter, n.burnin, n.thin, n.chains,
                                    parallel=TRUE, model_file, M,covariates, Covariate, beta0, beta1, psi, show.plot) {
  
  # Stores the true states for all sites across all simulations
  SimulationParameters <- generate_true_states(M = M,nsimulation = nsimulation, 
                                               beta0 = beta0, beta1 = beta1,  Covariate, 
                                               covariates = covariates, psi = psi)
  
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
                                                          p=p[i], simulationN = simulationN, show.plot = show.plot, SimulationParameters = SimulationParameters, Covariate = Covariate)
        params_df[[j]][[i]][[sim]] <- bdata[[j]][[i]][[sim]][names(bdata[[j]][[i]][[sim]]) %in% c("param_df")]
        psi[[j]][[i]][[sim]] <-  bdata[[j]][[i]][[sim]][names(bdata[[j]][[i]][[sim]]) %in% c("p", "mean.psi")]
        bdata[[j]][[i]][[sim]]  <- bdata[[j]][[i]][[sim]][!names(bdata[[j]][[i]][[sim]]) %in% c("p", "mean.psi", "param_df")]
      }
    } 
  }
  
  
  # Parameters for the model
  params <- c("p","mean.psi", "beta0", "beta1") 
  
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
    parallel = parallel,
    )
  
  
  # Return results
  return(list(results, bdata, psi, params_df))
}

