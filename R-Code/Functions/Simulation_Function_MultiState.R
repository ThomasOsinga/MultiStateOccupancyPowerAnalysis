#Dont run these - Only there to be called from IndexFile
generate_true_states <- function(M = NULL, nsimulations = NULL, beta0 = NULL , 
                                 beta1 = NULL , Covariate = NULL, psi = NULL, R1 = NULL, R2 = NULL, covariates = NULL, occupancy=NULL) {
  
  # Initialize Omega matrix
  Omega <- matrix(0, nrow = M, ncol = 4)
  
  if (covariates == TRUE) {
    # If covariates are used, calculate Omega based on logistic regression
    for (i in 1:M) {
      psi_logit <- beta0[1] + beta1[1] * Covariate[i]
      R1_logit <- beta0[2] + beta1[2] * Covariate[i]
      R2_logit <- beta0[3] + beta1[3] * Covariate[i]
      
      psi <- plogis(psi_logit)
      R1 <- plogis(R1_logit)
      R2 <- plogis(R2_logit)
      
      Omega[i, ] <- c(1 - psi, psi * (1 - R1), psi * R1 * (1 - R2), psi * R1 * R2)
    }
  } else {
    for (i in 1:M) {
      # If covariates are not used, use fixed probabilities
      occupancy <- data.frame(psi, R1 , R2 )
      Omega[i, ] <- c(1 - psi, psi * (1 - R1), psi * R1 * (1 - R2), psi * R1 * R2)
    }
  }
  
  # Generate true states matrix
  true_states <- matrix(nrow = M, ncol = nsimulations)
  for (sim in 1:nsimulations) {
    for (i in 1:M) {
      true_states[i, sim] <- sample(1:4, 1, prob = Omega[i, ], replace = TRUE)
    }
  }
  
  return(list(true_states = true_states, Mean_Omega = colMeans(Omega), Omega = Omega, 
              Covariate = Covariate, beta0 = beta0, beta1 = beta1, occupancy = occupancy, covariates = covariates))
}



simulate_occupancy_data <- function(M = 97, J = 91, 
                                    p11 = 0.1, p21 = 0.05, p22 = 0.1, p31 = 0.05, p32 = 0.05, p33 = 0.1, 
                                    show.plot = FALSE, simulationN, SimulationParameters) {
  
  # Generate Theta matrix based on detection probabilities
  Theta <- matrix(ncol=4, nrow=4)
  Theta[1,1] <- 1
  Theta[1,2] <- 0
  Theta[1,3] <- 0
  Theta[1,4] <- 0
  
  Theta[2,1] <- 1 - p11
  Theta[2,2] <- p11
  Theta[2,3] <- 0
  Theta[2,4] <- 0
  
  Theta[3,1] <- 1 - (p21 + p22)
  Theta[3,2] <- p21
  Theta[3,3] <- p22
  Theta[3,4] <- 0
  
  Theta[4,1] <- 1 - (p31 + p32 + p33)
  Theta[4,2] <- p31
  Theta[4,3] <- p32
  Theta[4,4] <- p33
  
  # Covariate generation
  if(SimulationParameters$covariates == TRUE) {
  parameters <- c("beta0[1,1]", "beta0[2,1]","beta0[3,1]","beta1[1,1]", "beta1[2,1]", "beta1[3,1]",
                  "p1",
                  "p21", "p22",
                  "p31", "p32", "p33",
                  "mean.Omega1", "mean.Omega2", "mean.Omega3", "mean.Omega4"
                   ) 
  Covariate <- SimulationParameters$Covariate
  # Compute Omega matrix based on logistic regression of occupancy probabilities
  Omega <- SimulationParameters$Omega
  Mean.Omega <- colMeans(Omega) 
  values <- c(SimulationParameters$beta0[[1]],SimulationParameters$beta0[[2]],SimulationParameters$beta0[[3]],SimulationParameters$beta1[[1]],SimulationParameters$beta1[[2]],SimulationParameters$beta1[[3]], p11, p21, p22, p31, p32, p33,   Mean.Omega[1],   Mean.Omega[2],   Mean.Omega[3],   Mean.Omega[4])
  param_df <- data.frame(parameter = parameters, truth = values, covariates = SimulationParameters$covariates)
  } else {
    parameters <- c( "psi","R1","R2",
                    "p1",
                    "p21", "p22",
                    "p31", "p32", "p33", 
                    "mean.Omega1", "mean.Omega2", "mean.Omega3", "mean.Omega4"
                    )
    Omega <- SimulationParameters$Omega
    Mean.Omega <- colMeans(Omega) 
    values <- c(SimulationParameters$occupancy$psi,SimulationParameters$occupancy$R1, SimulationParameters$occupancy$R2, p11, p21, p22, p31, p32, p33,   Mean.Omega[1],   Mean.Omega[2],   Mean.Omega[3],   Mean.Omega[4])
    param_df <- data.frame(parameter = parameters, truth = values, covariates = SimulationParameters$covariates)
  }

  true.states <- SimulationParameters$true_states[, simulationN] 
  y_simulated <- matrix(NA, nrow = M, ncol = J)
  for (i in 1:M) {
    true_state <-  true.states[i]
    for (j in 1:J) {
      observed_state <- sample(1:4, 1, prob = Theta[true_state, ])
      y_simulated[i, j] <- observed_state
      
    }
  }
  
  # Optionally plot results
  if (show.plot & SimulationParameters$covariates) {
    par(mfrow = c(2, 2))
    plot(Covariate, Omega[, 1], main = "Psi (Unoccupied) vs Covariate", xlab = "Forest Density", ylab = "Probability", ylim=c(0,1))
    plot(Covariate, Omega[, 2], main = "Psi (Occupied without calves) vs Covariate", xlab = "Forest Density", ylab = "Probability", ylim=c(0,1))
    plot(Covariate, Omega[, 3], main = "Psi (Occupied with one calf) vs Covariate", xlab = "Forest Density", ylab = "Probability", ylim=c(0,1))
    plot(Covariate, Omega[, 4], main = "Psi (Occupied with >1 calves) vs Covariate", xlab = "Forest Density", ylab = "Probability", ylim=c(0,1))
  }
  
  x <- list(true.states = true.states, y = y_simulated, Theta = Theta, Omega = Omega, Covariate=Covariate, Mean.Omega = Mean.Omega, nsites=M, nsurveys=J, param_df = param_df)
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
        

      
        
        
        
        current_data <- bdata_list[[survey_index]][[detection_index]][[simulation_index]]
        # ... model runs ...
        cat("Start time:", Sys.time(), "\n")
        # Run JAGS model
        jags.model <- jags(current_data, inits = inits, parameters.to.save = params_to_monitor,
                           model.file = model_file, n.adapt = n.adapt, n.chains = n.chains, n.iter = n.iter,
                           n.burnin = n.burnin, n.thin = n.thin, parallel = parallel)
        # ... model runs ...
        cat("End time:", Sys.time(), "\n")
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

run_complete_simulation <- function(nsurveysvec, p11, p21, p22, p31, p32, p33,
                                    nsimulation, n.adapt, n.iter, n.burnin, n.thin, n.chains,
                                    parallel=TRUE, model_file, M, beta0, beta1, Covariate, psi, R1 , R2, covariates) {
  
  # Stores the true states for all sites across all simulations
  SimulationParameters <- generate_true_states(M = M, nsimulation = nsimulation,
                                               beta0 = beta0, beta1 = beta1, 
                                               Covariate=Covariate, covariates = covariates, 
                                               psi = psi, R1 = R1, R2 = R2)
  
  # Initialize storage structures
  bdata <- vector("list", length(nsurveysvec))
  params_df <- vector("list", length(nsurveysvec))
  psi <- vector("list", length(nsurveysvec))
  
  # Set up simulations based on survey vectors and detection probabilities
  for (j in seq_along(nsurveysvec)) {
    bdata[[j]] <- vector("list", length(p11))
    params_df[[j]] <- vector("list", length(p11))
    psi[[j]] <- vector("list", length(p11))
    
    for (i in seq_along(p11)) {
      bdata[[j]][[i]] <- vector("list", nsimulation)
      params_df[[j]][[i]] <- vector("list", nsimulation)
      psi[[j]][[i]] <- vector("list", nsimulation)
      
      for (sim in 1:nsimulation){
        simulationN <- sim
        bdata[[j]][[i]][[sim]] <- simulate_occupancy_data(M = M, J = nsurveysvec[j],
                                                          p11 = p11[i], p21 = p21, p22 = p22[i], p31 = p31, p32 = p32, p33 = p33[i],
                                                          simulationN, show.plot = FALSE, SimulationParameters = SimulationParameters)
        params_df[[j]][[i]][[sim]] <- bdata[[j]][[i]][[sim]][names(bdata[[j]][[i]][[sim]]) %in% c("param_df")]
        psi[[j]][[i]][[sim]] <-  bdata[[j]][[i]][[sim]][names(bdata[[j]][[i]][[sim]]) %in% c("Mean.Omega", "Theta","Omega")]
        bdata[[j]][[i]][[sim]] <- bdata[[j]][[i]][[sim]][
          !names(bdata[[j]][[i]][[sim]]) %in% c("Mean.Omega", "Theta", "Omega", "param_df", "true.states")]
      }
    } 
  }
  if(SimulationParameters$covariates == TRUE) {
  # Parameters for the model
  params <- c("beta0", "beta1", "mean.psi", "mean.R1", "mean.R2", "mean.Omega1", "mean.Omega2", "mean.Omega3", "mean.Omega4",
              "p1", "p21", "p22", "p31", "p32", "p33", "n.occ")
  } else {
  params <- c("psi", "R1", "R2","mean.psi", "mean.R1", "mean.R2", "mean.Omega1", "mean.Omega2", "mean.Omega3", "mean.Omega4",
                "p1", "p21", "p22", "p31", "p32", "p33", "n.occ")
  }
  # Define initial states
  zst <- rep(4, nrow(bdata[[1]][[1]][[1]]$y)) 
  inits <- function(){ list(z = zst) }
  
  # Run models

  results <- run_and_store_models(
    nsurveyModels = length(nsurveysvec), 
    nDetecmodels = length(p11),
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

