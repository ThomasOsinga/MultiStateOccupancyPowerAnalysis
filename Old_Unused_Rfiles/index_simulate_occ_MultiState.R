### Prelims - JAGS is required - 
## WorkingDirectory = MultiStateOccupancyModelsPowerAnalysis - RepositoryFolder
source("R-Code/SimulationCode/MultiState/Simulation_Function_MultiState.R") 

for (pkg in c("jagsUI", "tictoc")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

set.seed(1234567) # for reproducibility in randomization of the seeds
nrItersPerSession <- 13
nrSessions <- 4
useSeeds <- matrix(floor(runif(nrSessions*nrItersPerSession, min=0, max=1234567)),
                   nrow = nrItersPerSession, # rows the iterations within sessions
                   ncol = nrSessions)        # cols are sessions, 

### which session
sessionNR <- 1
x = 0 #for keeping track of which model is being ran in text file

### for multiple sub-sessions
for(subSession in 1:nrItersPerSession) {
  p11 <- c(0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9); p21 <- 0.05               #DP Occupied
  p22 <- c(0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9);                           #DP 1 Young
  p31 <- 0.05 ;p32 <- 0.05 ;p33 <-  c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9) #DP 2 Young
  psi <- 0.6
  R1 <- 0.5
  R2 <- 0.2
  covariates <- TRUE
  nsurveysvec <- c(7) 
  nsites <- 800
  nsimulation <- 1

  na <- 300; ni <- 5000 ; nt <- 5 ; nb <- 3250 ; nc <- 3
  model_file = "BugsModelCode/4States_1Cov_SimNoPlot.txt" 
  #model_file = "BugsModelCode/4States_NoCov.txt
  
  tstart <- Sys.time()
  cat(paste0("\nstart session ",sessionNR,": ",tstart,"\n"))
  
  ### NEW SEED PER R WINDOW ###
  set.seed(useSeeds[subSession,sessionNR]);
  Fdens <- runif(n = nsites, -2, 2) ##### LandscapeCovariate
  results <- run_complete_simulation(nsurveysvec = nsurveysvec, p11 = p11, p21 = p21, p22 = p22, p31 = p31, p32 = p32, p33 = p33,
                                     nsimulation=nsimulation, n.adapt = na, n.iter = ni, n.burnin = nb, n.thin = nt, n.chains = nc,
                                     parallel = TRUE, model_file = model_file, M = nsites, beta0 = beta0_vec, beta1 = beta1_vec,
                                     Fdens=Fdens, psi = psi, R1 = R1, R2 = R2, covariates = covariates) 
  
  tend <- Sys.time()
  cat(paste0("\nend: ",tend,"\n"))
  
  tdiff <- tend - tstart
  cat(paste0("\ntime needed: ",round(tdiff, 4),' ',units(tdiff),"\n"))
  
  ### Set filename
  saveFilename <- paste0('Outputs/NsurveySimulation/MultiState/SummaryResults_',"Nsites",nsites,sessionNR,'_',subSession,"_",format(tend, "%Y%m%d%H%M%S"),'.rds')
  # Save
  saveRDS(results, saveFilename) 
}


### END

x <- readRDS("Outputs/NsurveySimulation/MultiState/SummaryResults_Nsites10001_1_20250114213411.rds")
