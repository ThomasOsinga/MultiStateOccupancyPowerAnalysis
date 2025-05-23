### Prelims - JAGS is required 
## WorkingDirectory = MultiStateOccupancyModelsPowerAnalysis - RepositoryFolder

source("R-Code/SimulationCode/BasicModel/BasicSimulationFunction_SurveyScenarios.R")

for (pkg in c("jagsUI", "tictoc")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
set.seed(1234567) # for reproducibility in randomization of the seeds
nrItersPerSession <- 1
nrSessions <- 4
useSeeds <- matrix(floor(runif(nrSessions*nrItersPerSession, min=0, max=1234567)),
                   nrow = nrItersPerSession, # rows the iterations within sessions
                   ncol = nrSessions)        # cols are sessions, 
x=0
### which session
sessionNR <- 1

### for multiple sub-sessions
for(subSession in 1:nrItersPerSession) {
  p <- c(0.001,0.005,0.01,0.1,0.2,0.3,0.5,0.7,0.9);              #DP Occupied , 
  M=60   #Nsites                         
  psi = c(0.1, 0.3, 0.5, 0.7, 0.9)
  nsurveysvec <- c(7,14,35)
  nsimulation <- 1
  na <- 3000; ni <- 25000 ; nt <- 5 ; nb <- 12500 ; nc <- 3
  model_file = "BugsModelCode/SimpleOccupancyModel.txt"
  
  for (i in 1:length(psi)){
  tstart <- Sys.time()
  cat(paste0("\nstart session ",sessionNR,": ",tstart,"\n"))
  
  ### NEW SEED PER R WINDOW ###
  set.seed(useSeeds[subSession,sessionNR]);
  results <- run_complete_simulation(nsurveysvec = nsurveysvec, p,
                                     nsimulation=nsimulation, n.adapt = na, n.iter = ni, n.burnin = nb, n.thin = nt, n.chains = nc,
                                     parallel = TRUE, model_file = model_file, M = M, psi = psi[i]) 
  
  tend <- Sys.time()
  cat(paste0("\nend: ",tend,"\n"))
  
  tdiff <- tend - tstart
  cat(paste0("\ntime needed: ",round(tdiff, 4),' ',units(tdiff),"\n"))
  
  ## Set filename
  saveFilename <- paste0('Outputs/ReviseTest/Basic/SummaryResults_','PSI', psi[i], sessionNR,'_',subSession,"_",format(tend, "%Y%m%d%H%M%S"),'.rds')
 #saveFilename <- paste0('Outputs/NsurveySimulation/Basic/SummaryResults_', sessionNR,'_',subSession,"_",format(tend, "%Y%m%d%H%M%S"),'.rds')
  # Save
  saveRDS(results, saveFilename) ###Change the name of the file per R-execution after simulations are done
  }
}

### END
f <- readRDS(saveFilename)


f
