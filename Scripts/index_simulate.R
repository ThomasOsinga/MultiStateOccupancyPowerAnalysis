### Prelims
setwd("F:/SimulationFolderThomas")
source("script/Simulation_Function_fixSeed.R") ##### VERANDERD ######
set.seed(1234567) # for reproducibility in randomization of the seeds
nrItersPerSession <- 6
nrSessions <- 4
useSeeds <- matrix(floor(runif(nrSessions*nrItersPerSession, min=0, max=1234567)),
                   nrow = nrItersPerSession, # rows the iterations within sessions
                   ncol = nrSessions)        # cols are sessions, 

### which session
sessionNR <- 1

### for multiple sub-sessions
for(subSession in 1:nrItersPerSession) { ##### VERANDERD ######
  p11 <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9); p21 <- 0.05               #DP Occupied
  p22 <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9);                           #DP 1 Young
  p31 <- 0.05 ;p32 <- 0.05 ;p33 <-  c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9) #DP 2 Young
  
  nsurveysvec <- c(35)
  nsimulation <- 10 ##### VERANDERD ######
  library(jagsUI)
  library(tictoc)
  na <- 3000; ni <- 50000 ; nt <- 5 ; nb <- 32500 ; nc <- 3
  model_file = "4States_1Cov_SimNoPlot.txt"
  
  tstart <- Sys.time()
  cat(paste0("\nstart session ",sessionNR,": ",tstart,"\n"))
  
  ### NEW SEED PER R WINDOW ###
  set.seed(useSeeds[subSession,sessionNR]);
  Fdens <- runif(n = 60, -2, 2) ##### TOEGEVOEGD ######
  results <- run_complete_simulation(nsurveysvec = nsurveysvec, p11 = p11, p21 = p21, p22 = p22, p31 = p31, p32 = p32, p33 = p33,
                                     nsimulation=nsimulation, n.adapt = na, n.iter = ni, n.burnin = nb, n.thin = nt, n.chains = nc,
                                     parallel = TRUE, model_file = model_file, M = 60, beta0 = log(c(3,0.5,0.1)), beta1 = c(4,5,1),
                                     Fdens=Fdens) ##### TOEGEVOEGD ######
  
  tend <- Sys.time()
  cat(paste0("\nend: ",tend,"\n"))
  
  tdiff <- tend - tstart
  cat(paste0("\ntime needed: ",round(tdiff, 4),' ',units(tdiff),"\n"))
  
  ### Set filename
  saveFilename <- paste0('Output/SummaryResults_',sessionNR,'_',subSession,"_",format(tend, "%Y%m%d%H%M%S"),'.rds')
  # Save
  saveRDS(results, saveFilename) ###Change the name of the file per R-execution after simulations are done
}


### END