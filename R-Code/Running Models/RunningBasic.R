#### Majority of the models were ran on a HPC through rslurm - this is an alternative. 
### Prelims
## WorkingDirectory = MultiStateOccupancyModelsPowerAnalysis - RepositoryFolder
#Jags V4.3.2-1.2204.0
#R V4.4.1 
#JagsUI V1.6.2

## Make sure your Working Directory is set to the folder containing the following folders:
# BugsModelCode and R-Code
# setwd("~/Repository_detectionProbability")
#################
## WorkingDirectory = MultiStateOccupancyModelsPowerAnalysis-main - RepositoryFolder
## This should work too if file is opened from the repository folder:
## setwd(dirname(dirname(dirname(normalizePath(rstudioapi::getSourceEditorContext()$path)))))
###############

#### START CODE ####
for (pkg in c("jagsUI")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

source("R-Code/Functions/BasicSimulationFunction.R") 
set.seed(1234567) # for reproducibility in randomization of the seeds
nrItersPerSession <- 5
nrSessions <- 1
useSeeds <- matrix(floor(runif(nrSessions*nrItersPerSession, min=0, max=1234567)),
                   nrow = nrItersPerSession, # rows the iterations within sessions
                   ncol = nrSessions)        # cols are sessions, 
### which session
sessionNR <- 1
p <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9); #DP Occupied
psi_fixed = 0.1
beta0 = 3 #intercept
beta1 = 2 #betacoefficient for occupancy vs 1 Covariate
nsimulation <- 1 
#Model settings, na = n.adapt, ni = n.iterations, nt = thin, nb = n.burnin, nc = n.chains
na <- 3000; ni <- 25000 ; nt <- 5 ; nb <- 12500 ; nc <- 3
### To avoid confusion, this code has 
#1) Covariate = simulated landscape covariate 
#                 and
#2) covariates = A binary T/F enabling or disabling the Covariate
# We'll define two "model scenarios": covariates=TRUE vs covariates=FALSE
model_scenarios <- list(
  list(
    covariates = TRUE, 
    model_file = "BugsModelCode/SimpleOccupancyModel_WithCov.txt",
    label      = "Cov"
    # We'll pass beta0, beta1, Covariate inside the loop
  ),
  list(
    covariates = FALSE,
    model_file = "BugsModelCode/SimpleOccupancyModel.txt",
    label      = "NoCov"
    # We'll pass psi, R1, R2 inside the loop
  )
)



ModelNr <- 0
# Loop over each scenario, site counts, survey counts, and subSessions 
# Use this for loop if you want to run both covariate/non-covariate models - 
# You need to adjust "model_scenarios[x]" to "scenarios" if you want to use the extra for loop below. 
# for (scenario in model_scenarios) {
#   
#   cat("\n\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
#   cat("Starting scenario:", scenario$label,
#       "- model_file =", scenario$model_file, "\n")
#   cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n")
# 
  
  for (M in c(500, 1000)) {  # loop over site counts
    Covariate <- runif(n = M, min=-2, max=2)
    for (surveyConfig in c(7, 14)) {       # loop over # of surveys
      
      cat("\n\n=============================================\n")
      cat("Running for M =", M, 
          ", nsurveys =", surveyConfig, 
          ", scenario =", model_scenarios[[2]]$label, "\n")
      cat("=============================================\n\n")
      
      # subSession loop
      for(subSession in 1:nrItersPerSession) {
        # Start timing
        tstart <- Sys.time()
        cat(paste0("\nStart session ", sessionNR,
                   ", subSession ", subSession, 
                   " for M=", M, " surveys=", surveyConfig,
                   ", scenario=", model_scenarios[[2]]$label, ": ", tstart, "\n"))
        # Set new seed per iteration
        set.seed(useSeeds[subSession, sessionNR])
        
        results <- run_complete_simulation(nsurveysvec = surveyConfig, p,
                                           nsimulation=nsimulation, n.adapt = na, 
                                           n.iter = ni, n.burnin = nb, n.thin = nt,
                                           n.chains = nc, covariates = model_scenarios[[2]]$covariates,
                                           parallel = TRUE, model_file = model_scenarios[[2]]$model_file, M = M, 
                                           Covariate = Covariate, beta0, beta1, psi=psi_fixed, show.plot = TRUE)
        
        tend <- Sys.time()
        cat(paste0("\nend: ",tend,"\n"))
        
        tdiff <- tend - tstart
        cat(paste0("\ntime needed: ",round(tdiff, 4),' ',units(tdiff),"\n"))
        
        saveDir <- "Outputs/NsurveySimulation/Basic"
        if (!dir.exists(saveDir)) {
          dir.create(saveDir, recursive = TRUE)
        }
        
        # Then build your filename
        saveFilename <- paste0(
          saveDir, "/",                       # Use the directory
          "SummaryResults_", "Basic_", 
          model_scenarios[[2]]$label,  # "Cov" or "NoCov"
          "_Nsites", M,
          "_Nsurveys", surveyConfig,
          "_session", sessionNR,
          "_sub", subSession, 
          "_", format(tend, "%Y%m%d%H%M%S"), 
          ".rds"
        )
        
        # Save
        saveRDS(results, file = saveFilename)
      } # end subSession loop
    } # end surveyConfig loop
  } # end M loop

### END

