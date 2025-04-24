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
## This should work too: 
setwd(dirname(dirname(dirname(normalizePath(rstudioapi::getSourceEditorContext()$path)))))
###############

#### START CODE ####
source("R-Code/Functions/Simulation_Function_MultiState.R") 
for (pkg in c("jagsUI")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}


set.seed(1234567) # for reproducibility in randomization of the seeds
nrItersPerSession <- 60
nrSessions <- 4
sessionNR <- 1   # SET FOR EACH R WINDOW 
nsimulation <- 1   # how many replicates per iteration
useSeeds <- matrix(
  floor(runif(nrSessions * nrItersPerSession, min=0, max=1234567)),
  nrow = nrItersPerSession, # rows: iterations within sessions
  ncol = nrSessions         # cols: sessions
)


# Detection parameters
p11 <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9)
p21 <- 0.05               
p22 <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9)
p31 <- 0.05
p32 <- 0.05
p33 <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9)

# Common occupancy parameters (for the no-covariate scenario)
psi_fixed <- 0.6
R1_fixed  <- 0.5
R2_fixed  <- 0.2

# Covariate scenario: logistic parameters
beta0_vec <- log(c(3, 0.5, 0.1)) # intercept for psi, R1, R2
beta1_vec <- c(4, 5, 1)          # slope for psi, R1, R2

#Model settings, #na = n.adapt, ni = n.iterations, nt = thin, nb = n.burnin, nc = n.chains
na <- 3000; ni <- 50000 ; nt <- 5 ; nb <- 32500 ; nc <- 3


### To avoid confusion, this code has 
#1) Covariate = a simulated landscape covariate scaled between -2,2
#                 and
#2) covariates = A binary T/F enabling or disabling the Covariate
# We'll define two "model scenarios": covariates = TRUE vs covariates = FALSE
model_scenarios <- list(
  list(
    covariates = TRUE, 
    model_file = "BugsModelCode/4States_1Cov.txt",
    label      = "Cov"
    # We'll pass beta0, beta1, Covariate inside the loop
  ),
  list(
    covariates = FALSE,
    model_file = "BugsModelCode/4States_NoCovs.txt",
    label      = "NoCov"
    # We'll pass psi, R1, R2 inside the loop
  )
)

### Keeps track of how many models are ran in text file.
ModelNr <- 0 # RUN THIS BEFORE RUNNING MODEL
# Loop over each scenario, site counts, survey counts, and subSessions 
# Use this for loop if you want to run both covariate/non-covariate models - 
# You need to adjust "model_scenarios[x]" to "scenarios" if you want to use the for loop. 
# for (scenario in model_scenarios) {
#   
#   cat("\n\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
#   cat("Starting scenario:", scenario$label,
#       "- model_file =", scenario$model_file, "\n")
#   cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n")
#   
for (M in c(500, 1000)) {  # loop over site counts
  Covariate <- runif(n = M, min=-2, max=2)
  for (surveyConfig in c(7,14,35)) {       # loop over # of surveys
    
    cat("\n\n=============================================\n")
    cat("Running for M =", M, 
        ", nsurveys =", surveyConfig, 
        ", scenario =", model_scenarios[[1]]$label, "\n")
    cat("=============================================\n\n")
    
    # subSession loop
    for(subSession in 1:nrItersPerSession) {
      # Start timing
      tstart <- Sys.time()
      cat(paste0("\nStart session ", sessionNR,
                 ", subSession ", subSession, 
                 " for M=", M, " surveys=", surveyConfig,
                 ", scenario=", model_scenarios[[1]]$label, ": ", tstart, "\n"))
      # Set new seed per iteration
      set.seed(useSeeds[subSession, sessionNR])
      
      results <- run_complete_simulation(
        nsurveysvec = surveyConfig, 
        p11 = p11, p21 = p21, p22 = p22,
        p31 = p31, p32 = p32, p33 = p33,
        nsimulation=nsimulation,
        n.adapt = na, n.iter = ni, n.burnin = nb, n.thin = nt, n.chains = nc,
        parallel = TRUE, model_file = model_scenarios[[1]]$model_file,
        M = M, 
        beta0 = beta0_vec, 
        beta1 = beta1_vec, 
        Covariate = Covariate,
        psi = psi_fixed, R1 = R1_fixed, R2 = R2_fixed,  
        covariates = model_scenarios[[1]]$covariates
      )
      
      
      # End timing
      tend <- Sys.time()
      cat(paste0("\nEnd: ", tend,"\n"))
      tdiff <- tend - tstart
      cat(paste0("\nTime needed: ",round(tdiff, 4),' ',units(tdiff),"\n"))
      
      saveDir <- "Outputs/NsurveySimulation/MultiState/lowoccupancy"
      if (!dir.exists(saveDir)) {
        dir.create(saveDir, recursive = TRUE)
      }
      
      # Then build your filename
      saveFilename <- paste0(
        saveDir, "/",                       # Use the directory
        "SummaryResults_", "Multi-state_", 
        model_scenarios[[1]]$label,  # "Cov" or "NoCov"
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
#}# end scenario loop

