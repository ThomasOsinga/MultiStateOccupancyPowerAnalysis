### Load HPC library
library(rslurm)
library(jagsUI)

### Create parameter grid
params = expand.grid(
  'sessionNR' = 1:163,
  'nrSessions' = 163,
  'SubSession' = 1,
  'nrItersPerSession' = 1
)

### Create simulation function
fn = function(sessionNR, nrSessions, SubSession, nrItersPerSession) {
  set.seed(1234567)
  useSeeds <- matrix(floor(runif(nrSessions * nrItersPerSession, min = 0, max = 1234567)),
                     nrow = nrItersPerSession, ncol = nrSessions)
  setwd("../HPC_Occupancy")
  source("R-Code/MultiState/Simulation_Function_MultiState.R")
  start_time <- Sys.time()
  na <- 3000; ni <- 50000 ; nt <- 5 ; nb <- 32500 ; nc <- 3
  M <- 1000
  set.seed(useSeeds[SubSession, sessionNR])
  result <- run_complete_simulation(
    p11 = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9), 
    p21 = 0.05, p22 = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9),                         #DP 1 Young
    p31 = 0.05, p32 = 0.05,p33 =  c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9) ,#DP 2 Young
    psi = 0.6, R1 = 0.5, R2 = 0.2,
    nsimulation = 1,
    n.adapt = na, n.iter = ni, n.burnin = nb, n.thin = nt, n.chains = nc,
    model_file = "BugsModelCode/4States_1Cov.txt",
    parallel = FALSE,
    M = M,
    beta0 = log(c(3, 0.5, 0.1)),
    beta1 = c(4, 5, 1),
    Covariate = runif(M, -2, 2),
    covariates = TRUE,
    nsurveysvec = c(14)
  )
  
  end_time <- Sys.time()
  elapsed <- end_time - start_time
  
  # End timing
  tend <- Sys.time()
  
  saveDir <- "Outputs/MultiState/"
  if (!dir.exists(saveDir)) {
    dir.create(saveDir, recursive = TRUE)
  }
  
  # Then build your filename
  saveFilename <- paste0(
    saveDir, "/",                       # Use the directory
    "SummaryResults_", "Multi-state_", 
    "cov",
    "_Nsites", 1000,
    "_Nsurveys", 14,
    "_session", sessionNR,
    "_sub", SubSession, 
    "_", format(tend, "%Y%m%d%H%M%S"), 
    ".rds"
  )
  # Save
  saveRDS(result, file = saveFilename)
  return(result)
  
}

### Start HPC array job
sopt = list('time' = '8-00:00:00',  
            'mail-type' = 'FAIL',
            'error' = 'error_%a.txt',
            'qos' = 'std',
            'mem-per-cpu' = '3000',
            'cpus-per-task' = '1')

sjob = slurm_apply(f = fn,
                   params = params,
                   jobname = '1000Sites7Surveys',
                   nodes = nrow(params),
                   cpus_per_node = 1,
                   job_array_task_limit = 240,
                   slurm_options = sopt,
                   submit = TRUE)
