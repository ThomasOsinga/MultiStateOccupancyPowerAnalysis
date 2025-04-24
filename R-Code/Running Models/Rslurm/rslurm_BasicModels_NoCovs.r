### Load HPC library
library(rslurm)
library(jagsUI)

### Create parameter grid
params = expand.grid(
  'sessionNR' = 1:240,
  'nrSessions' = 240,
  'SubSession' = 1,
  'nrItersPerSession' = 1
)

### Create simulation function

fn = function(sessionNR, nrSessions, SubSession, nrItersPerSession) {
  set.seed(1234567)
  useSeeds <- matrix(floor(runif(nrSessions * nrItersPerSession, min = 0, max = 1234567)),
                     nrow = nrItersPerSession, ncol = nrSessions)
  setwd("../BasicModels")
  source("R-Code/BasicModel/BasicSimulationFunction_SiteScenarios.R")
  start_time <- Sys.time()
  model <- list(
    "500" = NA,
    "1000" = NA
  )
p <-  c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9)
  na <- 3000; ni <- 25000 ; nt <- 5 ; nb <- 12500 ; nc <- 3
  set.seed(useSeeds[SubSession, sessionNR])
  for (M in c(60)) {
  Covariate <- runif(M, -2, 2)
  result <- run_complete_simulation(
    p = p, 
    psi = 0.6,
    nsimulation = 1,
    n.adapt = na, n.iter = ni, n.burnin = nb, n.thin = nt, n.chains = nc,
    model_file = "BugsModelCode/SimpleOccupancyModel.txt",
    parallel = FALSE,
    M = M,
    beta0 = 3, 
    beta1 = 2,
    Covariate = Covariate,
    covariates = FALSE,
    nsurveysvec = c(7,14),
    show.plot=FALSE
  )
 

  end_time <- Sys.time()
  elapsed <- end_time - start_time
  
  # End timing
  tend <- Sys.time()
  
  saveDir <- "Outputs/Basic_Model/NoCovs"
  if (!dir.exists(saveDir)) {
    dir.create(saveDir, recursive = TRUE)
  }
  
  # Then build your filename
  saveFilename <- paste0(
    saveDir, "/",                       # Use the directory
    "SummaryResults_", "Basic_", 
    "cov",
    "_Nsites",60,
    "_Nsurveys_7_14",
    "_session", sessionNR,
    "_sub", SubSession, 
    "_", format(tend, "%Y%m%d%H%M%S"), 
    ".rds"
  )
  # Save
  saveRDS(result, file = saveFilename)
  model[[paste0(M)]] <- result
  }
  return(model)
}

### Start HPC array job
sopt = list('time' = '4-00:00:00',  
            'mail-type' = 'FAIL',
            'error' = 'error_%a.txt',
            'qos' = 'std',
            'mem-per-cpu' = '1500',
            'cpus-per-task' = '1')

sjob = slurm_apply(f = fn,
                   params = params,
                   jobname ="BasicNoCovs",
                   nodes = nrow(params),
                   cpus_per_node = 1,
                   job_array_task_limit = 240,
                   slurm_options = sopt,
                   submit = TRUE)
