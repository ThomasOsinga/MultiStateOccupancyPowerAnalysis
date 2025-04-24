psi <- 0.6
R1 <- 0.5
R2 <- 0.2
nsites <- 1000
Fdens <- rnorm(n = nsites, -2, 2)
Fdens2 <- runif(n = nsites, -2, 2)
Omega <- matrix(0, nrow = nsites, ncol = 4)
beta0 = log(c(3, 0.5, 0.1))
beta1 = c(1, 3, 1)
# If covariates are used, calculate Omega based on logistic regression
for (i in 1:nsites) {
  psi_logit <- beta0[1] + beta1[1] * Fdens[i]
  R1_logit <- beta0[2] + beta1[2] * Fdens[i]
  R2_logit <- beta0[3] + beta1[3] * Fdens[i]
  
  psi <- plogis(psi_logit)
  R1 <- plogis(R1_logit)
  R2 <- plogis(R2_logit)
  
  Omega[i, ] <- c(1 - psi, psi * (1 - R1), psi * R1 * (1 - R2), psi * R1 * R2)
}


plot(Omega[])
rowSums(Omega[,2:4])


par(mfrow = c(2, 2))
plot(Fdens, Omega[, 1], main = "Psi (Unoccupied) vs Fdens", xlab = "Forest Density", ylab = "Probability", ylim=c(0,1))
plot(Fdens, Omega[, 2], main = "Psi (Occupied without calves) vs Fdens", xlab = "Forest Density", ylab = "Probability", ylim=c(0,1))
plot(Fdens, Omega[, 3], main = "Psi (Occupied with one calf) vs Fdens", xlab = "Forest Density", ylab = "Probability", ylim=c(0,1))
plot(Fdens, Omega[, 4], main = "Psi (Occupied with >1 calves) vs Fdens", xlab = "Forest Density", ylab = "Probability", ylim=c(0,1))






############

# ------------------------------
# 1. Setup and helper functions
# ------------------------------

rm(list = ls())

set.seed(123)   # for reproducibility
nsites <- 1000

# Generate a covariate 'Fdens'
Fdens <- runif(n = nsites, min = -2, max = 2)

# Helper function: given intercept (b0), slope (b1), and data vector x,
# compute the mean of plogis(b0 + b1 * x).
mean_logistic <- function(b0, b1, x) {
  mean(plogis(b0 + b1 * x))
}

# Objective function for optim: we want the mean of plogis(b0 + b1*x)
# to be a 'target'. We return the squared difference for easy minimization.
obj_fn <- function(b0, b1, x, target) {
  (mean_logistic(b0, b1, x) - target)^2
}

# ------------------------------
# 2. Define scenarios
# ------------------------------

# We'll define the "target occupancy" (mean of psi),
# and also the "target R1" and "target R2" means.
# For simplicity, let's keep R1 = 0.5, R2 = 0.5 across all scenarios,
# so half of occupied sites go to R1, and then half of those to R2, etc.
# But you can definitely vary those if you want different partitions.
scenarios <- data.frame(
  occupancy = c(0.1, 0.3, 0.5, 0.7, 0.9),  # mean of psi
  R1        = 0.5,                       # mean of R1
  R2        = 0.5                        # mean of R2
)

# We also define slopes for psi, R1, R2.
# If you want them to differ by scenario, make them columns in 'scenarios'.
beta1_psi <- 1
beta1_R1  <- 0.5
beta1_R2  <- 0.2

# Prepare a list to hold all results
Omega_list <- vector("list", length = nrow(scenarios))
Beta_list  <- vector("list", length = nrow(scenarios))
# ------------------------------
# 3. Calibrate intercepts and compute Omega
# ------------------------------

for (s in seq_len(nrow(scenarios))) {
  
  # Extract the target means
  target_psi <- scenarios$occupancy[s]
  target_R1  <- scenarios$R1[s]
  target_R2  <- scenarios$R2[s]
  
  # ---- 3a. Solve for beta0_psi so that mean(psi) = target_psi ----
  
  # We'll do a 1D optimization for b0 given we fixed the slope
  res_psi <- optim(
    par    = 0,  # initial guess for intercept
    fn     = function(b0) obj_fn(b0, beta1_psi, Fdens, target_psi),
    method = "BFGS"
  )
  beta0_psi <- res_psi$par
  
  # Check how well it matched the target
  mean_psi <- mean_logistic(beta0_psi, beta1_psi, Fdens)
  
  # ---- 3b. Solve for beta0_R1 so that mean(R1) = target_R1 ----
  res_R1 <- optim(
    par    = 0,
    fn     = function(b0) obj_fn(b0, beta1_R1, Fdens, target_R1),
    method = "BFGS"
  )
  beta0_R1 <- res_R1$par
  mean_R1  <- mean_logistic(beta0_R1, beta1_R1, Fdens)
  
  # ---- 3c. Solve for beta0_R2 so that mean(R2) = target_R2 ----
  res_R2 <- optim(
    par    = 0,
    fn     = function(b0) obj_fn(b0, beta1_R2, Fdens, target_R2),
    method = "BFGS"
  )
  beta0_R2 <- res_R2$par
  mean_R2  <- mean_logistic(beta0_R2, beta1_R2, Fdens)
  
  # ---- 3d. Compute Omega for all sites ----
  
  # For each site, compute psi[i], R1[i], R2[i]
  psi_vals <- plogis(beta0_psi + beta1_psi * Fdens)
  R1_vals  <- plogis(beta0_R1 + beta1_R1 * Fdens)
  R2_vals  <- plogis(beta0_R2 + beta1_R2 * Fdens)
  
  # Build the 4-state matrix
  #   state1 = unoccupied = 1 - psi
  #   state2 = occupied w/o calves = psi * (1 - R1)
  #   state3 = occupied w/ 1 calf  = psi * R1 * (1 - R2)
  #   state4 = occupied w/ >1 calves = psi * R1 * R2
  Omega_scenario <- matrix(NA, nrow = nsites, ncol = 4)
  Omega_scenario[, 1] <- 1 - psi_vals
  Omega_scenario[, 2] <- psi_vals * (1 - R1_vals)
  Omega_scenario[, 3] <- psi_vals * R1_vals * (1 - R2_vals)
  Omega_scenario[, 4] <- psi_vals * R1_vals * R2_vals
  
  # Store in the list
  Omega_list[[s]] <- Omega_scenario
  
  Beta_list[[s]] <- list(
    beta0_psi = beta0_psi,
    beta1_psi = beta1_psi,
    beta0_R1  = beta0_R1,
    beta1_R1  = beta1_R1,
    beta0_R2  = beta0_R2,
    beta1_R2  = beta1_R2
  )
  # Print out a summary
  cat("\nScenario", s, "----------------\n")
  cat("Target mean occupancy (psi) =", target_psi, "\n")
  cat("Achieved mean(psi)         =", round(mean_psi, 4), "\n")
  cat("Target mean R1             =", target_R1, "\n")
  cat("Achieved mean(R1)          =", round(mean_R1, 4), "\n")
  cat("Target mean R2             =", target_R2, "\n")
  cat("Achieved mean(R2)          =", round(mean_R2, 4), "\n")
  
  # Check the actual "mean occupancy" from the final Omega
  #  This is mean( Omega[,2] + Omega[,3] + Omega[,4] )
  mean_occ <- mean(rowSums(Omega_scenario[, 2:4]))
  cat("Mean occupancy from Omega  =", round(mean_occ, 4), "\n")
}
# Done!

mean_occ <- rowSums(Omega_list[[3]][, 2:4])

par(mfrow = c(2, 2))
for (i in 1:4){
plot(Fdens, Omega_list[[1]][, 1], main = "Psi (Unoccupied) vs Fdens", xlab = "Forest Density", ylab = "Probability", ylim=c(0,1))
plot(Fdens, Omega_list[[1]][, 2], main = "Psi (Occupied without calves) vs Fdens", xlab = "Forest Density", ylab = "Probability", ylim=c(0,1))
plot(Fdens, Omega_list[[1]][, 3], main = "Psi (Occupied with one calf) vs Fdens", xlab = "Forest Density", ylab = "Probability", ylim=c(0,1))
plot(Fdens, Omega_list[[1]][, 4], main = "Psi (Occupied with >1 calves) vs Fdens", xlab = "Forest Density", ylab = "Probability", ylim=c(0,1))

}

# s: index of the scenario you're interested in
s <- 1  # or 2, 3, etc.

# Extract intercepts
beta0_vec <- c(
  Beta_list[[s]]$beta0_psi,
  Beta_list[[s]]$beta0_R1,
  Beta_list[[s]]$beta0_R2
)

# Extract slopes
beta1_vec <- c(
  Beta_list[[s]]$beta1_psi,
  Beta_list[[s]]$beta1_R1,
  Beta_list[[s]]$beta1_R2
)

beta0_vec
beta1_vec

# Suppose Beta_list has length = number of scenarios
n_scen <- length(Beta_list)

# Each row in beta0_mat => intercepts for that scenario
beta0_mat <- do.call(rbind, lapply(Beta_list, function(x) {
  c(x$beta0_psi, x$beta0_R1, x$beta0_R2)
}))
colnames(beta0_mat) <- c("psi", "R1", "R2")

# Each row in beta1_mat => slopes for that scenario
beta1_mat <- do.call(rbind, lapply(Beta_list, function(x) {
  c(x$beta1_psi, x$beta1_R1, x$beta1_R2)
}))
colnames(beta1_mat) <- c("psi", "R1", "R2")

beta0_mat
beta1_mat
