# =======================================================================

#                       GAMM model diagnostics                          #

# =======================================================================





# =======================================================================
# 1. Load libraries, datasets, and models
# =======================================================================



# Librairies ------------------------------------------------------------

library(data.table)
library(brms)



# Import the model ------------------------------------------------------

# Load models
mod1 <- readRDS("./outputs/02_outputs_models/02A1_GAMM.rds")
mod2 <- readRDS("./outputs/02_outputs_models/02A2_GAMM.rds")

# =======================================================================
# =======================================================================




# =======================================================================
# 2. Prepare functions for post-processing (beta-binom custom family)
# =======================================================================

# Expose functions
expose_functions(mod1, vectorize = TRUE)

# Define the log likelihood function
log_lik_beta_binomial2 <- function(i, prep) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  phi <- brms::get_dpar(prep, "phi", i = i)
  trials <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  beta_binomial2_lpmf(y, mu, phi, trials)
}

# Define function for posterior_predict
posterior_predict_beta_binomial2 <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  phi <- brms::get_dpar(prep, "phi", i = i)
  trials <- prep$data$vint1[i]
  beta_binomial2_rng(mu, phi, trials)
}

# Define function for posterior_epred
posterior_epred_beta_binomial2 <- function(prep) {
  mu <- brms::get_dpar(prep, "mu")
  trials <- prep$data$vint1
  trials <- matrix(trials, nrow = nrow(mu), ncol = ncol(mu), byrow = TRUE)
  mu * trials
}

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Basic model diagnostics
# =======================================================================


# Trace plots and parameter distributions -------------------------------

plot(mod1)
plot(mod2)



# Posterior predictive checks -------------------------------------------

# Check distributions
pp1 <- pp_check(mod1)
pp2 <- pp_check(mod2)
 
 
# Effects plot (mean variance plot)
stat1 <- pp_check(mod1, type = "stat_2d")
stat2 <- pp_check(mod2, type = "stat_2d")


# Predicted means
mean1 <- pp_check(mod1, type = "stat", stat = "mean")
mean2 <- pp_check(mod2, type = "stat", stat = "mean")


# Error scatter
e_scat1 <- pp_check(mod1, type = "error_scatter_avg")
e_scat2 <- pp_check(mod2, type = "error_scatter_avg")

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Model comparison - LOO-PSIS
# =======================================================================


# Compute LOO-PSIS ------------------------------------------------------

# LOO-PSIS
loo1 <- loo(mod1)
loo2 <- loo(mod2)

# Save outputs
saveRDS(loo1,
        file = "./outputs/03_outputs_model-validation/02A1_loo.rds")
saveRDS(loo2,
        file = "./outputs/03_outputs_model-validation/02A2_loo.rds")



# Compare models --------------------------------------------------------

loo_tab <- loo_compare(loo1, loo2)

print(loo_tab, simplify = FALSE)

saveRDS(loo_tab,
        file = "./outputs/03_outputs_model-validation/loo_table_GAMM.rds")

# =======================================================================
# =======================================================================




