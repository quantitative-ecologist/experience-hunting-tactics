# =======================================================================

#                       GAMM model diagnostics                          #

# =======================================================================





# =======================================================================
# 1. Load libraries, datasets, and models
# =======================================================================



# Librairies ------------------------------------------------------------

library(data.table)
library(brms)
library(ggpubr)



# Import the model ------------------------------------------------------

# Load models
mod1 <- readRDS("./outputs/02_outputs_models/02A1_GAMM.rds")
mod2 <- readRDS("./outputs/02_outputs_models/02A2_GAMM.rds")
mod3 <- readRDS("./outputs/02_outputs_models/02A3_GLMM.rds")

# Check object size
print(object.size(mod1), units = "MB")
print(object.size(mod2), units = "MB")


# Read saved loo outputs
loo1 <- readRDS("./outputs/03_outputs_model-validation/02A1_loo.rds")
loo2 <- readRDS("./outputs/03_outputs_model-validation/02A2_loo.rds")

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
plot(mod3)


# Posterior predictive checks -------------------------------------------

# Check distributions
pp1 <- pp_check(mod1)
pp2 <- pp_check(mod2)
pp3 <- pp_check(mod3)
 
# Effects plot (mean variance plot)
stat1 <- pp_check(mod1, type = "stat_2d")
stat2 <- pp_check(mod2, type = "stat_2d")
stat3 <- pp_check(mod3, type = "stat_2d")


# Predicted means
mean1 <- pp_check(mod1, type = "stat", stat = "mean")
mean2 <- pp_check(mod2, type = "stat", stat = "mean")
mean3 <- pp_check(mod3, type = "stat", stat = "mean")


# Error scatter
e_scat1 <- pp_check(mod1, type = "error_scatter_avg")
e_scat2 <- pp_check(mod2, type = "error_scatter_avg")
e_scat3 <- pp_check(mod3, type = "error_scatter_avg")



# Save plots as figure --------------------------------------------------

# Arrange a figure
stat_fig1 <- ggarrange(pp1,
                       stat1,
                       mean1,
                       e_scat1,
                       ncol = 2, nrow = 2)

stat_fig2 <- ggarrange(pp2,    
                       stat2,
                       mean2,
                       e_scat2,
                       ncol = 2, nrow = 2)

stat_fig3 <- ggarrange(pp3,    
                       stat3,
                       mean3,
                       e_scat3,
                       ncol = 2, nrow = 2)

# Export the figure
ggexport(stat_fig1,
         filename = "./outputs/03_outputs_model-validation/03_GAMM-diagnostics1.png",
         width = 3000, height = 2500, res = 300) # more 

ggexport(stat_fig2,
         filename = "./outputs/03_outputs_model-validation/03_GAMM-diagnostics2.png",
         width = 3000, height = 2500, res = 300) # more 

ggexport(stat_fig3,
         filename = "./outputs/03_outputs_model-validation/03_GLMM-diagnostics.png",
         width = 3000, height = 2500, res = 300) # more 

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Model comparison - LOO-PSIS
# =======================================================================


# Compute LOO-PSIS ------------------------------------------------------

# LOO-PSIS
loo1 <- loo(mod1)
loo2 <- loo(mod2)
loo3 <- loo(mod3)

# Save outputs
saveRDS(loo1,
        file = "./outputs/03_outputs_model-validation/02A1_loo.rds")
saveRDS(loo2,
        file = "./outputs/03_outputs_model-validation/02A2_loo.rds")
saveRDS(loo3,
        file = "./outputs/03_outputs_model-validation/02A3_loo.rds")


# Compare models --------------------------------------------------------

# Compare models
loo_tab <- loo_compare(loo1, loo2, loo3)

# Compute table with complete information
loo_table <- print(loo_tab, simplify = FALSE)

# Save table
saveRDS(loo_table,
        file = "./outputs/03_outputs_model-validation/03_A_models-lootable.rds")

# =======================================================================
# =======================================================================