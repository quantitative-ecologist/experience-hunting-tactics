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

 path <- file.path(getwd(), "outputs", "01_outputs_models")

 # Load models
 mod1 <- readRDS(file.path(path, "A1_GAMM.rds"))
 mod2 <- readRDS(file.path(path, "A2_GAMM.rds"))
 mod3 <- readRDS(file.path(path, "A3_GAMM.rds"))
 mod4 <- readRDS(file.path(path, "A2_GAMM-prey.rds"))
 mod5 <- readRDS(file.path(path, "A3_GAMM-prey.rds"))

 # Check object size
 print(object.size(mod1), units = "MB")
 print(object.size(mod2), units = "MB")
 print(object.size(mod3), units = "MB")
 print(object.size(mod4), units = "MB")
 print(object.size(mod5), units = "MB")

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
   trials <- matrix(trials,
                    nrow = nrow(mu),
                    ncol = ncol(mu),
                    byrow = TRUE)
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
 plot(mod4)
 plot(mod5)



# Posterior predictive checks -------------------------------------------

 # Check distributions
 pp1 <- pp_check(mod1)
 pp2 <- pp_check(mod2)
 pp3 <- pp_check(mod3)
 pp4 <- pp_check(mod4)
 pp5 <- pp_check(mod5)

 # Effects plot (mean variance plot)
 stat1 <- pp_check(mod1, type = "stat_2d")
 stat2 <- pp_check(mod2, type = "stat_2d")
 stat3 <- pp_check(mod3, type = "stat_2d")
 stat4 <- pp_check(mod4, type = "stat_2d")
 stat5 <- pp_check(mod5, type = "stat_2d")

 # Predicted means
 mean1 <- pp_check(mod1, type = "stat", stat = "mean")
 mean2 <- pp_check(mod2, type = "stat", stat = "mean")
 mean3 <- pp_check(mod3, type = "stat", stat = "mean")
 mean4 <- pp_check(mod4, type = "stat", stat = "mean")
 mean5 <- pp_check(mod5, type = "stat", stat = "mean")


 # Error scatter
 e_scat1 <- pp_check(mod1, type = "error_scatter_avg")
 e_scat2 <- pp_check(mod2, type = "error_scatter_avg")
 e_scat3 <- pp_check(mod3, type = "error_scatter_avg")
 e_scat4 <- pp_check(mod4, type = "error_scatter_avg")
 e_scat5 <- pp_check(mod5, type = "error_scatter_avg")



# Save plots as figure --------------------------------------------------

 plots <- list(
  # Arrange a figure
  stat_fig1 <- ggarrange(
   pp1, stat1, mean1, e_scat1,
   ncol = 2, nrow = 2
  ),

  stat_fig2 <- ggarrange(
   pp2, stat2, mean2, e_scat2,
   ncol = 2, nrow = 2
  ),

  stat_fig3 <- ggarrange(
   pp3, stat3, mean3, e_scat3,
   ncol = 2, nrow = 2
  ),

  stat_fig4 <- ggarrange(
   pp4, stat4, mean4, e_scat4,
   ncol = 2, nrow = 2
  ),

  stat_fig5 <- ggarrange(
   pp5, stat5, mean5, e_scat5,
   ncol = 2, nrow = 2
  )
 )

  # Export the figures into a .pdf file
  ggexport(
    plots,
    filename = file.path(path1, "A1-3_GAMM-diagnostics.pdf")
  )


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
 loo4 <- loo(mod4)
 loo5 <- loo(mod5)



# Compare models --------------------------------------------------------

 # Compare models
 loo_tab <- loo_compare(loo1, loo2, loo3, loo4, loo5)

 # Compute table with complete information
 loo_table <- print(loo_tab, simplify = FALSE)



# Save outputs ----------------------------------------------------------

 # Folder path where I will save the outputs
 path1 <- file.path(getwd(), "outputs", "02_outputs_model-validation")

 # Save loo
 #saveRDS(loo1, file = file.path(path1, "A1_loo.rds"))
 #saveRDS(loo2, file = file.path(path1, "A2_loo.rds"))
 #saveRDS(loo3, file = file.path(path1, "A3_loo.rds"))
 #saveRDS(loo4, file = file.path(path1, "A2_loo-prey.rds"))
 #saveRDS(loo5, file = file.path(path1, "A3_loo-prey.rds"))

 # Save table
 saveRDS(loo_table, file = file.path(path1, "A1-3_loo-cv-table.rds"))

# =======================================================================
# =======================================================================