# =======================================================================

#                       DHMLM model diagnostics                         #

# =======================================================================





# =======================================================================
# 1. Load libraries, datasets, and models
# =======================================================================



# Librairies ------------------------------------------------------------

 library(data.table)
 library(brms)
 library(ggpubr)



# Import the model ------------------------------------------------------

 path <- file.path(getwd(), "outputs", "02_outputs_models")

 # Load models
 mod1 <- readRDS(file.path(path, "02B_DHMLM.rds"))

 # Read saved loo outputs
 #loo1 <- readRDS("./outputs/03_outputs_model-validation/03A1_loo.rds")

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
 # Convergence is attained for all the parameters


# Posterior predictive checks -------------------------------------------

 # Check distributions
 pp1a <- pp_check(mod1, resp = "speednovice")
 pp1b <- pp_check(mod1, resp = "speedinterm")
 pp1c <- pp_check(mod1, resp = "speedadvanced")
 pp1d <- pp_check(mod1, resp = "preyspeednovice")
 pp1e <- pp_check(mod1, resp = "preyspeedinterm")
 pp1f <- pp_check(mod1, resp = "preyspeedadvanced")
 pp1g <- pp_check(mod1, resp = "successnovice")
 pp1h <- pp_check(mod1, resp = "successinterm")
 pp1i <- pp_check(mod1, resp = "successadvanced")
 
 
 
 # Effects plot (mean variance plot)
 stat1a <- pp_check(mod1, type = "stat_2d", resp = "speednovice")
 stat1b <- pp_check(mod1, type = "stat_2d", resp = "speedinterm")
 stat1c <- pp_check(mod1, type = "stat_2d", resp = "speedadvanced")
 stat1d <- pp_check(mod1, type = "stat_2d", resp = "preyspeednovice")
 stat1e <- pp_check(mod1, type = "stat_2d", resp = "preyspeedinterm")
 stat1f <- pp_check(mod1, type = "stat_2d", resp = "preyspeedadvanced")
 stat1g <- pp_check(mod1, type = "stat_2d", resp = "successnovice")
 stat1h <- pp_check(mod1, type = "stat_2d", resp = "successinterm")
 stat1i <- pp_check(mod1, type = "stat_2d", resp = "successadvanced")
 
 # Predicted means
 mean1a <- pp_check(mod1, type = "stat", stat = "mean", resp = "speednovice")
 mean1b <- pp_check(mod1, type = "stat", stat = "mean", resp = "speedinterm")
 mean1c <- pp_check(mod1, type = "stat", stat = "mean", resp = "speedadvanced")
 mean1d <- pp_check(mod1, type = "stat", stat = "mean", resp = "preyspeednovice")
 mean1e <- pp_check(mod1, type = "stat", stat = "mean", resp = "preyspeedinterm")
 mean1f <- pp_check(mod1, type = "stat", stat = "mean", resp = "preyspeedadvanced")
 mean1g <- pp_check(mod1, type = "stat", stat = "mean", resp = "successnovice")
 mean1h <- pp_check(mod1, type = "stat", stat = "mean", resp = "successinterm")
 mean1i <- pp_check(mod1, type = "stat", stat = "mean", resp = "successadvanced")
 
 # Error scatter
 e_scat1a <- pp_check(mod1, type = "error_scatter_avg", resp = "speednovice")
 e_scat1b <- pp_check(mod1, type = "error_scatter_avg", resp = "speedinterm")
 e_scat1c <- pp_check(mod1, type = "error_scatter_avg", resp = "speedadvanced")
 e_scat1d <- pp_check(mod1, type = "error_scatter_avg", resp = "preyspeednovice")
 e_scat1e <- pp_check(mod1, type = "error_scatter_avg", resp = "preyspeedinterm")
 e_scat1f <- pp_check(mod1, type = "error_scatter_avg", resp = "preyspeedadvanced")
 e_scat1g <- pp_check(mod1, type = "error_scatter_avg", resp = "successnovice")
 e_scat1h <- pp_check(mod1, type = "error_scatter_avg", resp = "successinterm")
 e_scat1i <- pp_check(mod1, type = "error_scatter_avg", resp = "successadvanced")
 
 

# Save plots as figure --------------------------------------------------

 # Arrange a figure for each trait and stat
 plots <- list(
  # pp checks
  stat_fig1 <- ggarrange(
    pp1a, pp1b, pp1c,
    ncol = 2, nrow = 2
  ),
 
  stat_fig2 <- ggarrange(
    pp1d, pp1e, pp1f,
    ncol = 2, nrow = 2
  ),
  
  stat_fig3 <- ggarrange(
    pp1g, pp1h, pp1i,
    ncol = 2, nrow = 2
  ),
  
  # Effects plot (mean variance plot) 
  stat_fig4 <- ggarrange(
    stat1a, stat1b, stat1c,
    ncol = 2, nrow = 2
  ),
  
  stat_fig5 <- ggarrange(
    stat1d, stat1e, stat1f,
    ncol = 2, nrow = 2
  ),
  
  stat_fig6 <- ggarrange(
    stat1g, stat1h, stat1i,
    ncol = 2, nrow = 2
  ),
  
  
  # Predicted means
  stat_fig7 <- ggarrange(
    mean1a, mean1b, mean1c,
    ncol = 2, nrow = 2
  ),
  
  stat_fig8 <- ggarrange(
    mean1d, mean1e, mean1f,
    ncol = 2, nrow = 2
  ),
  
  stat_fig9 <- ggarrange(
    mean1g, mean1h, mean1i,
    ncol = 2, nrow = 2
  ),
  
  # Error scatter
  stat_fig10 <- ggarrange(
    e_scat1a, e_scat1b, e_scat1c,
    ncol = 2, nrow = 2
  ),
  
  stat_fig11 <- ggarrange(
    e_scat1d, e_scat1e, e_scat1f,
    ncol = 2, nrow = 2
  ),
  
  stat_fig12 <- ggarrange(
    e_scat1i, e_scat1h, e_scat1i,
    ncol = 2, nrow = 2
  )
 )
 
 

# Export the figures into a .pdf file
 path1 <- file.path(getwd(), "outputs", "03_outputs_model-validation")
 ggexport(
   plots,
   filename = file.path(path1, "03B_DHMLM-diagnostics.pdf")
   #width = 3000, height = 2500, res = 300
 )



# =======================================================================
# =======================================================================