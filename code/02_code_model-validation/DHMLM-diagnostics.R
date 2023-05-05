# =======================================================================

#                       DHMLM model diagnostics                         #

# =======================================================================





# =======================================================================
# 1. Load libraries, datasets, and models
# =======================================================================

# Global options --------------------------------------------------------

 # If you have multiple cores
 options(mc.cores = parallel::detectCores())



# Librairies ------------------------------------------------------------

 library(data.table)
 library(brms)
 library(ggpubr)
 library(tidybayes)



# Import the model ------------------------------------------------------

 path <- file.path(getwd(), "outputs", "01_outputs_models")

 # Load models
 mod1 <- readRDS(file.path(path, "B1_DHMLM-no-outlier.rds"))

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
 pp1a <- pp_check(mod1, resp = "speednovice") +
    ggtitle("speed nov")
 pp1b <- pp_check(mod1, resp = "speedinterm") +
    ggtitle("speed interm")
 pp1c <- pp_check(mod1, resp = "speedadvanced") +
    ggtitle("speed adv")
 pp1d <- pp_check(mod1, resp = "preyspeednovice") +
    ggtitle("prey speed nov")
 pp1e <- pp_check(mod1, resp = "preyspeedinterm") +
    ggtitle("prey speed interm")
 pp1f <- pp_check(mod1, resp = "preyspeedadvanced") +
    ggtitle("prey speed adv")
 pp1g <- pp_check(mod1, resp = "successnovice") +
    ggtitle("success nov")
 pp1h <- pp_check(mod1, resp = "successinterm") +
    ggtitle("success interm")
 pp1i <- pp_check(mod1, resp = "successadvanced") +
    ggtitle("success adv")

 # Effects plot (mean variance plot)
 stat1a <- pp_check(mod1, type = "stat_2d", resp = "speednovice") +
    ggtitle("speed nov")
 stat1b <- pp_check(mod1, type = "stat_2d", resp = "speedinterm") +
    ggtitle("speed interm")
 stat1c <- pp_check(mod1, type = "stat_2d", resp = "speedadvanced") +
    ggtitle("speed adv")
 stat1d <- pp_check(mod1, type = "stat_2d", resp = "preyspeednovice") +
    ggtitle("prey speed nov")
 stat1e <- pp_check(mod1, type = "stat_2d", resp = "preyspeedinterm") +
    ggtitle("prey speed interm")
 stat1f <- pp_check(mod1, type = "stat_2d", resp = "preyspeedadvanced") +
    ggtitle("prey speed adv")
 stat1g <- pp_check(mod1, type = "stat_2d", resp = "successnovice") +
    ggtitle("success nov")
 stat1h <- pp_check(mod1, type = "stat_2d", resp = "successinterm") +
    ggtitle("success interm")
 stat1i <- pp_check(mod1, type = "stat_2d", resp = "successadvanced") +
    ggtitle("success adv")

 # Predicted means
 mean1a <- pp_check(
  mod1, type = "stat",
  stat = "mean", resp = "speednovice"
 ) + ggtitle("speed nov")

 mean1b <- pp_check(
  mod1, type = "stat",
  stat = "mean", resp = "speedinterm"
 ) + ggtitle("speed interm")

 mean1c <- pp_check(
  mod1, type = "stat",
  stat = "mean", resp = "speedadvanced"
 ) + ggtitle("speed adv")

 mean1d <- pp_check(
  mod1, type = "stat",
  stat = "mean", resp = "preyspeednovice"
 ) + ggtitle("prey speed nov")

 mean1e <- pp_check(
  mod1, type = "stat",
  stat = "mean", resp = "preyspeedinterm"
 ) + ggtitle("prey speed interm")

 mean1f <- pp_check(
  mod1, type = "stat",
  stat = "mean", resp = "preyspeedadvanced"
 ) + ggtitle("prey speed adv")

 mean1g <- pp_check(
  mod1, type = "stat",
  stat = "mean", resp = "successnovice"
 ) + ggtitle("success nov")

 mean1h <- pp_check(
  mod1, type = "stat",
  stat = "mean", resp = "successinterm"
 ) + ggtitle("success interm")

 mean1i <- pp_check(
  mod1, type = "stat",
  stat = "mean", resp = "successadvanced"
 ) + ggtitle("success adv")


 # Error scatter
 e_scat1a <- pp_check(
  mod1, type = "error_scatter_avg", resp = "speednovice"
  ) + ggtitle("speed nov")

 e_scat1b <- pp_check(
  mod1, type = "error_scatter_avg", resp = "speedinterm"
  ) + ggtitle("speed interm")

 e_scat1c <- pp_check(
  mod1, type = "error_scatter_avg", resp = "speedadvanced"
  ) + ggtitle("speed adv")

 e_scat1d <- pp_check(
  mod1, type = "error_scatter_avg", resp = "preyspeednovice"
  ) + ggtitle("prey speed nov")

 e_scat1e <- pp_check(
  mod1, type = "error_scatter_avg", resp = "preyspeedinterm"
  ) + ggtitle("prey speed interm")

 e_scat1f <- pp_check(
  mod1, type = "error_scatter_avg", resp = "preyspeedadvanced"
  ) + ggtitle("prey speed adv")

 e_scat1g <- pp_check(
  mod1, type = "error_scatter_avg", resp = "successnovice"
  ) + ggtitle("success nov")

 e_scat1h <- pp_check(
  mod1, type = "error_scatter_avg", resp = "successinterm"
  ) + ggtitle("success interm")

 e_scat1i <- pp_check(
  mod1, type = "error_scatter_avg", resp = "successadvanced"
  ) + ggtitle("success adv")


 # Residuals vs fitted
 fitted1a <- fitted(mod1, resp = "speednovice")[, 1]
 resid1a <- residuals(mod1, resp = "speednovice")[, 1]
 assum1a <- ggplot() +
   geom_point(aes(x = fitted1a, y = resid1a), alpha = 0.2) +
   theme_bw() + theme(panel.grid = element_blank()) +
   ggtitle("speed nov")

 fitted1b <- fitted(mod1, resp = "speedinterm")[, 1]
 resid1b <- residuals(mod1, resp = "speedinterm")[, 1]
 assum1b <- ggplot() +
   geom_point(aes(x = fitted1b, y = resid1b), alpha = 0.2) +
   theme_bw() + theme(panel.grid = element_blank()) +
   ggtitle("speed interm")

 fitted1c <- fitted(mod1, resp = "speedadvanced")[, 1]
 resid1c <- residuals(mod1, resp = "speedadvanced")[, 1]
 assum1c <- ggplot() +
   geom_point(aes(x = fitted1c, y = resid1c), alpha = 0.2) +
   theme_bw() + theme(panel.grid = element_blank()) +
   ggtitle("speed adv")

 fitted1d <- fitted(mod1, resp = "preyspeednovice")[, 1]
 resid1d <- residuals(mod1, resp = "preyspeednovice")[, 1]
 assum1d <- ggplot() +
   geom_point(aes(x = fitted1d, y = resid1d), alpha = 0.2) +
   theme_bw() + theme(panel.grid = element_blank()) +
   ggtitle("prey speed nov")

 fitted1e <- fitted(mod1, resp = "preyspeedinterm")[, 1]
 resid1e <- residuals(mod1, resp = "preyspeedinterm")[, 1]
 assum1e <- ggplot() +
   geom_point(aes(x = fitted1e, y = resid1e), alpha = 0.2) +
   theme_bw() + theme(panel.grid = element_blank()) +
   ggtitle("prey speed interm")
 
 fitted1f <- fitted(mod1, resp = "preyspeedadvanced")[, 1]
 resid1f <- residuals(mod1, resp = "preyspeedadvanced")[, 1]
 assum1f <- ggplot() +
   geom_point(aes(x = fitted1f, y = resid1f), alpha = 0.2) +
   theme_bw() + theme(panel.grid = element_blank()) +
   ggtitle("prey speed adv")

 fitted1g <- fitted(mod1, resp = "successnovice")[, 1]
 resid1g <- residuals(mod1, resp = "successnovice")[, 1]
 assum1g <- ggplot() +
   geom_point(aes(x = fitted1g, y = resid1g), alpha = 0.2) +
   theme_bw() + theme(panel.grid = element_blank()) +
   ggtitle("success nov")

 fitted1h <- fitted(mod1, resp = "successinterm")[, 1]
 resid1h <- residuals(mod1, resp = "successinterm")[, 1]
 assum1h <- ggplot() +
   geom_point(aes(x = fitted1h, y = resid1h), alpha = 0.2) +
   theme_bw() + theme(panel.grid = element_blank()) +
   ggtitle("success interm")

 fitted1i <- fitted(mod1, resp = "successadvanced")[, 1]
 resid1i <- residuals(mod1, resp = "successadvanced")[, 1]
 assum1i <- ggplot() +
   geom_point(aes(x = fitted1i, y = resid1i), alpha = 0.2) +
   theme_bw() + theme(panel.grid = element_blank()) +
   ggtitle("success adv")



# Save plots as figure --------------------------------------------------

 # Arrange a figure for each trait and stat
 plots <- list(
  # pp checks
  stat_fig1 <- ggarrange(
    pp1a, pp1b, pp1c,
    ncol = 2, nrow = 2,
    common.legend = TRUE
  ),

  stat_fig2 <- ggarrange(
    pp1d, pp1e, pp1f,
    ncol = 2, nrow = 2,
    common.legend = TRUE
  ),

  stat_fig3 <- ggarrange(
    pp1g, pp1h, pp1i,
    ncol = 2, nrow = 2,
    common.legend = TRUE
  ),

  # Effects plot (mean variance plot)
  stat_fig4 <- ggarrange(
    stat1a, stat1b, stat1c,
    ncol = 2, nrow = 2,
    common.legend = TRUE
  ),

  stat_fig5 <- ggarrange(
    stat1d, stat1e, stat1f,
    ncol = 2, nrow = 2,
    common.legend = TRUE
  ),

  stat_fig6 <- ggarrange(
    stat1g, stat1h, stat1i,
    ncol = 2, nrow = 2,
    common.legend = TRUE
  ),


  # Predicted means
  stat_fig7 <- ggarrange(
    mean1a, mean1b, mean1c,
    ncol = 2, nrow = 2,
    common.legend = TRUE
  ),

  stat_fig8 <- ggarrange(
    mean1d, mean1e, mean1f,
    ncol = 2, nrow = 2,
    common.legend = TRUE
  ),

  stat_fig9 <- ggarrange(
    mean1g, mean1h, mean1i,
    ncol = 2, nrow = 2,
    common.legend = TRUE
  ),

  # Error scatter
  stat_fig10 <- ggarrange(
    e_scat1a, e_scat1b, e_scat1c,
    ncol = 2, nrow = 2,
    common.legend = TRUE
  ),

  stat_fig11 <- ggarrange(
    e_scat1d, e_scat1e, e_scat1f,
    ncol = 2, nrow = 2,
    common.legend = TRUE
  ),

  stat_fig12 <- ggarrange(
    e_scat1g, e_scat1h, e_scat1i,
    ncol = 2, nrow = 2,
    common.legend = TRUE
  ),

 # Residuals vs fitted plots
 stat_fig13 <- ggarrange(
    assum1a, assum1b, assum1c,
    ncol = 2, nrow = 2
  ),

  stat_fig14 <- ggarrange(
    assum1d, assum1e, assum1f,
    ncol = 2, nrow = 2
  ),

  stat_fig15 <- ggarrange(
    assum1g, assum1h, assum1i,
    ncol = 2, nrow = 2
  )
 )

# Export the figures into a .pdf file
 path1 <- file.path(getwd(), "outputs", "02_outputs_model-validation")
 ggexport(
   plots,
   filename = file.path(path1, "DHMLM-diagnostics.pdf")
 )

# =======================================================================
# =======================================================================