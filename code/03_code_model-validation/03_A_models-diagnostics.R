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
 
 path <- file.path(getwd(), "outputs", "02_outputs_models")

 # Load models
 mod1 <- readRDS(file.path(path, "02A1_GAMM.rds"))
 mod2 <- readRDS(file.path(path, "02A2_GAMM.rds"))
 mod3 <- readRDS(file.path(path, "02A3_GAMM.rds"))

 mod4 <- readRDS(file.path(path, "02A4_GAMM.rds"))
 mod5 <- readRDS(file.path(path, "02A5_GAMM.rds"))
 mod6 <- readRDS(file.path(path, "02A6_GAMM.rds"))

 mod7 <- readRDS(file.path(path, "02A7_GAMM.rds"))
 mod8 <- readRDS(file.path(path, "02A8_GAMM.rds"))
 mod9 <- readRDS(file.path(path, "02A9_GAMM.rds"))
 
 # Check object size
 print(object.size(mod1), units = "MB")
 print(object.size(mod2), units = "MB")
 print(object.size(mod3), units = "MB")
 
 print(object.size(mod4), units = "MB")
 print(object.size(mod5), units = "MB")
 print(object.size(mod6), units = "MB")

 print(object.size(mod7), units = "MB")
 print(object.size(mod8), units = "MB")
 print(object.size(mod9), units = "MB")

 # Read saved loo outputs
 #loo1 <- readRDS("./outputs/03_outputs_model-validation/03A1_loo.rds")
 #loo2 <- readRDS("./outputs/03_outputs_model-validation/03A2_loo.rds")
 #loo3 <- readRDS("./outputs/03_outputs_model-validation/03A3_loo.rds")

 # Folder path where I will save the outputs
 path1 <- "./outputs/03_outputs_model-validation"

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
 plot(mod6)

 plot(mod7)
 plot(mod8)
 plot(mod9)



# Posterior predictive checks -------------------------------------------

 # Check distributions
 pp1 <- pp_check(mod1)
 pp2 <- pp_check(mod2)
 pp3 <- pp_check(mod3)

 pp4 <- pp_check(mod4)
 pp5 <- pp_check(mod5)
 pp6 <- pp_check(mod6)
 
 pp7 <- pp_check(mod7)
 pp8 <- pp_check(mod8)
 pp9 <- pp_check(mod9)

 # Effects plot (mean variance plot)
 stat1 <- pp_check(mod1, type = "stat_2d")
 stat2 <- pp_check(mod2, type = "stat_2d")
 stat3 <- pp_check(mod3, type = "stat_2d")

 stat4 <- pp_check(mod4, type = "stat_2d")
 stat5 <- pp_check(mod5, type = "stat_2d")
 stat6 <- pp_check(mod6, type = "stat_2d")
 
 stat7 <- pp_check(mod7, type = "stat_2d")
 stat8 <- pp_check(mod8, type = "stat_2d")
 stat9 <- pp_check(mod9, type = "stat_2d")
 
 # Predicted means
 mean1 <- pp_check(mod1, type = "stat", stat = "mean")
 mean2 <- pp_check(mod2, type = "stat", stat = "mean")
 mean3 <- pp_check(mod3, type = "stat", stat = "mean")

 mean4 <- pp_check(mod4, type = "stat", stat = "mean")
 mean5 <- pp_check(mod5, type = "stat", stat = "mean")
 mean6 <- pp_check(mod6, type = "stat", stat = "mean")

 mean7 <- pp_check(mod7, type = "stat", stat = "mean")
 mean8 <- pp_check(mod8, type = "stat", stat = "mean")
 mean9 <- pp_check(mod9, type = "stat", stat = "mean")
 
 
 # Error scatter
 e_scat1 <- pp_check(mod1, type = "error_scatter_avg")
 e_scat2 <- pp_check(mod2, type = "error_scatter_avg")
 e_scat3 <- pp_check(mod3, type = "error_scatter_avg")
 
 e_scat4 <- pp_check(mod4, type = "error_scatter_avg")
 e_scat5 <- pp_check(mod5, type = "error_scatter_avg")
 e_scat6 <- pp_check(mod6, type = "error_scatter_avg")

 e_scat7 <- pp_check(mod7, type = "error_scatter_avg")
 e_scat8 <- pp_check(mod8, type = "error_scatter_avg")
 e_scat9 <- pp_check(mod9, type = "error_scatter_avg")



# Save plots as figure --------------------------------------------------

 # Arrange a figure
 stat_fig1 <- ggarrange(
  pp1, stat1, mean1, e_scat1,
  ncol = 2, nrow = 2
 )

 stat_fig2 <- ggarrange(
  pp2, stat2, mean2, e_scat2,
  ncol = 2, nrow = 2
 )

 stat_fig3 <- ggarrange(
  pp3, stat3, mean3, e_scat3,
  ncol = 2, nrow = 2
 )


 stat_fig4 <- ggarrange(
  pp4, stat4, mean4, e_scat4,
  ncol = 2, nrow = 2
 )

 stat_fig5 <- ggarrange(
  pp5, stat5, mean5, e_scat5,
  ncol = 2, nrow = 2
 )

 stat_fig6 <- ggarrange(
  pp6, stat6, mean6, e_scat6,
  ncol = 2, nrow = 2
 )


 stat_fig7 <- ggarrange(
  pp7, stat7, mean7, e_scat7,
  ncol = 2, nrow = 2
 )

 stat_fig8 <- ggarrange(
  pp8, stat8, mean8, e_scat8,
  ncol = 2, nrow = 2
 )

 stat_fig9 <- ggarrange(
  pp9, stat9, mean9, e_scat9,
  ncol = 2, nrow = 2
 )
 

 # Export the figure
 ggexport(
  stat_fig1,
  filename = file.path(path1, "03A1_GAMM-diagnostics.png"),
  width = 3000, height = 2500, res = 300
 )
 
 ggexport(
  stat_fig2,
  filename = file.path(path1, "03A2_GAMM-diagnostics.png"),
  width = 3000, height = 2500, res = 300
 )
 
 ggexport(
  stat_fig3,
  filename = file.path(path1, "03A3_GAMM-diagnostics.png"),
  width = 3000, height = 2500, res = 300
 )
 

 ggexport(
  stat_fig4,
  filename = file.path(path1, "03A4_GAMM-diagnostics.png"),
  width = 3000, height = 2500, res = 300
 )
 
 ggexport(
  stat_fig5,
  filename = file.path(path1, "03A5_GAMM-diagnostics.png"),
  width = 3000, height = 2500, res = 300
 )
 
 ggexport(
  stat_fig6,
  filename = file.path(path1, "03A6_GAMM-diagnostics.png"),
  width = 3000, height = 2500, res = 300
 )


 ggexport(
  stat_fig7,
  filename = file.path(path1, "03A7_GAMM-diagnostics.png"),
  width = 3000, height = 2500, res = 300
 )
 
 ggexport(
  stat_fig8,
  filename = file.path(path1, "03A8_GAMM-diagnostics.png"),
  width = 3000, height = 2500, res = 300
 )
 
 ggexport(
  stat_fig9,
  filename = file.path(path1, "03A9_GAMM-diagnostics.png"),
  width = 3000, height = 2500, res = 300
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
 loo6 <- loo(mod6)

 loo7 <- loo(mod7)
 loo8 <- loo(mod8)
 loo9 <- loo(mod9)


 # Save outputs
 saveRDS(loo1, file = file.path(path1, "03A1_loo.rds"))
 saveRDS(loo2, file = file.path(path1, "03A2_loo.rds"))
 saveRDS(loo3, file = file.path(path1, "03A3_loo.rds"))
 
 saveRDS(loo4, file = file.path(path1, "03A4_loo.rds"))
 saveRDS(loo5, file = file.path(path1, "03A5_loo.rds")
 saveRDS(loo6, file = file.path(path1, "03A6_loo.rds"))

 saveRDS(loo7, file = file.path(path1, "03A7_loo.rds"))
 saveRDS(loo8, file = file.path(path1, "03A8_loo.rds"))
 saveRDS(loo9, file = file.path(path1, "03A9_loo.rds"))        



# Compare models --------------------------------------------------------

 # Compare models
 loo_tab1 <- loo_compare(loo1, loo2, loo3)
 loo_tab2 <- loo_compare(loo4, loo5, loo6)
 loo_tab3 <- loo_compare(loo7, loo8, loo9)
 
 # Compute table with complete information
 loo_table1 <- print(loo_tab1, simplify = FALSE)
 loo_table2 <- print(loo_tab2, simplify = FALSE)
 loo_table3 <- print(loo_tab3, simplify = FALSE)
 
 # Save table
 saveRDS(loo_table1, file = file.path(path1, "03A_lootable1.rds"))
 saveRDS(loo_table2, file = file.path(path1, "03A_lootable2.rds"))
 saveRDS(loo_table3, file = file.path(path1, "03A_lootable3.rds"))

# =======================================================================
# =======================================================================