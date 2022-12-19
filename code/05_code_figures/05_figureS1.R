# ==========================================================================

#                          Plot 02A1 GAMM model

# ==========================================================================



# ==========================================================================
# 1. Prepare script
# ==========================================================================



# Load libraries and model -------------------------------------------------

 library(brms)
 library(data.table)
 library(ggplot2)
 library(ggpubr)
 
 path <- file.path(getwd(), "outputs", "02_outputs_models")

 mod1 <- readRDS(file.path(path, "02A1_GAMM.rds"))
 mod4 <- readRDS(file.path(path, "02A4_GAMM.rds"))
 mod7 <- readRDS(file.path(path, "02A7_GAMM.rds"))


 
# Load the data ------------------------------------------------------------

 data <- fread("./data/FraserFrancoetalXXXX-data.csv",
               select = c("predator_id",
                          "game_duration",
                          "pred_speed",
                          "prey_avg_speed",
                          "cumul_xp_pred",
                          "total_xp_pred",
                          "hunting_success",
                          "latency_1st_capture",
                          "chase_count",
                          "chase_count_success"))
 
 data[, predator_id := as.factor(predator_id)]



# Post processing preparations for custom family ---------------------------

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



# Setup a custom theme for the plot ----------------------------------------
 
 custom_theme <- theme(
   # axis values size
   axis.text.x = element_text(face = "plain", 
                              size = 12,
                              color = "black"),
   axis.text.y = element_text(face = "plain", 
                              size = 12,
                              color = "black"),
   # axis ticks lenght
   axis.ticks.length = unit(.15, "cm"),
   # axis ticks width
   axis.ticks = element_line(size = 0.90, 
                             color = "black"),
   # axis titles size
   axis.title = element_text(size = 14, 
                             face = "plain",
                             color = "black"),
   axis.line = element_line(size = 0.95,
                            color = "black"),
   legend.position = "none",
   panel.grid = element_blank(),
   panel.background = element_blank()
 )

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Plot 1 : GAMM fitted line
# ==========================================================================



# Prepare the plot ---------------------------------------------------------
 
 # Conditions back-transforms to probability scale

 # With intercept using built-in function
 fig1 <- conditional_smooths(mod1, method = "fitted", robust = FALSE)
 fig2 <- conditional_smooths(mod4, method = "fitted", robust = FALSE)
 fig3 <- conditional_smooths(mod7, method = "fitted", robust = FALSE)
 
 # Extract values in a table
 tab1 <- fig1$"mu: s(Zcumul_xp)"
 tab2 <- fig2$"mu: s(Zcumul_xp)"
 tab3 <- fig3$"mu: s(Zcumul_xp)"
 
 # Transform as data.table
  tab1 <- data.table(tab1)
  tab2 <- data.table(tab2)
  tab3 <- data.table(tab3)
  

 # Back transform x-axis values
 sequence <- (seq(0, 500, 100) - mean(data$cumul_xp_pred))
 standev <- sd(data$cumul_xp_pred)
 scaled_breaks <- sequence / standev
 
 seq1 <- seq(min(data$latency_1st_capture), 600, 120)
 sequence1 <- (seq(min(data$latency_1st_capture),
                   600, 120) 
               - mean(data$latency_1st_capture))
 standev1 <- sd(data$latency_1st_capture)
 scaled_breaks1 <- sequence1 / standev1


 
# Produce the plot --------------------------------------------------------
 
 # With conditional_effects, the predictions are on the original scale of y
 # With conditional_smooths, the predictions are on the link scale
 # conditional effects in GAMM context cannot handle the mean population trend
 # IDK why is that.

 gamm_plot1 <- ggplot(tab1,
                      aes(x = Zcumul_xp,
                          y = plogis(estimate__))) +
    geom_ribbon(aes(x = Zcumul_xp,
                    ymin = plogis(lower__),
                    ymax = plogis(upper__)),
                alpha = 0.5,
                fill = "gray") +
    geom_line(#linetype = "dashed",
       size = 1,
       color = "black") +
    ylab("Prey captured") +
    scale_y_continuous(breaks = seq(0, 1, 0.25),
                       limits = c(0, 1)) +
    scale_x_continuous(breaks = scaled_breaks,
                       labels = seq(0, 500, 100)) +
    xlab("Cumulative experience") +
    custom_theme
 
 gamm_plot2 <- ggplot(tab2,
                      aes(x = Zcumul_xp,
                          y = estimate__)) +
    geom_ribbon(aes(x = Zcumul_xp,
                    ymin = lower__,
                    ymax = upper__),
                alpha = 0.5,
                fill = "gray") +
    geom_line(#linetype = "dashed",
       size = 1,
       color = "black") +
    ylab(expression(paste("Latency before the ", 1^st, " capture"))) +
    scale_y_continuous(breaks = scaled_breaks1,
                       labels = round(seq1, digits = 1),
                       limits = c(-2, 3.0497684)) +
    scale_x_continuous(breaks = scaled_breaks,
                       labels = seq(0, 500, 100)) +
    xlab("Cumulative experience") +
    custom_theme
 
 gamm_plot3 <- ggplot(tab3,
                      aes(x = Zcumul_xp,
                          y = plogis(estimate__))) +
    geom_ribbon(aes(x = Zcumul_xp,
                    ymin = plogis(lower__),
                    ymax = plogis(upper__)),
                alpha = 0.5,
                fill = "gray") +
    geom_line(#linetype = "dashed",
       size = 1,
       color = "black") +
    ylab("Chase success ratio") +
    scale_y_continuous(breaks = seq(0, 1, 0.25),
                       limits = c(0, 1)) +
    scale_x_continuous(breaks = scaled_breaks,
                       labels = seq(0, 500, 100)) +
    xlab("Cumulative experience") +
    custom_theme

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Combine plots into 1 figure
# ==========================================================================



# Prepare figure ------------------------------------------------------------

 
 # Arrange paneled figure
 figure <- ggarrange(
    #gamm_plot1, NULL, gamm_plot3, NULL, gamm_plot2,
    #ncol = 5, nrow = 1,
    #labels = c("(A)", "", "(B)", "", "(C)"),
    #widths = c(1.5, 0.1, 1.5, 0.1, 1.5),
    NULL, gamm_plot1, NULL, gamm_plot3, NULL, gamm_plot2,
    ncol = 6, nrow = 1,
    labels = c("(A)", "", "(B)", "", "(C)", ""),
    widths = c(0.15, 1.5, 0.15, 1.5, 0.15, 1.5)
 )
 
# Export the figure -----------------------------------------------------

 path <- file.path(getwd(), "outputs", "05_outputs_figures")
 
 ggexport(figure,
          filename = file.path(path, "05_figureS1.png"),
          width = 4200,
          height = 1300,
          res = 300)

# ==========================================================================
# ==========================================================================