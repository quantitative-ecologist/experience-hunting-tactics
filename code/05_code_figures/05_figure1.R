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
 library(viridis)
 
 path <- file.path(getwd(), "outputs", "02_outputs_models")

 mod1 <- readRDS(file.path(path, "02A3_GAMM.rds"))
 mod2 <- readRDS(file.path(path, "02A3_prey-GAMM.rds"))


 
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
    axis.text = element_text(face = "plain", 
                             size = 14,
                             color = "black"),
    # axis ticks lenght
    axis.ticks.length = unit(.15, "cm"),
    # axis ticks width
    axis.ticks = element_line(size = 0.90, 
                              color = "black"),
    # axis titles size
    axis.title = element_text(size = 16, 
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

 # Using built-in function
 # With conditional_smooths, the predictions are on the link scale
 fig1 <- conditional_smooths(mod1, method = "fitted", robust = FALSE)
 fig2 <- conditional_smooths(mod2, method = "fitted", robust = FALSE)
 
 # Extract values in a table
 tab1 <- fig1[[1]]
 tab2 <- fig2[[1]]
 
 # Transform as data.table
  tab1 <- data.table(tab1)
  tab2 <- data.table(tab2)
  

 # Back transform x-axis values
 sequence <- (seq(0, 500, 100) - mean(data$cumul_xp_pred))
 standev <- sd(data$cumul_xp_pred)
 scaled_breaks <- sequence / standev

 
 
# Cut fitted values based on player XP ------------------------------------

 # Extract player IDs with their total XP from the original data
 xp <- unique(data[,.(predator_id, total_xp_pred)])
 
 # Compute non standardized cumulative XP
 tab1[, cumul_xp := (Zcumul_xp*standev)+mean(data$cumul_xp_pred)]
 tab2[, cumul_xp := (Zcumul_xp*standev)+mean(data$cumul_xp_pred)]
 
 # Now merge the two tables adding the total XP
 tab1 <- merge(tab1, xp, by = "predator_id")
 tab2 <- merge(tab2, xp, by = "predator_id")
 
 # Cut all matches where fitted values are above total XP
 tab1 <- tab1[cumul_xp <= total_xp_pred,]
 tab2 <- tab2[cumul_xp <= total_xp_pred,]
 
 
 
# Produce the plot --------------------------------------------------------

 gamm_plot1 <- ggplot(tab1,
                      aes(x = Zcumul_xp,
                          y = plogis(estimate__),
                          color = predator_id)) +
    geom_line(size = 1) +
    scale_color_viridis(discrete = TRUE, option = "D") + #B
    ylab("Hunting success\n") +
    scale_y_continuous(breaks = seq(0, 1, 0.25),
                       limits = c(0, 1)) +
    scale_x_continuous(breaks = scaled_breaks,
                       labels = seq(0, 500, 100)) +
    xlab("\nCumulative experience") +
    custom_theme
 
 gamm_plot2 <- ggplot(tab2,
                      aes(x = Zcumul_xp,
                          y = plogis(estimate__),
                          color = predator_id)) +
    geom_line(size = 1) +
    scale_color_viridis(discrete = TRUE, option = "D") + #B
    ylab("Hunting success\n") +
    scale_y_continuous(breaks = seq(0, 1, 0.25),
                       limits = c(0, 1)) +
    scale_x_continuous(breaks = scaled_breaks,
                       labels = seq(0, 500, 100)) +
    xlab("\nCumulative experience") +
    custom_theme

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Combine plots into 1 figure
# ==========================================================================



# Prepare figure ------------------------------------------------------------

 
 # Arrange paneled figure
 figure <- ggarrange(
    NULL, gamm_plot1, NULL, gamm_plot2,
    ncol = 4, nrow = 1,
    labels = c("(A)", "", "(B)", ""),
    widths = c(0.15, 1.5, 0.15, 1.5)
 )
 
# Export the figure -----------------------------------------------------

 path <- file.path(getwd(), "outputs", "05_outputs_figures")
 
 ggexport(figure,
          filename = file.path(path, "05_figure1.png"),
          width = 3000,
          height = 1200,
          res = 300)

# ==========================================================================
# ==========================================================================