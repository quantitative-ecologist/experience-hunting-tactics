# ==========================================================================

#                          Plot 02A1 GAMM model

# ==========================================================================



# ==========================================================================
# 1. Prepare script
# ==========================================================================



# Load librairies and model ------------------------------------------------

library(brms)
library(data.table)
library(ggplot2)

mod1 <- readRDS("./outputs/02_outputs_models/02A1_GAMM.rds")



# Load the data ------------------------------------------------------------

data <- fread("./data/FraserFrancoetalXXXX-data.csv",
              select = c("predator_id",
                         "pred_game_duration",
                         "pred_speed",
                         "prey_avg_speed",
                         "cumul_xp_killer",
                         "total_xp_killer",
                         "hunting_success"))

data <- unique(data)



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
 custom_theme <- theme(# axis values size
                       axis.text.x = element_text(face = "plain", 
                                                  size = 15,
                                                  color = "black"),
                       axis.text.y = element_text(face = "plain", 
                                                  size = 15,
                                                  color = "black"),
                       # axis ticks lenght
                       axis.ticks.length = unit(.15, "cm"),
                       # axis ticks width
                       axis.ticks = element_line(size = 0.90, 
                                                 color = "black"),
                       # axis titles size
                       axis.title = element_text(size = 17, 
                                                 face = "plain",
                                                 color = "black"),
                       axis.line = element_line(size = 0.95,
                                                color = "black"),
                       legend.position = "none",
                       panel.grid = element_blank(),
                       panel.background = element_blank())

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Plot 1 : GAMM fitted line
# ==========================================================================



# Prepare the plot ---------------------------------------------------------
 
 # With intercept using built-in function
 fig1 <- conditional_effects(mod1, method = "fitted", robust = FALSE)

 # Extract values in a table
 tab <- fig1$Zcumul_xp

 # Transform as data.table
  tab <- data.table(tab)

 # Back transform x-axis values
 sequence <- (seq(0, 500, 100) - mean(data$cumul_xp_killer))
 standev <- sd(data$cumul_xp_killer)
 scaled_breaks <- sequence / standev



# Produce the plot --------------------------------------------------------

 gamm_plot <- ggplot(tab,
                     aes(x = Zcumul_xp,
                         y = estimate__)) +
     geom_ribbon(aes(x = Zcumul_xp,
                     ymin = lower__,
                     ymax = upper__),
                 alpha = 0.5,
                 fill = "gray") +
     geom_line(#linetype = "dashed",
               size = 1.5,
               color = "black") +
     ylab("Hunting success\n") +
     scale_y_continuous(breaks = seq(0, 4, 1),
                        limits = c(0, 4)) +
     scale_x_continuous(breaks = scaled_breaks,
                        labels = seq(0, 500, 100)) +
     xlab("\nCumulative experience") +
     custom_theme

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Plot 2 : the average hunting success of players
# ==========================================================================



# Prepare the plot ---------------------------------------------------------

 # Extract draws from each player
 draws <- data.table(
     as_draws_df(
         mod1,
         variable = c("^s_spredator_id_1"),
         regex = TRUE
     )
 )
 
 draws[, c(275:277) := NULL]
 
 # Rename columns
 colnames(draws) <- as.character(seq(1, 274, 1))

 # Flip the table
 draws <- melt(draws,
               variable.name = "predator_id")
 
 # Transform predicted y values to original scale
 draws[, value := plogis(value)]

 # Compute the average for each player + 95% CI in a new table
 lower_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[1]}
 upper_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[2]}
 
 tab1 <- draws[, .(mean = mean(value),
                   lower_ci = lower_interval(value),
                   upper_ci = upper_interval(value)),
                  by = predator_id]

 # Back transform x-axis values
 scaled_breaks1 <- (seq(0, 4, 1) / 4)



# Produce the plot --------------------------------------------------------

 # Add total xp in table
 total_xp <- unique(data[, .(predator_id, total_xp_killer)])
 tab1 <- cbind(tab1, total_xp = total_xp$total_xp)
 tab1[total_xp < 150, expertise := "Below 150"]
 tab1[total_xp %between% c(150, 299), expertise := "Between 150 and 300"]
 tab1[total_xp %between% c(300, 500), expertise := "Above 300"]
 
 # Reorder the factors
 tab1$expertise <- factor(tab1$expertise, 
                          levels = c("Below 150",
                                     "Between 150 and 300",
                                     "Above 300"))
 
 id_plot <- ggplot(tab1,
                   aes(x = predator_id,
                       y = mean,
                       color = expertise,
                       shape = expertise)) +
      geom_pointrange(aes(ymin = lower_ci,
                          ymax = upper_ci)) +
      #geom_hline(intercept = 
      #           linetype = "dashed",
      #           size = 1,
      #           color = "black") +
      ylab("Hunting success\n") +
      xlab("\nPredator ID") +
      scale_y_continuous(breaks = scaled_breaks1,
                         labels = seq(0, 4, 1),
                         limits = c(0, 1)) +
      scale_shape_manual(name = "Total experience :",
                         values = c(15, 16, 17)) +
      #scale_color_manual(name = "Experience :",
      #                   values = c("#999999", "#FFDB6D", "#00AFBB")) +
      scale_color_manual(name = "Total experience :",
                         values = c("#999999", "#E69F00", "#00AFBB")) +
      custom_theme +
      theme(#axis.text.x = element_blank(),
            #axis.ticks.x = element_blank(),
            axis.text.x = element_text(color = "transparent"),
            axis.ticks.x = element_line(color = "transparent"),
            legend.position = "top",
            legend.key = element_rect(fill = "transparent"),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 12))

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Combine plots into 1 figure
# ==========================================================================



# Prepare figure ------------------------------------------------------------

 # Load library
 library(ggpubr)
 
 # Arrange paneled figure
 figure <- ggarrange(gamm_plot, id_plot,
                     ncol = 2, nrow = 1,
                     labels = c("(A)", "(B)"),
                     common.legend = TRUE,
                     legend = "top")
 
# Export the figure -----------------------------------------------------

 ggexport(figure,
          filename = "./outputs/05_outputs_figures/05_figure1.png",
          width = 3500,
          height = 1800,
          res = 300)

# ==========================================================================
# ==========================================================================