# ==========================================================================

#                         expertise-variance plot

# ==========================================================================





# ==========================================================================
# 1. Prepare script
# ==========================================================================


# Load libraries and models ------------------------------------------------

library(brms)
library(data.table)
library(ggplot2)
library(ggridges)
library(ggpubr)

path <- file.path(getwd(), "outputs", "02_outputs_models")

fit1 <- readRDS(file.path(path, "02A3_GAMM.rds"))
fit2 <- readRDS(file.path(path, "02B_DHMLM.rds"))



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

expose_functions(fit1, vectorize = TRUE)

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





# =======================================================================
# 2. Extract posterior draws for predator speed
# =======================================================================


# Extract sigma values --------------------------------------------------

draws <- data.table(
  as_draws_df(
    fit2,
    variable = c("r_predator_id__sigma_speed"),
    regex = TRUE)
)

draws[, c(1:33, 287:539) := NULL]
draws[, c(507:509) := NULL]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Reshape the draws table
# =======================================================================

# Reshape sigma speed ---------------------------------------------------

# Long format
draws <- melt(draws,
              measure = patterns("^r_predator_id"),
              variable.name = "predator_id")

# Add experience level
draws[, xp_level := ifelse(predator_id %like% "novice",
                           "novice",
                           "advanced")]

# Add predator ID
draws[, predator_id := as.character(predator_id)]
draws[, predator_id := gsub("[]_,a-zA-Z,[]", "", predator_id)]
draws[, predator_id := gsub(" ", "", paste("pred", predator_id))]
draws[, predator_id := as.factor(predator_id)]

# Rename value to speed
setnames(draws, "value", "speed_sigma")



# Transform  ------------------------------------------------------------

# Add population intercept
int1 <- fixef(fit2, pars = "sigma_speednovice_Intercept")[1]
int2 <- fixef(fit2, pars = "sigma_speedadvanced_Intercept")[1]

draws[xp_level == "novice", speed_sigma := speed_sigma + int1]
draws[xp_level == "advanced", speed_sigma := speed_sigma + int2]

# Back transform to original scale (sigma is on log scale)
draws[, exp_speed_sigma := exp(speed_sigma)]


# Calculate mean predicted value for each individual
draws <- draws[, average_speed_sigma := mean(exp_speed_sigma),
               by = c("predator_id",
                      "xp_level")]

# =======================================================================
# =======================================================================





# ==========================================================================
# 4. Prepare data for the plots
# ==========================================================================

fig1 <- conditional_smooths(fit1, method = "fitted", robust = FALSE)

# Extract values in a table
tab1 <- fig1[[1]]

# Transform as data.table
tab1 <- data.table(tab1)

# Calculate 1st and final value
tab1[, ":=" (
  first = min(Zcumul_xp), 
  last = max(Zcumul_xp)), 
  by = predator_id]

tab1[, equal1 := ifelse(Zcumul_xp == first, 1, 0), by = predator_id]
tab1[, equal2 := ifelse(Zcumul_xp == last, 1, 0), by = predator_id]

tab2 <- tab1[equal1 == 1 | equal2 == 1, .(predator_id, Zcumul_xp, estimate__)]
tab2[, range := rep(c("min", "max"), 253)]
tab2[, estimate__ := plogis(estimate__)]
tab2[, Zcumul_xp := NULL]

tab2 <- dcast(
  tab2,
  predator_id ~ range,
  value.var = "estimate__"
  )

# Calculate the difference in predicted success between minimum and maximum xp
tab2[, difference := max - min]

table <- merge(
  tab1[, .(predator_id, Zcumul_xp, estimate__)], 
  tab2[, .(predator_id, difference)], 
  by = "predator_id"
)

# Extract a selection of players (greatest increase and decrease)
table <- unique(
  table[
    difference > 0.1 | difference < -0.1, 
    .(predator_id, difference, Zcumul_xp, estimate__)
  ]
)
draws <- draws[, .(predator_id, xp_level, exp_speed_sigma, average_speed_sigma)]
dat <- merge(draws, unique(table[, .(predator_id, difference)]), by = "predator_id")
dat[, difference := ifelse(difference > 0.1, 1, 0)]

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Produce the GAMM plots
# ==========================================================================

# Back transform x-axis values
sequence <- (seq(0, 500, 100) - mean(data$cumul_xp_pred))
standev <- sd(data$cumul_xp_pred)
scaled_breaks <- sequence / standev


# Cut fitted values based on player XP ------------------------------------

# Extract player IDs with their total XP from the original data
xp <- unique(data[,.(predator_id, total_xp_pred)])

# Compute non standardized cumulative XP
table[, cumul_xp := (Zcumul_xp*standev)+mean(data$cumul_xp_pred)]

# Now merge the two tables adding the total XP
table <- merge(table, xp, by = "predator_id")

# Cut all matches where fitted values are above total XP
table <- table[cumul_xp <= total_xp_pred,]



# Plot for players that had an increase in success -------------------------

length(unique(table[difference > 0, predator_id]))
# 56 players had an increase in hunting success

gamm_plot1 <- ggplot(table[difference > 0.1,],
                     aes(x = Zcumul_xp,
                         y = plogis(estimate__),
                         color = predator_id)) +
  geom_line(size = 1) +
  viridis::scale_color_viridis(discrete = TRUE, option = "D") + #B
  ylab("Hunting success\n") +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = scaled_breaks,
                     labels = seq(0, 500, 100)) +
  xlab("\nCumulative experience") +
  custom_theme



# Plot for players that had a reduction in success -------------------------

length(unique(table[difference < 0, predator_id]))
# 27 players had an increase in hunting success

gamm_plot2 <- ggplot(table[difference < -0.1,],
                     aes(x = Zcumul_xp,
                         y = plogis(estimate__),
                         color = predator_id)) +
  geom_line(size = 1) +
  viridis::scale_color_viridis(discrete = TRUE, option = "D") + #B
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
# 4. Produce the distributions plots
# ==========================================================================

# Plot the distributions of novice players
plot1 <- ggplot() +
  
  geom_density_ridges(data = dat[xp_level == "novice" & difference == 1],
                      rel_min_height = 0.005,
                      fill = "#999999",
                      aes(x = exp_speed_sigma,
                          y = predator_id,
                          height = ..density..,
                          scale = 3)) +
  
  geom_point(data = unique(dat[xp_level == "novice" & difference == 1, c(1, 4)]),
             aes(x = average_speed_sigma, 
                 y = predator_id),
             size = 1,
             color = "black") +
  scale_x_continuous(breaks = seq(0, 2, 0.5),
                     limits = c(0, 2.05)) +
  #scale_x_continuous(breaks = scaled_breaks1,
  #                   labels = scaleFUN,
  #                   limits = c(0, 4)) +
  
  ylab("Predator ID\n") +
  xlab("\nIntra individual SD (m/s)") +
  
  custom_theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15,
                                  face = "bold"))

plot2 <- ggplot() +
  
  geom_density_ridges(data = dat[xp_level == "novice" & difference == 0],
                      rel_min_height = 0.005,
                      fill = "#999999",
                      aes(x = exp_speed_sigma,
                          y = predator_id,
                          height = ..density..,
                          scale = 3)) +
  
  geom_point(data = unique(dat[xp_level == "novice" & difference == 0, c(1, 4)]),
             aes(x = average_speed_sigma, 
                 y = predator_id),
             size = 1,
             color = "black") +
  scale_x_continuous(breaks = seq(0, 2, 0.5),
                     limits = c(0, 2.05)) +
  #scale_x_continuous(breaks = scaled_breaks1,
  #                   labels = scaleFUN,
  #                   limits = c(0, 4)) +
  
  ylab("Predator ID\n") +
  xlab("\nIntra individual SD (m/s)") +
  
  custom_theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15,
                                  face = "bold"))
  

plot3 <- ggplot() +
  
  geom_density_ridges(data = dat[xp_level == "advanced" & difference == 1],
                      rel_min_height = 0.005,
                      fill = "#00AFBB",
                      aes(x = exp_speed_sigma,
                          y = predator_id,
                          height = ..density..,
                          scale = 3)) +
  
  geom_point(data = unique(dat[xp_level == "advanced" & difference == 1, c(1, 4)]),
             aes(x = average_speed_sigma, 
                 y = predator_id),
             size = 1,
             color = "black") +
  scale_x_continuous(breaks = seq(0, 2, 0.5),
                     limits = c(0, 2.05)) +
  #scale_x_continuous(breaks = scaled_breaks1,
  #                   labels = scaleFUN,
  #                   limits = c(0, 4)) +
  
  ylab("Predator ID\n") +
  xlab("\nIntra individual SD (m/s)") +
  
  custom_theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15,
                                  face = "bold"))

plot4 <- ggplot() +
  
  geom_density_ridges(data = dat[xp_level == "advanced" & difference == 0],
                      rel_min_height = 0.005,
                      fill = "#00AFBB",
                      aes(x = exp_speed_sigma,
                          y = predator_id,
                          height = ..density..,
                          scale = 3)) +
  
  geom_point(data = unique(dat[xp_level == "advanced" & difference == 0, c(1, 4)]),
             aes(x = average_speed_sigma, 
                 y = predator_id),
             size = 1,
             color = "black") +
  scale_x_continuous(breaks = seq(0, 2, 0.5),
                     limits = c(0, 2.05)) +
  #scale_x_continuous(breaks = scaled_breaks1,
  #                   labels = scaleFUN,
  #                   limits = c(0, 4)) +
  
  ylab("Predator ID\n") +
  xlab("\nIntra individual SD (m/s)") +
  
  custom_theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15,
                                  face = "bold"))

# =======================================================================
# =======================================================================





# =======================================================================
# 5. Combine plots into figures
# =======================================================================


# Prepare figures -------------------------------------------------------

# Figure S1
figureS1 <- ggarrange(
  NULL, gamm_plot1, NULL, gamm_plot2,
  ncol = 4, nrow = 1,
  labels = c("(A)", "", "(B)", ""),
  widths = c(0.15, 1.5, 0.15, 1.5)
)

# Figure S2
figureS2 <- ggarrange(
  plot1, plot3, plot2, plot4,
  labels = c("(A)", "(B)" , "(C)", "(D)"),
  ncol = 2, nrow = 2
)



# Export figures  -------------------------------------------------------

# Folder path
path <- file.path(getwd(), "outputs", "05_outputs_figures")

# Save figures
ggexport(figureS1,
         filename = file.path(path, "05_figureS1.png"),
         width = 3000,
         height = 1200,
         res = 300)

ggexport(figureS2,
         filename = file.path(path, "05_figureS2.png"),
         width = 2500,
         height = 2500,
         res = 300)

# =======================================================================
# =======================================================================
