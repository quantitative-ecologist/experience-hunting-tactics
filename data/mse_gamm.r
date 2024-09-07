# ==========================================================================

#            Calculate Mean-Squared Error for each predator ID
#              Based on model in Fraser Franco et al. (2025)
#                Add link to paper [here] at some point...

#                               Idea :
#               Test if deviation from global curve
#           can explain differences specialisation patterns

# ==========================================================================



# ==========================================================================
# 1. Prepare script
# ==========================================================================

# PROBLÈME PRÉDICTIONS PAS PAREILLES OU SINON PAS LE BON MODÈLE


# Load libraries and model -------------------------------------------------

options(mc.cores = parallel::detectCores())

library(parallel)
library(brms)
library(data.table)

path <- file.path(getwd(), "data", "osf_folder")
fit <- readRDS(file.path(path, "GAMM-V.rds"))



# Load the data ------------------------------------------------------------

data <- fread(
  file.path(
    getwd(),
    "data",
    "FraserFrancoetal2023-data.csv"
  )
)

data[, predator_id := as.factor(predator_id)]



# Post processing preparations for custom family ---------------------------

expose_functions(fit, vectorize = TRUE)

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

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Prepare the predictions
# ==========================================================================



# Prepare the plots --------------------------------------------------------

# Group-level smooths
tab_a <- conditional_effects(
  fit, method = "fitted",
  effects = "Zcumul_xp:predator_id",
  robust = TRUE, re_formula = NULL
)
tab_a <- data.table(tab_a[[1]])

# Cumulative XP global trend
tab_b <- conditional_effects(
  fit, method = "fitted",
  robust = TRUE, re_formula = NULL,
  effects = "Zcumul_xp",
  conditions = data.frame(predator_id = NA)
)
tab_b <- data.table(tab_b[[1]])



# Transform values ---------------------------------------------------------

# Back transform x-axis values
sequence <- (seq(0, 500, 100) - mean(data$cumul_xp_pred))
standev <- sd(data$cumul_xp_pred)
scaled_breaks <- sequence / standev

# List the tables
tables <- list(
  tab_a, tab_b
)
names(tables) <- c(
  "tab_a", "tab_b"
)

# Function to apply transformation
# Computes non standardized cumulative XP
func <- function(x) {
  x[, cumul_xp := (Zcumul_xp * standev) + mean(data$cumul_xp_pred)]
}

# Apply function
lapply(tables, func)

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Calculate MSE
# ==========================================================================


# Merge both tables --------------------------------------------------------

merged_data <- merge(
  tab_a, tab_b,
  by = "cumul_xp",
  suffixes = c("_ind", "_global")
)


# SE for each row ----------------------------------------------------------

merged_data[, squared_error := (estimate___ind - estimate___global)^2]



# Compute MSE for each predator --------------------------------------------

mse_dat <- merged_data[
  , .(MSE = mean(squared_error)),
  by = predator_id_ind
]

# Assign a quantile-based bin to MSE values
mse_dat[
  , MSE_bins := cut(
    mse_dat$MSE,
    breaks = quantile(mse_dat$MSE, probs = seq(0, 1, by = 0.25)),
    include.lowest = TRUE
  )
]

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 4. Plot the data
# ==========================================================================

# Combine values to original table -----------------------------------------

setnames(mse_dat, old = "predator_id_ind", new = "predator_id")
tab_a <- merge(x = tab_a, y = mse_dat, by = "predator_id")



# Custom theme
custom_theme <- theme(
  # axis values size
  axis.text = element_text(face = "plain",
                           size = 14,
                           color = "black"),
  # axis ticks lenght
  axis.ticks.length = unit(.15, "cm"),
  # axis ticks width
  axis.ticks = element_line(linewidth = 0.90,
                            color = "black"),
  # axis titles size
  axis.title = element_text(size = 16,
                            face = "plain",
                            color = "black"),
  axis.line = element_line(linewidth = 0.95,
                           color = "black"),
  legend.position = "none",
  panel.grid = element_blank(),
  panel.background = element_blank()
)





p <- ggplot(
  tab_a,
  aes(x = Zcumul_xp,
      y = estimate__ / 4,
      color = predator_id)
) +
  geom_line(linewidth = 1, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE, option = "D") + #B
  ylab("Hunting success\n") +
  ggtitle("Prey rank") +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = scaled_breaks,
                     labels = seq(0, 500, 100)) +
  xlab("\nCumulative experience") +
  facet_wrap(~ MSE_bins) +
  custom_theme

p1 <- ggplot(
  tab_b,
  aes(x = Zcumul_xp,
      y = estimate__ / 4)
) +
  geom_ribbon(
    aes(
      ymin = lower__ / 4,
      ymax = upper__ / 4
    ),
    fill = "gray"
  ) +
  geom_line(linewidth = 1) +
  ylab("Hunting success\n") +
  ggtitle("Prey rank") +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  ) +
  scale_x_continuous(
    breaks = scaled_breaks,
    labels = seq(0, 500, 100)
  ) +
  xlab("\nCumulative experience") +
  custom_theme
