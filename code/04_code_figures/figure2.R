# =======================================================================

#                       Code to produce Figure 2                        #

# =======================================================================





# =======================================================================
# 1. Load libraries and import model
# =======================================================================

# Load libraries
library(brms)
library(ggplot2)
library(ggpubr)
library(viridis)
library(data.table)

path <- file.path(getwd(), "outputs", "01_outputs_models")
fit <- readRDS(file.path(path, "B1_DHMLM-no-outlier.rds"))

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Estimate differences between experience levels
# =======================================================================

# a -> Compare novices vs intermediate
# b -> Compare intermediate with advanced
# c-> Compare novices with advanced


# Extract the posterior samples -----------------------------------------

dt <- data.table(
  as_draws_df(
    fit,
    variable = c("b_", "sd"),
    regex = TRUE
  )
)
dt <- dt[, c(1:15, 54:68)]



# Back-transform values -------------------------------------------------

# Take the exponent of sigma parameters
dt[
  , c("b_sigma_speednovice_Intercept",
      "b_sigma_speedinterm_Intercept",
      "b_sigma_speedadvanced_Intercept",
      "b_sigma_preyspeednovice_Intercept",
      "b_sigma_preyspeedinterm_Intercept",
      "b_sigma_preyspeedadvanced_Intercept",
      "sd_predator_id__sigma_speednovice_Intercept",
      "sd_predator_id__sigma_speedinterm_Intercept",
      "sd_predator_id__sigma_speedadvanced_Intercept",
      "sd_predator_id__sigma_preyspeednovice_Intercept",
      "sd_predator_id__sigma_preyspeedinterm_Intercept",
      "sd_predator_id__sigma_preyspeedadvanced_Intercept"
  )
  := lapply(.SD, function(x) {exp(x)}),
  .SDcols = c(
    "b_sigma_speednovice_Intercept",
    "b_sigma_speedinterm_Intercept",
    "b_sigma_speedadvanced_Intercept",
    "b_sigma_preyspeednovice_Intercept",
    "b_sigma_preyspeedinterm_Intercept",
    "b_sigma_preyspeedadvanced_Intercept",
    "sd_predator_id__sigma_speednovice_Intercept",
    "sd_predator_id__sigma_speedinterm_Intercept",
    "sd_predator_id__sigma_speedadvanced_Intercept",
    "sd_predator_id__sigma_preyspeednovice_Intercept",
    "sd_predator_id__sigma_preyspeedinterm_Intercept",
    "sd_predator_id__sigma_preyspeedadvanced_Intercept"
  )
]

# Back-transform hunting success
dt[
  , c("b_successnovice_Intercept",
      "b_successinterm_Intercept",
      "b_successadvanced_Intercept"
  )
  := lapply(.SD, function(x) {plogis(x)}),
  .SDcols = c(
    "b_successnovice_Intercept",
    "b_successinterm_Intercept",
    "b_successadvanced_Intercept"
  )
]


# Prepare the data ------------------------------------------------------

# Reshape the table
dt1 <- melt(dt, measure.vars = names(dt))

# Add xp level
dt1[variable %like% "novice", xp_level := "novice"]
dt1[variable %like% "interm", xp_level := "intermediate"]
dt1[variable %like% "advanced", xp_level := "advanced"]

# Add variable
dt1[
  , parameter := ifelse(
    variable %like% "sd" &
      variable %like% "sigma",
    "id_var",
    "pop_var"
  )
]

dt1[
  !(variable %like% "sigma") &
    variable %like% "sd",
  parameter := "id_mean"
]

dt1[
  variable %like% "Intercept" &
    !(variable %like% "sigma") &
    !(variable %like% "sd"),
  parameter := "pop_mean"
]

# Add trait
dt1[variable %like% "speed", trait := "Predator speed"]
dt1[variable %like% "preyspeed", trait := "Prey speed"]
dt1[variable %like% "success", trait := "Hunting success"]

dt1[, variable := NULL]



# Individual tables by xp -----------------------------------------------

nov <- dt1[xp_level == "novice", ]
int <- dt1[xp_level == "intermediate", ]
adv <- dt1[xp_level == "advanced", ]

nov[, value_nov :=  value][, c("value", "xp_level") := NULL]
int[, value_int :=  value][, c("value", "xp_level") := NULL]
adv[, value_adv :=  value][, c("value", "xp_level") := NULL]

tab1 <- cbind(nov, int[, 3])
tab2 <- cbind(int, adv[, 3])
tab3 <- cbind(nov, adv[, 3])

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Plot the differences
# =======================================================================


# Combine estimated differences as 1 table ------------------------------

# Calculate difference
tab1[, diff := value_int - value_nov][
  , test := "intermediate - novice"
]
tab2[, diff := value_adv - value_int][
  , test := "advanced - intermediate"
]
tab3[, diff := value_adv - value_nov][
  , test := "advanced - novice"
]

# Combine
vars <- c("parameter", "trait", "diff", "test")
tab <- rbind(tab1[, ..vars], tab2[, ..vars], tab3[, ..vars])



# Calculate posterior medians and intervals -----------------------------

# HPD intervals
lower_95 <- function(x) {
  coda::HPDinterval(as.mcmc(x), 0.95)[1]
}
upper_95 <- function(x) {
  coda::HPDinterval(as.mcmc(x), 0.95)[2]
}

lower_80 <- function(x) {
  coda::HPDinterval(as.mcmc(x), 0.80)[1]
}
upper_80 <- function(x) {
  coda::HPDinterval(as.mcmc(x), 0.80)[2]
}

lower_50 <- function(x) {
  coda::HPDinterval(as.mcmc(x), 0.50)[1]
}
upper_50 <- function(x) {
  coda::HPDinterval(as.mcmc(x), 0.50)[2]
}

# Median difference + CIs
tab[
  , ":=" (
    median_diff = median(diff),
    lower_95 = lower_95(diff),
    lower_80 = lower_80(diff),
    lower_50 = lower_50(diff),
    upper_95 = upper_95(diff),
    upper_80 = upper_80(diff),
    upper_50 = upper_50(diff)
    ),
  by = .(trait, parameter, test)
]

# Now round the values to 3 digits
tab[
  , c(5:11) := lapply(.SD, function(x) round(x, digits = 3)),
  .SDcols = c(5:11)
]

tab <- unique(tab[, c(1, 2, 4:11)])



# Compute plots ---------------------------------------------------------

# Color scale for CIs
colors <- c(
  "0.95" = "gray85",
  "0.80" = "gray61",
  "0.50" = "gray21"
)


# Setup my theme
custom_theme <- theme(
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 11, color = "black"),
  strip.text = element_text(size = 13, face = "bold"),
  panel.grid = element_blank(),
  panel.background = element_rect(fill = "white"),
  panel.border = element_rect(color = "black", fill = NA),
  legend.position = "top",
  #legend.title = element_text(size = 13),
  legend.title = element_blank(),
  legend.text = element_text(size = 11)
)

# y-axis labels
labels_speed <- c(
  "id_var" = "Individual foraging specialisation",
  "id_mean" = "Individual foraging tactic",
  "pop_var" = "Population foraging specialisation",
  "pop_mean" = "Population foraging tactic"
)

labels_preyspeed <- c(
  "id_var" = "Variation in prey encountered (id.)",
  "id_mean" = "Mean prey encountered (id.)",
  "pop_var" = "Variation in prey encountered (pop.)",
  "pop_mean" = "Mean prey encountered (pop.)"
)

labels_success <- c(
  "id_mean" = "Mean individual success",
  "pop_mean" = "Mean population success"
)

# Set dodge
dodge <- position_dodge(width = 0.7)

# Plot
tab[, test := factor(test, levels = c(
  "intermediate - novice",
  "advanced - intermediate",
  "advanced - novice"
))]

make_plot <- function(trait_name, label_map) {
  dat <- tab[trait == trait_name]
  dat[, parameter := factor(
    parameter,
    levels = names(label_map),
    labels = label_map)
  ]

  ggplot(dat, aes(x = median_diff, y = parameter, shape = test)) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "purple",
      linewidth = 0.8
    ) +
    geom_errorbarh(
      aes(xmin = lower_95, xmax = upper_95, color = "0.95"),
      height = 0.25, position = dodge,
      linewidth = 1, show.legend = FALSE
    ) +
    geom_errorbarh(
      aes(xmin = lower_80, xmax = upper_80, color = "0.80"),
      height = 0.25, position = dodge,
      linewidth = 1, show.legend = FALSE
    ) +
    geom_errorbarh(
      aes(xmin = lower_50, xmax = upper_50, color = "0.50"),
      height = 0.25, position = dodge,
      linewidth = 1, show.legend = FALSE
    ) +
    geom_point(size = 2.5, fill = "black", position = dodge) +
    scale_color_manual(name = "Interval", values = colors) +
    scale_shape_manual(name = "Comparison", values = c(21, 22, 24)) +
    scale_x_continuous(
      breaks = seq(-0.2, 0.2, 0.1),
      limits = c(-0.245, 0.245)
    ) +
    labs(
      title = trait_name,
      x = "\nPosterior median difference",
      y = NULL
    ) +
    theme_bw() +
    custom_theme
}

p1 <- make_plot("Predator speed", labels_speed)
p2 <- make_plot("Prey speed", labels_preyspeed)
p3 <- make_plot("Hunting success", labels_success)

fig <- ggarrange(
  p1, p2, p3,
  ncol = 3,
  labels = c("A", "B", "C"),
  common.legend = TRUE,
  legend = "top",
  widths = c(1, 1, 1)
)
fig



# Export figure ---------------------------------------------------------

# Folder
path <- file.path(getwd(), "outputs", "04_outputs_figures")

# Export
ggsave(
  filename = file.path(path, "figure2.png"),
  plot = fig,
  width = 52, height = 14, # 32 14
  units = "cm",
  dpi = 300, scale = 0.9
)

# =======================================================================
# =======================================================================