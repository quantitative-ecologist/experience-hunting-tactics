# =======================================================================

#                       Code to produce Figure 2                        #

# =======================================================================





# =======================================================================
# 1. Load libraries and import model
# =======================================================================

# Load libraries
library(brms)
library(ggplot2)
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
    "predator ID (sigma)",
    "intercept (sigma)"
  )
]

dt1[
  !(variable %like% "sigma") &
    variable %like% "sd",
  parameter := "predator ID (mean)"
]

dt1[
  variable %like% "Intercept" &
    !(variable %like% "sigma") &
    !(variable %like% "sd"),
  parameter := "intercept (mean)"
]

# Add trait
dt1[variable %like% "speed", trait := "predator speed"]
dt1[variable %like% "preyspeed", trait := "prey speed"]
dt1[variable %like% "success", trait := "success"]

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
  , test := "intermediate vs novice"
]
tab2[, diff := value_adv - value_int][
  , test := "advanced vs intermediate"
]
tab3[, diff := value_adv - value_nov][
  , test := "advanced vs novice"
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
  "0.95" = "gray91",
  "0.80" = "gray71",
  "0.50" = "gray21"
)

# To bold the letter but not the text
tab$test <- c(
      rep("bold((A))~intermediate~vs~novice", 10),
      rep("bold((B))~advanced~vs~intermediate", 10),
      rep("bold((C))~advanced~vs~novice", 10)
)

# Plot
fig <- ggplot(
  tab,
  aes(x = parameter, y = median_diff, shape = trait)
) +
  geom_hline(
    yintercept = 0, linewidth = 1, colour = "red",
    linetype = "dashed", alpha = 0.5
  ) +
  geom_linerange(
    aes(ymin = lower_95,
        ymax = upper_95,
        color = "0.95"),
    position = position_dodge(width = 0.8),
    linewidth = 1,
    key_glyph = "path"
    ) +
  geom_linerange(
    aes(ymin = lower_80,
        ymax = upper_80,
        color = "0.80"),
    position = position_dodge(width = 0.8),
    linewidth = 1,
    key_glyph = "path"
    ) +
  geom_linerange(
    aes(ymin = lower_50,
        ymax = upper_50,
        color = "0.50"),
    position = position_dodge(width = 0.8),
    linewidth = 1,
    key_glyph = "path"
    ) +
  geom_point(
    fill = "black",
    size = 2,
    position = position_dodge(width = 0.8)
  ) +
  scale_shape_manual(
    name = "Trait :",
    values = c(22, 25, 21)
  ) +
  scale_color_manual(
    name = "Density:",
    values = colors
  ) +
  ylab("\nPosterior median difference") +
  coord_flip() +
  theme_bw() +
  facet_wrap(~ test, labeller = label_parsed) +
  theme(
    axis.title.y = element_blank(),
    axis.title = element_text(size = 15, face = "plain", color = "black"),
    axis.text = element_text(face = "plain", size = 12, color = "black"),
    strip.text = element_text(size = 13),
    # to left align panel titles
    #strip.text = element_text(size = 13, hjust = 0),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.key = element_rect(fill = "transparent"),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13)
  )



# Export figure ---------------------------------------------------------

# Folder
path <- file.path(getwd(), "outputs", "04_outputs_figures")

# Export
ggsave(
  filename = file.path(path, "figure2.png"),
  plot = fig,
  width = 32, height = 14, # 32 14
  units = "cm",
  dpi = 300, scale = 0.9
)

# =======================================================================
# =======================================================================