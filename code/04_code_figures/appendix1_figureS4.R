# =======================================================================

#               Code to produce Appendix 1 : Figure S4                  #

# =======================================================================





# =======================================================================
# 1. Load libraries and import model
# =======================================================================

# Load libraries
library(brms)
library(ggplot2)
library(data.table)

path <- file.path(getwd(), "outputs", "01_outputs_models")
fit <- readRDS(file.path(path, "B1_DHMLM-no-outlier.rds"))

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Estimate differences between novices and advanced levels
# =======================================================================


cors <- as_draws_df(fit, variable = "cor", regex = TRUE)
cors <- data.table(cors)
cors <- cors[, !c(".chain", ".iteration", ".draw")]

cors <- melt(cors)
cors <- cors[!(variable %like% "prior")]

cors1 <- cors[!(variable %like% "interm")]
cors1 <- cors1[!(variable %like% "novice")]
cors2 <- cors[!(variable %like% "interm")]
cors2 <- cors2[!(variable %like% "advanced")]

setnames(cors1, c("variable", "value"), c("variable_adv", "value_adv"))
setnames(cors2, c("variable", "value"), c("variable_nov", "value_nov"))


tab <- cbind(cors1, cors2)
tab[, difference_cor := value_adv - value_nov]


# Intervals
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

tab[
  , ":="(
    median_difference = median(difference_cor),
    lower_ci_95 = lower_95(difference_cor),
    upper_ci_95 = upper_95(difference_cor),
    lower_ci_80 = lower_80(difference_cor),
    upper_ci_80 = upper_80(difference_cor),
    lower_ci_50 = lower_50(difference_cor),
    upper_ci_50 = upper_50(difference_cor)),
  by = variable_adv
]

tab <- unique(
  tab[
    , .(
      variable_adv, median_difference,
      lower_ci_95, upper_ci_95,
      lower_ci_80, upper_ci_80,
      lower_ci_50, upper_ci_50
    )
  ]
)

tab[, c(2:8) := round(tab[, c(2:8)], digits = 2)]


names <- c(
  "(mean speed, IIV speed)", "(mean speed, mean prey speed)",
  "(IIV speed, mean prey speed)", "(mean speed, IIV prey speed)",
  "(IIV speed, IIV prey speed)", "(mean prey speed, IIV prey speed)",
  "(mean speed, mean success)", "(IIV speed, mean success)",
  "(mean prey speed, mean success)", "(IIV prey speed, mean success)"
)
tab[, variable_adv := names]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Plot the differences
# =======================================================================


# Compute plots ---------------------------------------------------------

colors <- c(
  "0.95" = "gray91",
  "0.80" = "gray71",
  "0.50" = "gray21"
)

fig <- ggplot(
  tab,
  aes(x = variable_adv, y = median_difference)
) +
  geom_hline(
    yintercept = 0, linewidth = 1, colour = "red",
    linetype = "dashed", alpha = 0.5
  ) +
  #geom_pointrange(
  #  aes(ymin = lower_ci_95, ymax = upper_ci_95),
  #  size = 0.5, position = position_dodge(width = 0.8)
  #) +
  geom_linerange(
    aes(ymin = lower_ci_95,
        ymax = upper_ci_95,
        color = "0.95"),
    size = 1,
    key_glyph = "path"
    ) +
  geom_linerange(
    aes(ymin = lower_ci_80,
        ymax = upper_ci_80,
        color = "0.80"),
    size = 1,
    key_glyph = "path"
    ) +
  geom_linerange(
    aes(ymin = lower_ci_50,
        ymax = upper_ci_50,
        color = "0.50"),
    size = 1,
    key_glyph = "path"
    ) +
  geom_point(size = 3, color = "black") +
  ylab("\nPosterior median difference") +
  scale_y_continuous(breaks = seq(-0.2, 0.2, 0.1)) +
  scale_color_manual(
    name = "Density:",
    values = colors) +
  coord_flip() +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text = element_text(face = "plain", size = 11, color = "black"),
    axis.title = element_text(size = 13, face = "plain", color = "black"),
    strip.text = element_text(size = 11),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.key = element_rect(fill = "transparent"),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  )

fig

# Export figure ---------------------------------------------------------

# Folder
path <- file.path(getwd(), "outputs", "04_outputs_figures")

# Export
ggsave(
  filename = file.path(path, "appendix1_figureS4.png"),
  plot = fig,
  width = 15, height = 12, # 32 14
  #scale = 1.2,
  units = "cm",
  dpi = 300,
)

# =======================================================================
# =======================================================================