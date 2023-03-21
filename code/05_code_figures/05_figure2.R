# =======================================================================

#          Plot distribution of players from novice to advanced         #

# =======================================================================





# =======================================================================
# 1. Prepare the script
# =======================================================================


# Load libraries and model ----------------------------------------------

# Libraries
library(brms)
library(data.table)
library(ggplot2)
library(ggpubr)
library(ggridges)

# import model
path <- file.path(getwd(), "outputs", "02_outputs_models")
fit <- readRDS(file.path(path, "02B_DHMLM.rds"))



# Load data ------------------------------------------------------------

# Data
data <- fread("./data/FraserFrancoetal2023-data.csv",
              select = c("predator_id",
                         "cumul_xp_pred",
                         "pred_speed"))

# Predator id as factor
data[, predator_id := as.factor(predator_id)]

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Extract posterior draws for predator speed
# =======================================================================

# Extract values --------------------------------------------------------

# individual means
draws <- data.table(
  as_draws_df(
    fit,
    variable = c(
      "r_predator_id__sigma_speed",
      "r_predator_id__speed"
    ),
    regex = TRUE)
)

# Remove columns that are not needed
draws <- draws[, .SD, .SDcols = ! names(draws) %like% "cor"]
draws <- draws[, .SD, .SDcols = ! names(draws) %like% "interm"]
draws <- draws[, .SD, .SDcols = ! names(draws) %like% "prey"]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Prepare the draws table
# =======================================================================


# Reshape ---------------------------------------------------------------

# Long format
draws <- melt(draws,
              measure = patterns("^r_predator_id"),
              variable.name = "predator_id")



# Add columns -----------------------------------------------------------

# Add experience level
draws[, xp_level := ifelse(predator_id %like% "novice",
                           "novice",
                           "advanced")]

# Add parameter
draws[, parameter := ifelse(predator_id %like% "sigma", "sigma", "mean")]

# Add predator ID
draws[, predator_id := as.character(predator_id)]
draws[, predator_id := gsub("[]_,a-zA-Z,[]", "", predator_id)]
draws[, predator_id := gsub(" ", "", paste("pred", predator_id))]
draws[, predator_id := as.factor(predator_id)]

# Delete the mcmc columns
draws[, c(1:3) := NULL]



# Add intercept values to predator means --------------------------------

# Extract intercepts
int1 <- fixef(fit, pars = "speednovice_Intercept")[1]
int2 <- fixef(fit, pars = "speedadvanced_Intercept")[1]
int3 <- fixef(fit, pars = "sigma_speednovice_Intercept")[1]
int4 <- fixef(fit, pars = "sigma_speedadvanced_Intercept")[1]

# Add the intercept values
draws[
  xp_level == "novice" & parameter == "mean",
  value_and_intercept := value + int1
]

draws[
  xp_level == "novice" & parameter == "sigma",
  value_and_intercept := value + int3
]

draws[
  xp_level == "advanced" & parameter == "mean",
  value_and_intercept := value + int2
]

draws[
  xp_level == "advanced" & parameter == "sigma",
  value_and_intercept := value + int4
]



# Calculate the averages ------------------------------------------------

# Back transform sigma
draws[parameter == "sigma", value_and_intercept := exp(value_and_intercept)]

# Compute the mean
table <- draws[
  , .(mean_value = mean(value_and_intercept)),
  by = .(predator_id, xp_level, parameter)
]



# Calculate the difference between novice and advanced ------------------

# Wide format to calculate the difference
table <- dcast(
  table,
  predator_id + parameter ~ xp_level,
  value.var = "mean_value"
)

# Reorder
setcolorder(table, c(1,2,4,3))

# Calculate difference in sigma values between novice and advanced
table[, difference := novice - advanced, by = .(predator_id, parameter)]

# Reorder the table to long format for further changes
table <- melt(
  table,
  measure = c("novice", "advanced"),
  variable.name = "xp_level"
  )

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Generate a distribution for every player based on model estimates
# =======================================================================


# Prepare the data for sampling -----------------------------------------

# Add sample size to sample normal distribution
table[, sample_size := 500]

# Reshape the table
table <- dcast(
  table,
   xp_level + predator_id + sample_size ~ parameter,
  value.var = c("value", "difference")
  )

# Compute normal distributions for each predator  
#normalize <- apply(
#  table[,c(3:5)],
#  1, 
#  function(x) rnorm(x[1], mean = x[2], sd = x[3])
#)

# Compute truncated normal distributions for each predator  
table[, trunc := 0]

normalize <- apply(
  table[,c(3:5,8)],
  1, 
  function(x) truncnorm::rtruncnorm(x[1], mean = x[2], sd = x[3], a = x[4])
)



# Extract the data for every xp level -----------------------------------

# As data table
normalize <- data.table(normalize)

# Novice players
novice <- normalize[,c(1:253)]
novice <- melt(novice)
novice[, xp_level := "novice"]
colnames(novice) <- paste(colnames(novice), "nov", sep = "_")

# Advanced players
adv <- normalize[,c(254:506)]
adv <- melt(adv)
adv[, xp_level := "advanced"]
colnames(adv) <- paste(colnames(adv), "adv", sep = "_")

# Combine as one table
dat <- cbind(novice, adv)

# Add predator ID
dat[, predator_id := rep(levels(table$predator_id), each = 500)]
dat[, predator_id := as.factor(predator_id)]

# Merge the tables
dat <- merge(
  dat,
  table[,.(predator_id, difference_sigma)],
  allow.cartesian = TRUE
)

# The merging produces duplicates so I remove them
dat <- unique(dat)

# =======================================================================
# =======================================================================





# =======================================================================
# 5. Compute the plots
# =======================================================================


# Calculate percentages of players learning -----------------------------

# Inspect quantiles
quantile(dat[, difference_sigma])


# Subset greatest increases + players that did not change
flex <- unique(
  dat[
    difference_sigma <= -0.2,
    .(predator_id, difference_sigma)
  ]
)

specialists <- unique(
  dat[
    difference_sigma >= 0.2,
    .(predator_id, difference_sigma)
  ]
)

constant <- unique(
  dat[
    difference_sigma %between% c(-0.05, 0.05),
    .(predator_id, difference_sigma)
  ]
)

# Inspect range
range(specialists$difference_sigma)
range(flex$difference_sigma)
range(constant$difference_sigma)

# Check percentages
length(unique(flex$predator_id)) / length(unique(dat$predator_id))
# 8%

length(unique(specialists$predator_id)) / length(unique(dat$predator_id))
# 5%

length(unique(constant$predator_id)) / length(unique(dat$predator_id))
# 43%



# Check quantiles to subsample predators --------------------------------

# Check quantiles
lower_q <- as.vector(quantile(dat$difference_sigma)[2])
upper_q <- as.vector(quantile(dat$difference_sigma)[4])

# Extract players with the greatest differences in sigma
# lower values = increase in flexibility
# higher values = decrease in flexibility (i.e. specialization)
ids <- factor(
  unique(
    dat[difference_sigma <= lower_q | difference_sigma >= upper_q, predator_id]
  )
)
#set.seed(123)
#id_sample <- factor(sample(ids, 50))
dat_sample <- dat[predator_id %in% ids]

# Inspect the distribution of sigma differences
hist(
  unique(dat_sample[, .(predator_id, difference_sigma)])$difference_sigma,
  breaks = 20
)

# Check sample range
unique(dat_sample[,.(predator_id, difference_sigma)][, range(difference_sigma)])
length(unique(dat_sample[, predator_id])) / length(unique(dat[, predator_id]))
# 50% se situent entre -1.62 et 0.66 avec les limites -0.05 et 0.07



# Compute the plots -----------------------------------------------------

# Highest increase in specialization
plot1 <- ggplot() +
  geom_density(data = dat_sample[difference_sigma > 0.2],
               fill = "#999999",
               color = "#999999",
               alpha = 0.5,
               aes(x = value_nov)) +
  geom_density(data = dat_sample[difference_sigma > 0.2],
               fill = "#00AFBB",
               alpha = 0.5,
               aes(x = value_adv)) +
  ylab("Density\n") +
  xlab("\nPredator speed (m/s)") +
  scale_x_continuous(breaks = seq(0, 8, 2), limits = c(0, 8)) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) +
  facet_wrap(~ reorder(as.factor(predator_id), difference_sigma))

# Highest increase in flexibility
plot2 <- ggplot() +
  geom_density(data = dat_sample[difference_sigma < -0.28],
               fill = "#999999",
               color = "#999999",
               alpha = 0.5,
               aes(x = value_nov)) +
  geom_density(data = dat_sample[difference_sigma < -0.28],
               fill = "#00AFBB",
               alpha = 0.5,
               aes(x = value_adv)) +
  ylab("Density\n") +
  xlab("\nPredator speed (m/s)") +
  scale_x_continuous(breaks = seq(0, 8, 2), limits = c(0, 8)) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) +
  facet_wrap(~ reorder(as.factor(predator_id), -difference_sigma))



# Transparent version ---------------------------------------------------

plot1T <- ggplot() +
  geom_density(data = dat_sample[difference_sigma > 0.2],
               fill = "#999999",
               color = "#999999",
               alpha = 0.5,
               aes(x = value_nov)) +
  geom_density(data = dat_sample[difference_sigma > 0.2],
               fill = "#00AFBB",
               alpha = 0.5,
               aes(x = value_adv)) +
  ylab("Density\n") +
  xlab("\nPredator speed (m/s)") +
  scale_x_continuous(breaks = seq(0, 8, 2), limits = c(0, 8)) +
  theme_bw() +
  theme(axis.text = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        axis.title = element_text(color = "white"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  facet_wrap(~ reorder(as.factor(predator_id), difference_sigma))

# Highest increase in flexibility
plot2T <- ggplot() +
  geom_density(data = dat_sample[difference_sigma < -0.28],
               fill = "#999999",
               color = "#999999",
               alpha = 0.5,
               aes(x = value_nov)) +
  geom_density(data = dat_sample[difference_sigma < -0.28],
               fill = "#00AFBB",
               alpha = 0.5,
               aes(x = value_adv)) +
  ylab("Density\n") +
  xlab("\nPredator speed (m/s)") +
  scale_x_continuous(breaks = seq(0, 8, 2), limits = c(0, 8)) +
  theme_bw() +
  theme(axis.text = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        axis.title = element_text(color = "white"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  facet_wrap(~ reorder(as.factor(predator_id), -difference_sigma))

# =======================================================================
# =======================================================================





# =======================================================================
# 6. Combine the plots as one figure
# =======================================================================

# Combine as one figure -------------------------------------------------

# Combine plots
figure <- ggarrange(plot1, plot2,
                    labels = c("(A)", "(B)"),
                    ncol = 2, nrow = 1)

# Folder path
path <- file.path(getwd(), "outputs", "05_outputs_figures")

# Save figure
ggexport(
  figure,
  filename = file.path(path, "05_figure2.png"),
  width = 2500,
  height = 1200,
  res = 300
)



# Transparent version ---------------------------------------------------

# Combine
figureT <- ggarrange(plot1T, plot2T,
                    labels = c("(A)", "(B)"),
                    font.label = list(color = "white"),
                    ncol = 2, nrow = 1)

# Folder path
path <- file.path(getwd(), "plots_presentations")

# Save figure
ggsave(figureT,
        filename = file.path(path, "05_figure2-transparent.png"),
        width = 2500,
        height = 1200,
        dpi = 300,
        units = "px",
        bg = "transparent")

# =======================================================================
# =======================================================================