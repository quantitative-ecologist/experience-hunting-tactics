# =======================================================================

#     Calculate percentages of players that changed specialization      #

# =======================================================================





# =======================================================================
# 1. Prepare the script
# =======================================================================


# Load libraries and model ----------------------------------------------

# Libraries
library(brms)
library(data.table)

# import model
path <- file.path(getwd(), "outputs", "01_outputs_models")
fit <- readRDS(file.path(path, "B1_DHMLM-no-outlier.rds"))



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
setcolorder(table, c(1, 2, 4, 3))

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
  table[, c(3:5, 8)],
  1,
  function(x) truncnorm::rtruncnorm(x[1], mean = x[2], sd = x[3], a = x[4])
)



# Extract the data for every xp level -----------------------------------

# As data table
normalize <- data.table(normalize)

# Novice players
novice <- normalize[, c(1:252)]
novice <- melt(novice)
novice[, xp_level := "novice"]
colnames(novice) <- paste(colnames(novice), "nov", sep = "_")

# Advanced players
adv <- normalize[, c(253:504)]
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
  table[, .(predator_id, difference_sigma)],
  allow.cartesian = TRUE
)

# The merging produces duplicates so I remove them
dat <- unique(dat)

# =======================================================================
# =======================================================================





# =======================================================================
# 5. Calculate population metrics
# =======================================================================


# Compute range of population change in specialization ------------------

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



# Inspect range ---------------------------------------------------------

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



# Compute player range --------------------------------------------------

dat[difference_sigma <= -0.2, change := "large"]
dat[difference_sigma >= 0.2, change := "large"]
dat[difference_sigma %between% c(-0.05, 0.05), change := "stable"]
dat[difference_sigma %between% c(-0.1999, -0.0503), change := "moderate"]
dat[difference_sigma %between% c(0.051, 0.1999), change := "moderate"]

# =======================================================================
# =======================================================================





# =======================================================================
# 6. Create the table and save
# =======================================================================

# Calculate percentages -------------------------------------------------

n_pred <- length(unique(dat$predator_id))

n_player_large <- length(unique(dat[change == "large", predator_id]))
n_player_stable <- length(unique(dat[change == "stable", predator_id]))
n_player_moderate <- length(unique(dat[change == "moderate", predator_id]))

percent_tab <- data.frame(
    percentage = c(
        round(n_player_large / n_pred, digits = 2),
        round(n_player_stable / n_pred, digits = 2),
        round(n_player_moderate / n_pred, digits = 2)
    ),
    change = c("large", "moderate", "stable")
)



# Save the data.frame ------------------------------------------------------

path <- file.path(getwd(), "outputs", "03_outputs_model-processing")
saveRDS(percent_tab, file = file.path(path, "DHMLM_percentages.rds"))

# =======================================================================
# =======================================================================