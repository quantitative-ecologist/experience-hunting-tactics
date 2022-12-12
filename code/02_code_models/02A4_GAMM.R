# ==========================================================================

#         GAMM model G - single common smoother - Latency 1st capture

# ==========================================================================





# ==========================================================================
# 1. Prepare the data
# ==========================================================================



# Load packages ------------------------------------------------------------

# Detect cores
options(mc.cores = parallel::detectCores())

# Load libraries
library(data.table)
library(brms)
library(parallel)



# Load data ----------------------------------------------------------------

# Folder path Compute Canada
folder <- file.path("/home", "maxime11", "projects", "def-monti", 
                    "maxime11", "phd_project", "data")

# Load data on compute canada
data <- fread(file.path(folder, "FraserFrancoetalXXXX-data.csv"),
              select = c("predator_id",
                         "latency_1st_capture",
                         "game_duration",
                         "cumul_xp_pred"))

# Project path for testing
#data <- fread("./data/FraserFrancoetalXXXX-data.csv",
#              select = c("predator_id",
#                         "latency_1st_capture",
#                         "pred_game_duration",
#                         "cumul_xp_pred"))

# Predator id as factor
data[, predator_id := as.factor(predator_id)]



# Standardise the variables (Z-scores) -------------------------------------

standardize <- function (x) {(x - mean(x, na.rm = TRUE)) / 
                              sd(x, na.rm = TRUE)}

data[, c("Zgame_duration",
         "Zcumul_xp") := lapply(.SD, standardize), 
       .SDcols = c(3:4)]

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Parametrize the model
# ==========================================================================

# Model formula ------------------------------------------------------------

model_formula <- brmsformula(
  latency_1st_capture ~
      s(Zcumul_xp) +
      s(predator_id, bs = "re") +
      Zgame_duration
)



# Define priors ------------------------------------------------------------

priors <- c(
  # priors on fixed effects
  set_prior("normal(0, 1)",
            class = "b",
            coef = "Zgame_duration"),
  set_prior("normal(0, 2)",
            class = "b",
            coef = "sZcumul_xp_1"),
  # prior on the intercept
  set_prior("normal(0.05, 0.5)",
            class = "Intercept"),
  # prior on sds predator_id
  set_prior("normal(0.60, 0.5)",
            class = "sds"),
  # priors on smooth terms
  set_prior("cauchy(0, 2)",
            class = "sds",
            coef = "s(Zcumul_xp)"),
  # priors on phi
  set_prior("normal(2, 0.5)",
            class = "phi")
)



# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Run the model
# ==========================================================================

model_g <- brm(formula = model_formula,
               family = gaussian,
               warmup = 500, 
               iter = 2500,
               thin = 8,
               chains = 4,
               threads = threading(12),
               backend = "cmdstanr",
               inits = "0", 
               seed = 123,
               prior = priors,
               sample_prior = TRUE,
               control = list(adapt_delta = 0.99),
               data = data,
               stanvars = stanvars)

saveRDS(model_g, file = "02A4_GAMM.rds")

# ==========================================================================
# ==========================================================================