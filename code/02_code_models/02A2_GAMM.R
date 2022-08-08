# ==========================================================================

#                   GAMM model GS - group-level smoother

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

# Load data
data <- fread(file.path(folder, "FraserFrancoetalXXXX-data.csv"),
              select = c("predator_id",
                         "hunting_success",
                         "pred_game_duration",
                         "pred_speed",
                         "cumul_xp_killer"))

# Project path for testing
#data <- fread("./data/FraserFrancoetalXXXX-data.csv",
#              select = c("predator_id",
#                         "pred_game_duration",
#                         "pred_speed",
#                         "prey_avg_speed",
#                         "cumul_xp_killer",
#                         "hunting_success"))

data <- unique(data)

# Predator id as factor
data[, predator_id := as.factor(predator_id)]



# Standardise the variables (Z-scores) -------------------------------------

standardize <- function (x) {(x - mean(x, na.rm = TRUE)) / 
                              sd(x, na.rm = TRUE)}

data[, c("Zgame_duration",
         "Zspeed",
         "Zcumul_xp") := lapply(.SD, standardize), 
       .SDcols = c(3:5)]

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Parametrize the model
# ==========================================================================



# Compute the custom family ------------------------------------------------

beta_binomial2 <- custom_family(
  "beta_binomial2", dpars = c("mu", "phi"),
  links = c("logit", "log"), lb = c(NA, 0),
  type = "int", vars = "vint1[n]"
)



# Function -----------------------------------------------------------------

stan_funs <- "
  real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
    return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
  }
  int beta_binomial2_rng(real mu, real phi, int T) {
    return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
  }
"



# Variables ----------------------------------------------------------------

stanvars <- stanvar(scode = stan_funs, block = "functions")



# Model formula ------------------------------------------------------------

model_formula <- brmsformula(
    hunting_success | vint(4) ~
        s(Zcumul_xp) +
        s(Zcumul_xp, predator_id, bs = "fs") + 
        Zgame_duration
)



# Define priors ------------------------------------------------------------

priors <- c(
  # priors on fixed effects
  set_prior("normal(0, 2)",
            class = "b",
            coef = "Zgame_duration"),
  set_prior("normal(0, 2)",
            class = "b",
            coef = "sZcumul_xp_1"),       
  # priors on smooth terms
  set_prior("normal(0, 2)",
            class = "sds",
            coef = "s(Zcumul_xp)"),
  # priors on phi
  set_prior("normal(2, 1)",
            class = "phi")
            )

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Run the model
# ==========================================================================

model_gs <- brm(formula = model_formula,
                family = beta_binomial2,
                warmup = 500, 
                iter = 1500,
                thin = 4,
                chains = 4,
                threads = threading(10),
                backend = "cmdstanr",
                inits = "0", 
                seed = 123,
                prior = priors,
                sample_prior = TRUE,
                control = list(adapt_delta = 0.99),
                data = data,
                stanvars = stanvars)

saveRDS(model_gs, file = "02A2_GAMM.rds")

# ==========================================================================
# ==========================================================================