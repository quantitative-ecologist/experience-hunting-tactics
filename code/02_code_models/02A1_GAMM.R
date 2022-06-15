# ==========================================================================

#                 Test GAMM model with a sub sample

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
                         "pred_game_duration",
                         "pred_speed",
                         "cumul_xp_killer",
                         "hunting_success"))

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

model_formula <- brmsformula(hunting_success | vint(4) ~
                                        s(Zcumul_xp) +
                                        Zgame_duration +
                                        (1 | predator_id))


# Run model ----------------------------------------------------------------

base_model <- brm(formula = model_formula,
                  family = beta_binomial2,
                  warmup = 500, 
                  iter = 2000,
                  thin = 8,
                  chains = 4,
                  cores = 4,
                  inits = "0", 
                  seed = 123,
                  #prior = priors,
                  control = list(adapt_delta = 0.95),
                  data = data,
                  stanvars = stanvars)

saveRDS(base_model, file = "01a_GAMM.rds")

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Perform PSIS-LOO
# ==========================================================================



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



# Perform PSIS-LOO ---------------------------------------------------------


loo_fit <- loo(fit)
saveRDS(loo_fit, file = "01a_loo")

# ==========================================================================
# ==========================================================================