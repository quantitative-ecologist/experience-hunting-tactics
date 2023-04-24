# =======================================================================

#                     Run a multivariate model                          #
#               to evaluate how tactics change with XP                  #

# =======================================================================





# =======================================================================
# 1. Load libraries, and import dataset
# =======================================================================


# When working on my local computer
# renv::activate()



# Detect number of cores ------------------------------------------------

options(mc.cores = parallel::detectCores())



# Load libraries --------------------------------------------------------

library(data.table)
library(brms)
library(parallel)



# Import the data -------------------------------------------------------

# Folder path Compute Canada
folder <- file.path("/home", "maxime11", "projects", "def-monti",
                    "maxime11", "phd_project", "data")

# Load data on compute canada
data <- fread(file.path(folder, "FraserFrancoetalXXXX-data.csv"),
              select = c("predator_id",
                         "avatar_id",
                         "environment_id",
                         "hunting_success",
                         "cumul_xp_pred",
                         "game_duration",
                         "pred_speed",
                         "total_chase_duration",
                         "prey_avg_speed",
                         "prey_avg_rank"))

# Predator id as factor
data[, predator_id := as.factor(predator_id)]
data[, predator_avatar_id := as.factor(avatar_id)]
data[, environment_id := as.factor(environment_id)]

# Remove outlier from the dataset
data <- data[predator_id != "pred379433"]

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Prepare the data for the model
# =======================================================================

# Here we prepare the data so the model can estimate trait combinations
# at different levels of experience

# Experience will thus be a random factor with 3 levels.
# The random factor is assigned based on the results of the gamm model.
# See figure X for reference ***

# Here :
  # Novice = 0 to 99
  # Intermediate = >=100 to 299
  # advanced = 300 to 500

# All traits will then need to be computed as variables
# at these levels of experience


# Create dummy variable ------------------------------------------------

# This is done so the model partition the covariances by experience

data[cumul_xp_pred < 100,
     xp_level := "novice"]

data[cumul_xp_pred %between% c(100, 299),
     xp_level := "intermediate"]

data[cumul_xp_pred >= 300,
     xp_level := "advanced"]

# Encode the variable as a factor
data[, xp_level := as.factor(xp_level)]


# This is done so the model can subsample the rows
data[, sub1 := ifelse(xp_level == "novice", 1, 0)]
data[, sub2 := ifelse(xp_level == "intermediate", 1, 0)]
data[, sub3 := ifelse(xp_level == "advanced", 1, 0)]



# Transform the variables (sqrt) ----------------------------------------

# Apply the function and create new columns
# The function standardizes the variables by group :
# in this case, by level of experience

data[
  , c("sqrt_game_duration", "sqrt_prey_avg_rank") := lapply(
    .SD, function(x) {
      sqrt(x)
    }
  ),
  .SDcols = c("game_duration", "prey_avg_rank"),
  by = xp_level
]

# Compute the new columns for each level of experience
data[, ":=" (
  speed_novice        = ifelse(xp_level == "novice", pred_speed, NA),
  prey_speed_novice   = ifelse(xp_level == "novice", prey_avg_speed, NA),
  success_novice      = ifelse(xp_level == "novice", hunting_success, NA),
  speed_interm        = ifelse(xp_level == "intermediate", pred_speed, NA),
  prey_speed_interm   = ifelse(xp_level == "intermediate", prey_avg_speed, NA),
  success_interm      = ifelse(xp_level == "intermediate", hunting_success, NA),
  speed_advanced      = ifelse(xp_level == "advanced", pred_speed, NA),
  prey_speed_advanced = ifelse(xp_level == "advanced", prey_avg_speed, NA),
  success_advanced    = ifelse(xp_level == "advanced", hunting_success, NA)
  )
]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Build the multivariate model
# =======================================================================

# We first compute submodels for each level of experience
# These submodels will be added in a joint model that
# will estimate all the covariances



# Speed at three levels of experience -----------------------------------

speed_novice <- bf(
  speed_novice | subset(sub1) + trunc(lb = 0) ~
      1 + sqrt_prey_avg_rank +
      (1 | a | predator_id) +
      (1 | environment_id) +
      (1 | avatar_id),
  sigma ~
      1 + sqrt_prey_avg_rank +
      (1 | a | predator_id)
) + gaussian()

speed_intermediate <- bf(
  speed_interm | subset(sub2) + trunc(lb = 0) ~
      1 + sqrt_prey_avg_rank +
      (1 | a | predator_id) +
      (1 | environment_id) +
      (1 | avatar_id),
  sigma ~
      1 + sqrt_prey_avg_rank +
      (1 | a | predator_id)
) + gaussian()

speed_advanced <- bf(
  speed_advanced | subset(sub3) + trunc(lb = 0) ~
      1 + sqrt_prey_avg_rank +
      (1 | a | predator_id) +
      (1 | environment_id) +
      (1 | avatar_id),
  sigma ~
      1 + sqrt_prey_avg_rank +
      (1 | a | predator_id)
) + gaussian()



# Prey speed at three levels of experience ------------------------------

prey_speed_novice <- bf(
  prey_speed_novice | subset(sub1) + trunc(lb = 0) ~
      1 + sqrt_prey_avg_rank +
      (1 | a | predator_id) +
      (1 | environment_id) +
      (1 | avatar_id),
  sigma ~
      1 + sqrt_prey_avg_rank +
      (1 | a | predator_id)
) + gaussian()

prey_speed_intermediate <- bf(
  prey_speed_interm | subset(sub2) + trunc(lb = 0) ~
      1 + sqrt_prey_avg_rank +
      (1 | a | predator_id) +
      (1 | environment_id) +
      (1 | avatar_id),
  sigma ~
      1 + sqrt_prey_avg_rank +
      (1 | a | predator_id)
) + gaussian()

prey_speed_advanced <- bf(
  prey_speed_advanced | subset(sub3) + trunc(lb = 0) ~
      1 + sqrt_prey_avg_rank +
      (1 | a | predator_id) +
      (1 | environment_id) +
      (1 | avatar_id),
  sigma ~
      1 + sqrt_prey_avg_rank +
      (1 | a | predator_id)
) + gaussian()



# Hunting success at three levels of experience -------------------------

# Compute the custom family

beta_binomial2 <- custom_family(
  "beta_binomial2", dpars = c("mu", "phi"),
  links = c("logit", "log"), lb = c(NA, 0),
  type = "int", vars = "vint1[n]"
)

# Function
stan_funs <- "
  real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
    return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
  }
  int beta_binomial2_rng(real mu, real phi, int T) {
    return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
  }
"

# Variables
stanvars <- stanvar(scode = stan_funs, block = "functions")

# Sub models
success_novice <- bf(
  success_novice | vint(4) + subset(sub1) ~
      1 + sqrt_game_duration +
      (1 | a | predator_id)
) + beta_binomial2

success_interm <- bf(
  success_interm | vint(4) + subset(sub2) ~
      1 + sqrt_game_duration +
      (1 | a | predator_id)
) + beta_binomial2

success_advanced <- bf(
  success_advanced | vint(4) + subset(sub3) ~
      1 + sqrt_game_duration +
      (1 | a | predator_id)
) + beta_binomial2



# priors ----------------------------------------------------------------

priors <- c(
  # Prior on game duration
  set_prior("normal(0.5, 0.5)",
            class = "b",
            coef = "sqrt_game_duration",
            resp = c("successnovice",
                     "successinterm",
                     "successadvanced")),
  # Prior on prey rank
  set_prior("normal(0, 1)",
            class = "b",
            coef = "sqrt_prey_avg_rank",
            resp = c("speednovice",
                     "speedinterm",
                     "speedadvanced",
                     "preyspeednovice",
                     "preyspeedinterm",
                     "preyspeedadvanced")),
  # priors on var. parameters (brms automatically detects half-normal)
  set_prior("normal(0, 1)",
            class = "sd", # applies to all variance parameters
            resp = c("speednovice",
                     "speedinterm",
                     "speedadvanced",
                     "preyspeednovice",
                     "preyspeedinterm",
                     "preyspeedadvanced",
                     "successnovice",
                     "successinterm",
                     "successadvanced")),
  # priors on phi
  set_prior("normal(2, 1)",
            class = "phi",
            resp = c("successnovice",
                     "successinterm",
                     "successadvanced")),
  set_prior("lkj(2)",
            class = "cor",
            group = "predator_id")
)

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Run the multivariate model
# =======================================================================

# ( nitt - burnin ) / thin = 1000
mv_model <- brm(speed_novice +
                speed_intermediate +
                speed_advanced +
                prey_speed_novice +
                prey_speed_intermediate +
                prey_speed_advanced +
                success_novice +
                success_interm +
                success_advanced +
                set_rescor(FALSE),
                warmup = 500,
                iter = 2500,
                thin = 8,
                chains = 4,
                inits = "0",
                threads = threading(12),
                backend = "cmdstanr",
                seed = 123,
                prior = priors,
                sample_prior = TRUE,
                control = list(adapt_delta = 0.99),
                stanvars = stanvars,
                #save_pars = save_pars(all = TRUE),
                data = data)

saveRDS(mv_model, file = "B1_DHMLM-no-outlier.rds")

# =======================================================================
# =======================================================================