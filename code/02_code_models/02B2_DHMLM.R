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
data <- fread(file.path(folder, "FraserFrancoetal2023-data-random.csv"),
              select = c("predator_id",
                         "avatar_id",
                         "hunting_success",
                         "cumul_xp_pred",
                         "total_xp_pred",
                         "game_duration",
                         "pred_speed",
                         "prey_avg_speed",
                         "prey_avg_rank"))

# Predator id as factor
data[, predator_id := as.factor(predator_id)]
data[, predator_avatar_id := as.factor(avatar_id)]

# Group players by experience
data[, player_type := "empty"]
data[total_xp_pred < 50, player_type := "<50"]
data[total_xp_pred %between% c(50, 99), player_type := ">=50<100"]
data[total_xp_pred %between% c(100, 299), player_type := ">=100<300"]
data[total_xp_pred >= 300, player_type := ">=300"]

data[, player_type := as.factor(player_type)]
data[, total_xp_pred := player_type]

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Prepare the data for the model
# =======================================================================

# Here we prepare the data so the model can estimate trait correlations



# Transform the variables (sqrt) ----------------------------------------

# Apply the function and create new columns
# The function standardizes the variables :

data[, c("sqrt_game_duration", "sqrt_prey_avg_rank") :=
       lapply(.SD, function (x) {sqrt(x)}), 
       .SDcols = c("game_duration", "prey_avg_rank")]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Build the multivariate model 
# =======================================================================

# We first compute submodels
# These submodels will be added in a joint model that
# will estimate all the covariances



# Speed at three levels of experience -----------------------------------

pred_speed <- bf(
  pred_speed ~
      1 + sqrt_prey_avg_rank +
      cumul_xp_pred +
      total_xp_pred +
 #     cumul_xp_pred : total_xp_pred +
      (1 | predator_id) +
      (1 | avatar_id),
  sigma ~  
      1 + sqrt_prey_avg_rank +
      cumul_xp_pred +
      total_xp_pred +
#      cumul_xp_pred : total_xp_pred +
      (1 | predator_id)
) + gaussian()



# Prey speed at three levels of experience ------------------------------

prey_speed <- bf(
  prey_avg_speed ~
      1 + sqrt_prey_avg_rank +
      cumul_xp_pred +
      total_xp_pred +
#      cumul_xp_pred : total_xp_pred +
      (1 | predator_id) +
      (1 | avatar_id),
  sigma ~  
      1 + sqrt_prey_avg_rank +
      cumul_xp_pred +
      total_xp_pred +
#      cumul_xp_pred : total_xp_pred +
      (1 | predator_id)
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
success <- bf(
  hunting_success | vint(4) ~ 
      1 + sqrt_game_duration +
      cumul_xp_pred +
      total_xp_pred +
#      cumul_xp_pred : total_xp_pred +
      (1 | predator_id)
) + beta_binomial2



# priors ----------------------------------------------------------------

priors <- c(
  # Prior on game duration
  set_prior("normal(1, 0.5)", 
            class = "b",
            coef = "sqrt_game_duration",
            resp = "huntingsuccess"),
  # Prior on prey rank
  set_prior("normal(2.5, 1)", 
            class = "b",
            coef = "sqrt_prey_avg_rank",
            resp = c("predspeed",
                     "preyavgspeed")),
  # Prior on total XP
  set_prior("normal(0, 1)", 
            class = "b",
            coef = "total_xp_pred",
            resp = c("predspeed",
                     "preyavgspeed",
                     "huntingsuccess")),
  # Prior on cumul xp
  set_prior("normal(0, 1)", 
            class = "b",
            coef = "cumul_xp_pred",
            resp = c("predspeed",
                     "preyavgspeed",
                     "huntingsuccess")),
  # priors on var. parameters (brms automatically detects half-normal)
  set_prior("normal(0, 1)",
            class = "sd", # applies to all variance parameters
            resp = c("predspeed",
                     "preyavgspeed",
                     "huntingsuccess")),
  # priors on phi
  set_prior("normal(2, 1)",
            class = "phi",
            resp = "huntingsuccess")
)

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Run the multivariate model 
# =======================================================================

# ( nitt - burnin ) / thin = 1000
mv_model <- brm(pred_speed +
                prey_speed +
                success +
                set_rescor(FALSE),
                warmup = 1000, 
                iter = 23000,
                thin = 88,
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

saveRDS(mv_model, file = "02B2_DHMLM.rds")

# =======================================================================
# =======================================================================