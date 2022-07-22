# =======================================================================

#                     Run a multivariate model                          #
#               that tests the effect of xp on tactics                  #

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
                         "hunting_success",
                         "cumul_xp_killer",
                         "pred_game_duration",
                         "pred_speed",
                         "total_chase_duration",
                         "prey_avg_speed"))

data <- unique(data)

# Predator id as factor
data[, predator_id := as.factor(predator_id)]


# To run the model on a subsample of players
#set.seed(123)
#chosen <- sample(unique(data$player_encode_id), 15)
#dat1 <- subset(data, player_encode_id %in% chosen)

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Prepare the data for the model
# =======================================================================

# Here we prepare the data so the model can estimate trait combinations
# at different levels of expertise

# Expertise will thus be a random factor with 3 levels.
# The random factor is assigned based on the results of the gamm model.
# See figure X for reference ***

# Here :
  # Novice = 0 to 100
  # Intermediate = >101 to 350
  # Expert = 351 to 500

# All traits will then need to be computed as variables 
# at these levels of expertise


# Create dummy variable ------------------------------------------------

# This is done so the model can estimate effects by expertise
data[cumul_xp_killer <= 100,
     xp_level := "novice"]

data[cumul_xp_killer %between% c(101, 350),
     xp_level := "intermediate"]

data[cumul_xp_killer > 350,
     xp_level := "expert"]

# Encode the variable as a factor
data[, xp_level := as.factor(xp_level)]


# This is done so the model can subsample the rows
data[, sub1 := ifelse(xp_level == "novice", 1, 0)]
data[, sub2 := ifelse(xp_level == "intermediate", 1, 0)]
data[, sub3 := ifelse(xp_level == "expert", 1, 0)]



# Transform variables ---------------------------------------------------

#data[, ":=" (sqrt_ambush_time_close = sqrt(ambush_time_close),
#             log_latency_1st_capture = log(latency_1st_capture + 1),
#             sqrt_game_duration = sqrt(game_duration)) ]



# Standardize the variables (Z-scores) ----------------------------------

# Create the function
standardize <- function (x) {(x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}

# Apply the function and create new columns
# The function standardizes the variables by group :
# in this case, by level of experience

data[, c("Zspeed_group",
         "Zprey_speed_group",
         "Zgame_duration") :=
       lapply(.SD, standardize), 
       .SDcols = c("pred_speed",
                   "prey_avg_speed",
                   "pred_game_duration"),
       by = xp_level]

# Compute the new columns for each level of experience
data[, ":=" (Zspeed_novice      = ifelse(xp_level == "novice", Zspeed_group, NA),
             Zprey_speed_novice = ifelse(xp_level == "novice", Zprey_speed_group, NA),
             success_novice     = ifelse(xp_level == "novice", hunting_success, NA),
             Zspeed_interm      = ifelse(xp_level == "intermediate", Zspeed_group, NA),
             Zprey_speed_interm = ifelse(xp_level == "intermediate", Zprey_speed_group, NA),
             success_interm     = ifelse(xp_level == "intermediate", hunting_success, NA),
             Zspeed_expert      = ifelse(xp_level == "expert", Zspeed_group, NA),
             Zprey_speed_expert = ifelse(xp_level == "expert", Zprey_speed_group, NA),
             success_expert     = ifelse(xp_level == "expert", hunting_success, NA))
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
  Zspeed_novice | subset(sub1) ~
      1 + Zgame_duration +
      (1 |a| predator_id),
  sigma ~  
      1 + Zgame_duration +
      (1 |a| predator_id)
) + gaussian()

speed_intermediate <- bf(
  Zspeed_interm | subset(sub2) ~
      1 + Zgame_duration +
      (1 |b| predator_id),
  sigma ~  
      1 + Zgame_duration +
      (1 |b| predator_id)
) + gaussian()

speed_expert <- bf(
  Zspeed_expert | subset(sub3) ~
      1 + Zgame_duration +
      (1 |c| predator_id),
  sigma ~  
      1 + Zgame_duration +
      (1 |c| predator_id)
) + gaussian()



# Prey speed at three levels of experience ------------------------------

prey_speed_novice <- bf(
  Zprey_speed_novice | subset(sub1) ~
      1 + Zgame_duration +
      (1 |a| predator_id),
  sigma ~  
      1 + Zgame_duration +
      (1 |a| predator_id)
) + gaussian()

prey_speed_intermediate <- bf(
  Zprey_speed_interm | subset(sub2) ~
      1 + Zgame_duration +
      (1 |b| predator_id),
  sigma ~ 
      1 + Zgame_duration +
      (1 |b| predator_id)
) + gaussian()

prey_speed_expert <- bf(
  Zprey_speed_expert | subset(sub3) ~
      1 + Zgame_duration +
      (1 |c| predator_id),
  sigma ~  
      1 + Zgame_duration +
      (1 |c| predator_id)
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
      1 + Zgame_duration +
      (1 |a| predator_id)
) + beta_binomial2

success_interm <- bf(
  success_interm | vint(4) + subset(sub2) ~
      1 + Zgame_duration +
      (1 |b| predator_id)
) + beta_binomial2

success_expert <- bf(
  success_expert | vint(4) + subset(sub3) ~
      1 + Zgame_duration +
      (1 |c| predator_id)
) + beta_binomial2



# priors ----------------------------------------------------------------

priors <- c(
  # Prior on game duration
  set_prior("normal(0, 2)", 
            class = "b",
            coef = "Zgame_duration",
            resp = c("Zspeednovice",
                     "Zspeedinterm",
                     "Zspeedexpert",
                     "Zpreyspeednovice",
                     "Zpreyspeedinterm",
                     "Zpreyspeedexpert",
                     "successnovice",
                     "successinterm",
                     "successexpert")),
  # priors on var. parameters (brms automatically detects half-normal)
  set_prior("normal(0, 1)",
            class = "sd", # applies to all variance parameters
            resp = c("Zspeednovice",
                     "Zspeedinterm",
                     "Zspeedexpert",
                     "Zpreyspeednovice",
                     "Zpreyspeedinterm",
                     "Zpreyspeedexpert",
                     "successnovice",
                     "successinterm",
                     "successexpert")),
  # priors on phi
  set_prior("normal(2, 1)",
            class = "phi",
            resp = c("successnovice",
                     "successinterm",
                     "successexpert")),
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
                speed_expert +
                prey_speed_novice +
                prey_speed_intermediate +
                prey_speed_expert +
                success_novice +
                success_interm +
                success_expert +
                set_rescor(FALSE),
                warmup = 500, 
                iter = 2500,
                thin = 8,
                chains = 4, 
                inits = "0",
                threads = threading(10),
                backend = "cmdstanr",
                seed = 123,
                prior = priors,
                sample_prior = TRUE,
                control = list(adapt_delta = 0.99),
                stanvars = stanvars,
                #save_pars = save_pars(all = TRUE),
                data = data)

saveRDS(mv_model, file = "02B_DHMLM.rds")

# =======================================================================
# =======================================================================