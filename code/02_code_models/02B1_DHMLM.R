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

# Project path for testing
data <- fread("./data/FraserFrancoetalXXXX-data.csv",
              select = c("predator_id",
                         "hunting_success",
                         "cumul_xp_killer",
                         "pred_game_duration",
                         "pred_speed",
                         "total_chase_duration",
                         "prey_avg_speed"))

# To run the model on a subsample of players
#set.seed(123)
#chosen <- sample(unique(data$player_encode_id), 15)
#dat1 <- subset(data, player_encode_id %in% chosen)

data <- unique(data)

# Predator id as factor
data[, predator_id := as.factor(predator_id)]

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
  # Novice = 0 to 100
  # Intermediate = >101 to 350
  # Expert = 351 to 500



# Create dummy variable ------------------------------------------------

# This is done so the model can estimate effects by experience
data[cumul_xp_killer <= 100,
     xp_level := "novice"]

data[cumul_xp_killer %between% c(101, 350),
     xp_level := "intermediate"]

data[cumul_xp_killer > 350,
     xp_level := "expert"]

# Encode the variable as a factor
data[, xp_level := as.factor(xp_level)]



# Transform variables ---------------------------------------------------

data[, ":=" (sqrt_total_chase_duration = sqrt(total_chase_duration),
             sqrt_game_duration = sqrt(pred_game_duration)) ]



# Standardize the variables (Z-scores) ----------------------------------

# Create the function
standardize <- function (x) {(x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}

# Apply the function and create new columns
# The function standardizes the variables by group :
# in this case, by level of experience

data[, c("Zpred_speed",
         "Zsqrt_total_chase_duration",
         "Zprey_avg_speed",
         "Zsqrt_game_duration") :=
       lapply(.SD, standardize), 
       .SDcols = c("pred_speed",
                   "sqrt_total_chase_duration",
                   "prey_avg_speed",
                   "sqrt_game_duration"),
       by = xp_level]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Build the multivariate model equations
# =======================================================================



# Speed at three levels of experience -----------------------------------

pred_speed <- bf(
  Zpred_speed ~
      1 +
      Zgame_duration + 
      (1 |a| gr(predator_id, by = xp_level)),
  sigma ~ 1 + (1 |a| gr(player_encode_id, by = xp_level))
) + gaussian()



# Total chase duration at three levels of experience --------------------

#chase_duration <- bf(
#  Zchase_duration ~
#      1 +
#      Zgame_duration +
#      (1 |a| gr(player_encode_id, by = xp_level))
#) + gaussian()



# Prey speed at three levels of experience ------------------------------

prey_speed <- bf(
  Zprey_avg_speed ~
      1 +
      Zgame_duration +
      (1 |a| gr(player_encode_id, by = xp_level)),
  sigma ~ 1 + (1 |a| gr(player_encode_id, by = xp_level))
) + gaussian()



# priors ----------------------------------------------------------------

priors <- c(
  # priors on game duration
  set_prior("normal(0, 2)", 
            class = "b",
            coef = "Zgame_duration",
            resp = c("Zpredspeed", "Zpreyavgspeed")),
  # priors on var. parameters (brms automatically detects half-normal)
  set_prior("normal(0, 1)",
            class = "sd", # applies to all variance parameters
            resp = c("Zpredspeed", "Zpreyavgspeed")),
  # prior on covariance matrices
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
mv_model <- brm(pred_speed +
                prey_speed +
                set_rescor(FALSE),
                warmup = 500, 
                iter = 1500,
                thin = 4,
                chains = 4, 
                inits = "0",
                threads = threading(10),
                backend = "cmdstanr",
                seed = 123,
                prior = priors,
                sample_prior = TRUE,
                control = list(adapt_delta = 0.99),
                #save_pars = save_pars(all = TRUE),
                data = data)

saveRDS(mv_model, file = "02B1_DHMLM.rds") 

# =======================================================================
# =======================================================================