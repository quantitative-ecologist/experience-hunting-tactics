# =======================================================================

#                     Run a multivariate model                          #
#               that tests the effect of xp on tactics                  #

# =======================================================================





# =======================================================================
# 1. Load libraries, and import dataset
# =======================================================================



# Detect number of cores ------------------------------------------------
options(mc.cores = parallel::detectCores())



# Load libraries --------------------------------------------------------

library(data.table)
library(brms)
library(parallel)



# Import the data -------------------------------------------------------

data <- fread("/home/maxime11/projects/def-monti/maxime11/data/final-data.csv",
              select = c("player_encode_id", "match_encode_id", 
                          "game_duration", "hook_count",
                         "hunting_success", "cumul_xp_pred_bins",
                         "pred_speed", "pred_amount_tiles_visited",
                         "ambush_time_close", "latency_1st_capture",
                         "prey_avg_speed", "prey_avg_amount_tiles_visited", 
                         "prey_total_heal_count", "prey_total_unhook_count"),
              stringsAsFactors = TRUE)

#data <- fread("C:/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Data Behaviour/03_final-data/#03_final-data_2021/final-data.csv",
#              select = c("player_encode_id", "match_encode_id", 
#                          "game_duration", "hook_count",
#                         "hunting_success", "cumul_xp_pred_bins",
#                         "pred_speed", "pred_amount_tiles_visited",
#                         "ambush_time_close", "latency_1st_capture",
#                         "prey_avg_speed", "prey_avg_amount_tiles_visited", 
#                         "prey_total_heal_count", "prey_total_unhook_count"),
#              stringsAsFactors = TRUE)


# There are 40 NaN observatinos for prey_avg_speed 
# and prey_avg_amount_tiles_visited

# These observations are removed from the model

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
# at different levels of experience

# Experience will thus be a random factor with 3 levels. All traits will
# then need to be computed as variables at these levels of experience



# Create dummy variables ------------------------------------------------

# This is done so the model can subsample the rows
data[, sub1 := ifelse(cumul_xp_pred_bins == "novice", 1, 0)]
data[, sub2 := ifelse(cumul_xp_pred_bins == "intermediate", 1, 0)]
data[, sub3 := ifelse(cumul_xp_pred_bins == "advanced", 1, 0)]



# Transform variables ---------------------------------------------------

# Selected variables from the data-exploration file
# raw speed
# raw space covered
# sqrt ambush
# log latency
# raw prey speed

data[, ":=" (sqrt_ambush_time_close = sqrt(ambush_time_close),
             log_latency_1st_capture = log(latency_1st_capture + 1)) ]

# There is 1 value of latency in the dataset that is of 0
# Maybe remove it if it adds weight to the model? (probably not)



# Standardise the variables (Z-scores) ----------------------------------

# Create the function
standardize <- function (x) {(x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}

# Apply the function and create new columns
# The function standardizes the variables by group :
# in this case, by level of experience

data[, c("Zspeed_group", "Zspace_group", 
         "Zambush_group", "Zlatency_group",
         "Zprey_speed_group") :=
       lapply(.SD, standardize), 
       .SDcols = c("pred_speed", "pred_amount_tiles_visited", 
                   "ambush_time_close", "log_latency_1st_capture", 
                   "prey_avg_speed"),
       by = cumul_xp_pred_bins]

# Compute the new columns separated by experience
data[, ":=" (Zspeed_novice        = ifelse(cumul_xp_pred_bins == "novice", Zspeed_group, NA),
             Zspace_novice        = ifelse(cumul_xp_pred_bins == "novice", Zspace_group, NA),
             Zambush_novice       = ifelse(cumul_xp_pred_bins == "novice", Zambush_group, NA),
             Zlatency_novice      = ifelse(cumul_xp_pred_bins == "novice", Zlatency_group, NA),
             Zprey_speed_novice   = ifelse(cumul_xp_pred_bins == "novice", Zprey_speed_group, NA),
             Zspeed_interm        = ifelse(cumul_xp_pred_bins == "intermediate", Zspeed_group, NA),
             Zspace_interm        = ifelse(cumul_xp_pred_bins == "intermediate", Zspace_group, NA),
             Zambush_interm       = ifelse(cumul_xp_pred_bins == "intermediate", Zambush_group, NA),
             Zlatency_interm      = ifelse(cumul_xp_pred_bins == "intermediate", Zlatency_group, NA),
             Zprey_speed_interm   = ifelse(cumul_xp_pred_bins == "intermediate", Zprey_speed_group, NA),
             Zspeed_advanced      = ifelse(cumul_xp_pred_bins == "advanced", Zspeed_group, NA),
             Zspace_advanced      = ifelse(cumul_xp_pred_bins == "advanced", Zspace_group, NA),
             Zambush_advanced     = ifelse(cumul_xp_pred_bins == "advanced", Zambush_group, NA),
             Zlatency_advanced    = ifelse(cumul_xp_pred_bins == "advanced", Zlatency_group, NA),
             Zprey_speed_advanced = ifelse(cumul_xp_pred_bins == "advanced", Zprey_speed_group, NA))]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Build the multivariate model 
# =======================================================================

# We first compute submodels for each level of experience
# These submodels will be added in a joint model that
# will estimate all the covariances



# Speed at three levels of experience -----------------------------------

speed_novice <-       bf(Zspeed_novice | subset(sub1) ~
                          game_duration +
                          (1 |a| player_encode_id)) +
                      gaussian()

speed_intermediate <- bf(Zspeed_interm | subset(sub2) ~
                          game_duration +
                          (1 |b| player_encode_id)) +
                      gaussian()

speed_advanced <-     bf(Zspeed_advanced | subset(sub3) ~
                          game_duration +
                          (1 |c| player_encode_id)) +
                      gaussian()


# Space covered at three levels of experience ---------------------------

#space_novice <-       bf(Zspace | subset(sub1) ~
#                          game_duration +
#                          (1 |a| player_encode_id)) +
#                      gaussian()
#
#space_intermediate <- bf(Zspace | subset(sub2) ~
#                          game_duration +
#                          (1 |b| player_encode_id)) +
#                      gaussian()
#
#space_advanced <-     bf(Zspace | subset(sub3) ~
#                          game_duration +
#                          (1 |c| player_encode_id)) +
#                      gaussian()


# Ambush at three levels of experience ----------------------------------

ambush_novice <-        bf(Zambush_novice | subset(sub1) ~
                            game_duration +
                            (1 |a| player_encode_id)) +
                       gaussian()

ambush_intermediate <- bf(Zambush_interm | subset(sub2) ~
                            game_duration +
                            (1 |b| player_encode_id)) +
                       gaussian()

ambush_advanced <-     bf(Zambush_advanced | subset(sub3) ~
                             game_duration +
                             (1 |c| player_encode_id)) +
                       gaussian()


# Latency for the 1st capture at three levels of experience -------------

latency_novice <-       bf(Zlatency_novice | subset(sub1) ~
                            game_duration +
                            (1 |a| player_encode_id)) +
                        gaussian()

latency_intermediate <- bf(Zlatency_interm | subset(sub2) ~
                            game_duration +
                            (1 |b| player_encode_id)) +
                        gaussian()

latency_advanced <-     bf(Zlatency_advanced | subset(sub3) ~
                            game_duration +
                            (1 |c| player_encode_id)) +
                        gaussian()


# Prey speed at three levels of experience ------------------------------

prey_speed_novice <-       bf(Zprey_speed_novice | subset(sub1) ~
                                game_duration +
                                (1 |a| player_encode_id), 
                              sigma ~ 
                                game_duration + 
                                (1 |a| player_encode_id)) +
                           gaussian()

prey_speed_intermediate <- bf(Zprey_speed_interm | subset(sub2) ~
                                game_duration +
                                (1 |b| player_encode_id), 
                              sigma ~ 
                                game_duration + 
                                (1 |b| player_encode_id)) +
                           gaussian()

prey_speed_advanced <-     bf(Zprey_speed_advanced | subset(sub3) ~
                                game_duration +
                                (1 |c| player_encode_id), 
                              sigma ~ 
                                game_duration + 
                                (1 |c| player_encode_id)) +
                           gaussian()



# priors ----------------------------------------------------------------

priors <- c(
  set_prior("lkj(2)", 
            class = "cor",
            group = "player_encode_id")
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
              #  space_novice +
              #  space_intermediate +
              #  space_advanced +
                ambush_novice +
                ambush_intermediate +
                ambush_advanced +
                latency_novice +
                latency_intermediate +
                latency_advanced +
                prey_speed_novice +
                prey_speed_intermediate +
                prey_speed_advanced + 
                set_rescor(FALSE),
              warmup = 300, 
              iter = 8300,
              thin = 8,
              chains = 4, 
              inits = "0",
              threads = threading(10),
              backend = "cmdstanr",
              seed = 123,
              prior = priors,
              control = list(adapt_delta = 0.95),
              save_pars = save_pars(all = TRUE),
              data = data)

saveRDS(mv_model, file = "multivariate_model.rds")

# =======================================================================
# =======================================================================