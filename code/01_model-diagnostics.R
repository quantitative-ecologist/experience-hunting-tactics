# =======================================================================

#                     Multivariate model diagnostics                    #

# =======================================================================





# =======================================================================
# 1. Load libraries, datasets, and models
# =======================================================================

# Activate project environment ------------------------------------------

renv::activate()



# Librairies ------------------------------------------------------------

library(data.table)
library(brms)
library(bayesplot)
#library(broom.helpers)



# Import the data -------------------------------------------------------

data <- fread("C:/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Data Behaviour/03_final-data/03_final-data_2021/final-data.csv",
              select = c("player_encode_id", "match_encode_id", 
                          "game_duration", "hook_count",
                         "hunting_success", "cumul_xp_pred_bins",
                         "pred_speed", "pred_amount_tiles_visited",
                         "ambush_time_close", "latency_1st_capture",
                         "prey_avg_speed", "prey_avg_amount_tiles_visited", 
                         "prey_total_heal_count", "prey_total_unhook_count"),
              stringsAsFactors = TRUE)

# Load model
model <- readRDS("./outputs/multivariate_model.rds")
print(object.size(model), units = "MB")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Basic model diagnostics
# =======================================================================

# Trace plots and parameter distributions -------------------------------
plot(model)

# =======================================================================
# =======================================================================




