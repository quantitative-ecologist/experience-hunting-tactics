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



# Posterior predictive checks -------------------------------------------

# Check distributions
a <- pp_check(model, resp = "Zspeednovice")
b <- pp_check(model, resp = "Zspeedinterm")
c <- pp_check(model, resp = "Zspeedadvanced")
 
d <- pp_check(model, resp = "Zambushnovice")
e <- pp_check(model, resp = "Zambushinterm")
f <- pp_check(model, resp = "Zambushadvanced")
 
g <- pp_check(model, resp = "Zlatencynovice")
h <- pp_check(model, resp = "Zlatencyinterm")
i <- pp_check(model, resp = "Zlatencyadvanced")
 
# Effects plot (mean variance plot)
j <- pp_check(model, resp = "Zspeednovice", type = "stat_2d")
k <- pp_check(model, resp = "Zspeedinterm", type = "stat_2d")
l <- pp_check(model, resp = "Zspeedadvanced", type = "stat_2d")
 
m <- pp_check(model, resp = "Zambushnovice", type = "stat_2d")
n <- pp_check(model, resp = "Zambushinterm", type = "stat_2d")
o <- pp_check(model, resp = "Zambushadvanced", type = "stat_2d")
 
p <- pp_check(model, resp = "Zlatencynovice", type = "stat_2d")
q <- pp_check(model, resp = "Zlatencyinterm", type = "stat_2d")
r <- pp_check(model, resp = "Zlatencyadvanced", type = "stat_2d")



# Add criterions to the model -------------------------------------------

model <- add_criterion(model, 
                       "loo", 
                       resp = c("Zspeednovice", "Zspeedinterm", "Zspeedadvanced",
                                "Zambushnovice", "Zambushinterm", "Zambushadvanced",
                                "Zlatencynovice", "Zlatencyinterm","Zlatencyadvanced",
                                "Zpreyspeednovice", "Zpreyspeedinterm", "Zpreyspeedadvanced")
                        )

# =======================================================================
# =======================================================================




