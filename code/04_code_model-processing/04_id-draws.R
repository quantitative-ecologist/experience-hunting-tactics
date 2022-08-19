# =======================================================================

#              Prepare a table to process behavioral types

# =======================================================================





# =======================================================================
# 1. Prepare the script
# =======================================================================

# load libraries
library(brms)
library(data.table)

# import model
fit <- readRDS("./outputs/02_outputs_models/02B_DHMLM.rds")

# Load data
data <- fread("./data/FraserFrancoetalXXXX-data.csv",
              select = c("predator_id",
                         "pred_game_duration",
                         "pred_speed",
                         "prey_avg_speed",
                         "cumul_xp_killer",
                         "total_xp_killer",
                         "hunting_success"))

data <- unique(data)

# Predator id as factor
data[, predator_id := as.factor(predator_id)]


data[cumul_xp_killer < 150,
     xp_level := "novice"]

data[cumul_xp_killer %between% c(150, 299),
     xp_level := "intermediate"]

data[cumul_xp_killer >= 300,
     xp_level := "advanced"]

# Encode the variable as a factor
data[, xp_level := as.factor(xp_level)]

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Extract the posterior draws of each player
# =======================================================================

draws <- data.table(
    as_draws_df(
        fit,
        variable = c("^r_predator_id__"),
        regex = TRUE)
)

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Reshape the table and save
# =======================================================================



# Long format -----------------------------------------------------------

draws <- melt(draws,
              measure = patterns("^r_predator_id"),
              variable.name = "ID_raw")

draws[, c(1:3) := NULL]



# Add variables ---------------------------------------------------------

# Add xp level
draws[, xp_level := ifelse(ID_raw %like% "novice", "novice", "unknown")]
draws[ID_raw %like% "interm", xp_level := "interm"]
draws[ID_raw %like% "advanced", xp_level := "advanced"]

# Add predator ID
draws[, ID_raw := as.character(ID_raw)]
draws[, predator_id := gsub("[]_,a-zA-Z,[]", "", ID_raw)]

# Add variable
draws[, variable := ifelse(Parameter %like% "Zspeed", 
                           "Zspeed",
                           "unknown")]
draws[ID_raw %like% "success", variable := "success"]
draws[ID_raw %like% "Zpreyspeed", variable := "Zpreyspeed"]

# Add variable telling if its sigma or not
draws[, sigma := ifelse(ID_raw %like% "sigma", 1, 0)]

# Delete older column
draws[, ID_raw := NULL]



# Save the table --------------------------------------------------------

saveRDS(draws, file = "./tests/04_id-draws.rds")

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Produce the final table - 1 value per player
# =======================================================================

# NEED TO BAKC TRANSFORM BEFORE REPORTING MEANS

# Compute means and CIs -------------------------------------------------

# Intervals
#lower_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[1]}
#upper_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[2]}

# Compute mean values by ID
#draws[, ":="(mean = mean(value),
#             lower_ci = lower_interval(value),
#             upper_ci = upper_interval(value)),
#        by = .(predator_id, xp_level, sigma, variable)]

# Create table with unique values
#id_tab <- unique(draws[, c(2:8)])

# Round values to 3 digits
#id_tab[, c(5:7) := 
#            lapply(.SD, function (x) {round(x,  digits = 3)}),
#          .SDcols = c(5:7)]



# Save the table --------------------------------------------------------

#saveRDS(id_tab, file = "./tests/results_ISBE/04_id_means-table.rds")

# =======================================================================
# =======================================================================