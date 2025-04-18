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
 path <- file.path(getwd(), "outputs", "01_outputs_models")
 fit <- readRDS(file.path(path, "B1_DHMLM-no-outlier.rds"))

 # Load data
 data <- fread("./data/FraserFrancoetal2023-data.csv",
               select = c("predator_id",
                          "game_duration",
                          "pred_speed",
                          "prey_avg_speed",
                          "cumul_xp_pred",
                          "total_xp_pred",
                          "hunting_success"))

 # Experience column
 data[cumul_xp_pred < 100,
      xp_level := "novice"]

 data[cumul_xp_pred %between% c(100, 299),
      xp_level := "intermediate"]

 data[cumul_xp_pred >= 300,
      xp_level := "advanced"]

 # Encode variables as a factor
 data[, xp_level := as.factor(xp_level)]
 data[, predator_id := as.factor(predator_id)]

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
 draws[, variable := ifelse(ID_raw %like% "speed",
                            "pred_speed",
                            "unknown")]
 draws[ID_raw %like% "success", variable := "success"]
 draws[ID_raw %like% "preyspeed", variable := "prey_speed"]

 # Add variable telling if its sigma or not
 draws[, sigma := ifelse(ID_raw %like% "sigma", 1, 0)]

 # Delete older column
 draws[, ID_raw := NULL]

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Produce the final table - 1 value per player
# =======================================================================


# Compute means and CIs -------------------------------------------------

 # Intervals
 lower_interval <- function(x) {
     coda::HPDinterval(as.mcmc(x), 0.95)[1]
 }
 upper_interval <- function(x) {
     coda::HPDinterval(as.mcmc(x), 0.95)[2]
 }

 # Compute mean values by ID
 draws[, ":="(mean_estimated = mean(value),
              lower_ci_estimated = lower_interval(value),
              upper_ci_estimated = upper_interval(value)),
         by = .(predator_id, xp_level, sigma, variable)]



# Save the table --------------------------------------------------------

 # Path
 path1 <- file.path(getwd(), "outputs", "03_outputs_model-processing")

 saveRDS(draws, file = file.path(path1, "DHMLM_id-draws.rds"))

# =======================================================================
# =======================================================================