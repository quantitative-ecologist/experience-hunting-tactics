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
 fit <- readRDS("./tests/02B_DHMLM.rds")
 
 # Load data
 data <- fread("./data/FraserFrancoetalXXXX-data.csv",
               select = c("predator_id",
                          "pred_game_mode",
                          "pred_game_duration",
                          "pred_speed",
                          "prey_avg_speed",
                          "cumul_xp_killer",
                          "total_xp_killer",
                          "hunting_success"))
 
 data <- unique(data)
 data <- data[pred_game_mode == "Online"]
 
 # Experience column
 data[cumul_xp_killer < 100,
      xp_level := "novice"]
 
 data[cumul_xp_killer %between% c(100, 299),
      xp_level := "intermediate"]
 
 data[cumul_xp_killer >= 300,
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



# Save the table --------------------------------------------------------

 # Path
 path <- "./outputs/04_outputs_model-processing"

 # Save
 saveRDS(draws, file = file.path(path, "04_id-draws.rds")

# =======================================================================
# =======================================================================