# ===========================================================================

#                         Code to produce Table 2                           #

# ===========================================================================





# ===========================================================================
# 1. Load libraries and import the model fit
# ===========================================================================


# Load libraries
library(data.table)
library(flextable)
library(officer)
library(dplyr)
library(brms)

# path
path <- file.path(getwd(), "outputs")
  
# Import the model
fit <- readRDS(
  file.path(path, "02_outputs_models", "02B_DHMLM.rds")
)

# Import the CV table
cv <- readRDS(
  file.path(path, "04_outputs_model-processing", "04_CV-table.rds")
)

# ===========================================================================
# ===========================================================================





# ===========================================================================
# 2. Fixed effects table
# ===========================================================================


# Prepare the table ---------------------------------------------------------

 # Extract the fixed effects from the model
 eff_tab <- data.table(fixef(fit), keep.rownames = T)
 eff_tab <- eff_tab[, c(1, 2, 4:5)]

 # Rename variables
 setnames(
   eff_tab, 
   c("rn", "Q2.5", "Q97.5"), 
   c("parameter", "lower_ci", "upper_ci")
 )

 # Add a column that specifies the experience level
 eff_tab[parameter %like% "novice", xp_level := "novice"]
 eff_tab[parameter %like% "interm", xp_level := "intermediate"]
 eff_tab[parameter %like% "advanced", xp_level := "advanced"]

 # Round values to 3 digits
 eff_tab[, c(2:4) := round(eff_tab[, c(2:4)], digits = 3)]



# Reorganize the table ------------------------------------------------------
 
 # reorder columns, and then rows based on xp level
 eff_tab <- eff_tab[, c(5, 1:4)]
 setorder(
   eff_tab, 
   cols = -"xp_level"
 )
 
 # Create trait column
 eff_tab[
   , trait := rep(c(
     "mu_pred_speed", "sigma_pred_speed", 
     "mu_prey_speed", "sigma_prey_speed", "mu_success"), 
     6
   )
 ]
 
 # Rename parameter
 eff_tab[, parameter := rep(c(rep("Intercept", 5), rep("Zgame_duration", 5)), 3)]
 
 # Paste upper and lower ci with estimate
 eff_tab[ , estimate := paste(Estimate, "(")]
 eff_tab[, estimate := paste(estimate, lower_ci, sep = "")]
 eff_tab[, estimate := paste(estimate, ",", sep = "")]
 eff_tab[, estimate := paste(estimate, upper_ci, sep = " ")]
 eff_tab[, estimate := paste(estimate, ")", sep = "")]
 eff_tab[, c("Estimate", "lower_ci", "upper_ci") := NULL]
 
 
 # Reshape
 eff_tab <- dcast(
   eff_tab,
   parameter + trait ~ xp_level,
   value.var = "estimate"
 )
 
# ===========================================================================
# ===========================================================================





# ===========================================================================
# 3. Calculate the repeatabilities
# ===========================================================================


# Prepare data --------------------------------------------------------------
 
 # Extract the random effect variances
 var_draws <- data.table(
    as_draws_df(
       fit, variable = "^sd_", regex = TRUE
    )
 )

 var_draws[, c(28:30) := NULL]

 
 # Square each value to obtain the variance
 var_draws[, ]

 # pour calculer la répétabilité, il faut que j'utilise la variance résiduelle de l'intercept
 # raphael fait l'exposant, et ensuite le carré
 
# ===========================================================================
# ===========================================================================







# Extract the coefficient of variation

