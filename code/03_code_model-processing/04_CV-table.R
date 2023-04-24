# =======================================================================

#                   Create a CV table from the DHMLM

# =======================================================================





# =======================================================================
# 1. Prepare the script
# =======================================================================



# Load libraries and data -----------------------------------------------

 # Libs
 library(brms)
 library(data.table)
 
 
 # Data
 data <- fread("./data/FraserFrancoetalXXXX-data.csv",
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



# Load model output -----------------------------------------------------
 
 # Model object
 path <- file.path(getwd(), "outputs", "02_outputs_models")
 fit <- readRDS(file.path(path, "02B_DHMLM.rds"))

 # Extract draws for sd of mu and sigma
 draws <- data.table(
     as_draws_df(
         fit,
         variable = c("^sd_predator_id__"),
         regex = TRUE)
 )
 
 # Delete columns
 draws[ , c(13:18) := NULL]
 
# =======================================================================
# =======================================================================





# =======================================================================
# 2. Transform the draws table
# =======================================================================


# Reshape the table -----------------------------------------------------

 # Long format
 table <- melt(draws,
               measure = patterns("^sd_predator_id"),
               variable.name = "Parameter")



# Work on columns -------------------------------------------------------

 # Add xp level
 table[, xp_level := ifelse(Parameter %like% "novice",
                            "novice",
                            "unknown")]
 table[Parameter %like% "interm", xp_level := "interm"]
 table[Parameter %like% "advanced", xp_level := "advanced"]
 
 # Add variable
 table[, variable := ifelse(Parameter %like% "preyspeed",
                            "prey_speed",
                            "pred_speed")]
 
 # Change parameter factor levels to sigma or mu

 table[, Parameter := ifelse(Parameter %like% "sigma", "sigma", "mu")]
 
 # Re-order the columns
 table <- table[, c(1,3,4,2)]
 


# Sdev to variances -----------------------------------------------------

 table[Parameter == "mu", value := value^2]
 table[Parameter == "sigma", value := exp(value^2)]
 
 # Need to confirm if exp should be computed before or after squaring***
 
# =======================================================================
# =======================================================================





# =======================================================================
# 3. Calculate coefficient of variation for mu and sigma
# =======================================================================


# Extract the mean of traits at each xp level ---------------------------
 
 # Predator speed
 mean_speed1 <- mean(data[xp_level == "novice", pred_speed])
 mean_speed2 <- mean(data[xp_level == "intermediate", pred_speed])
 mean_speed3 <- mean(data[xp_level == "advanced", pred_speed])
 
 # Prey speed
 mean_preyspeed1 <- mean(data[xp_level == "novice",
                              prey_avg_speed],
                         na.rm = TRUE)
 mean_preyspeed2 <- mean(data[xp_level == "intermediate",
                              prey_avg_speed],
                         na.rm = TRUE)
 mean_preyspeed3 <- mean(data[xp_level == "advanced",
                              prey_avg_speed],
                         na.rm = TRUE)

 # Add values in a column
 table[xp_level == "novice" & variable == "pred_speed", mean := mean_speed1]
 table[xp_level == "interm" & variable == "pred_speed", mean := mean_speed2]
 table[xp_level == "advanced" & variable == "pred_speed", mean := mean_speed3]
 table[xp_level == "novice" & variable == "prey_speed", mean := mean_preyspeed1]
 table[xp_level == "interm" & variable == "prey_speed", mean := mean_preyspeed2]
 table[xp_level == "advanced" & variable == "prey_speed", mean := mean_preyspeed3]



# Apply transformation --------------------------------------------------

 # Mu pred
 table[Parameter == "mu",
       cv_mu := sqrt(value / mean),
       by = c("xp_level", "variable")]


 # Sigma
 table[Parameter == "sigma", 
       cv_sigma := sqrt(value - 1),
       by = c("xp_level", "variable")]

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Prepare a synthetic table
# =======================================================================


# Rearrange -------------------------------------------------------------

 tab1 <- table[Parameter == "mu", c("Parameter", "xp_level",
                                    "variable", "cv_mu")]
 tab2 <- table[Parameter == "sigma", c("Parameter", "xp_level",
                                       "variable", "cv_sigma")]
 
 setnames(tab1, "cv_mu", "cv")
 setnames(tab2, "cv_sigma", "cv")
 
 cv_tab <- rbind(tab1, tab2)



# Summarize the values (means + 95% CI) ---------------------------------

 # Intervals
 lower_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[1]}
 upper_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[2]}


 cv_tab[, ":=" (mean = mean(cv),
                lower_ci = lower_interval(cv),
                upper_ci = upper_interval(cv)),
           by = .(Parameter, xp_level, variable)]

 cv_tab <- unique(cv_tab[, c(1:3, 5:7)])

 # Round values to 3 digits
 cv_tab[, c(4:6) := lapply(.SD, function (x) {round(x, digits = 3)}),
          .SDcols = c(4:6)]



# Save the table --------------------------------------------------------

 path <- file.path(getwd(),"outputs", "04_outputs_model-processing")
 saveRDS(cv_tab, file = file.path(path, "04_CV-table.rds"))

# =======================================================================
# =======================================================================