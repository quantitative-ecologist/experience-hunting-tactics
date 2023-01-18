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
 
 # Back transform success
 eff_tab[
   parameter %like% "success", 
   c(2:4) := lapply(.SD, function (x) {plogis(x)}),
   .SDcols = c(2:4)
 ]
 
 # Back transform sigma
 eff_tab[
   parameter %like% "sigma", 
   c(2:4) := lapply(.SD, function (x) {exp(x)}),
   .SDcols = c(2:4)
 ]
 
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
     "mu predator speed", "sigma predator speed", 
     "mu prey speed", "sigma prey speed", "mu success"), 
     6
   )
 ]
 
 # Rename parameter
 eff_tab[, parameter := rep(c(rep("Intercept", 5), rep("Z Match duration", 5)), 3)]
 
 # Paste upper and lower ci with estimate
 eff_tab[ , estimate := paste(format(Estimate, digits = 3), "(")]
 eff_tab[, estimate := paste(estimate, format(lower_ci, digits = 3), sep = "")]
 eff_tab[, estimate := paste(estimate, ",", sep = "")]
 eff_tab[, estimate := paste(estimate, format(upper_ci, digits = 3), sep = " ")]
 eff_tab[, estimate := paste(estimate, ")", sep = "")]
 eff_tab[, c("Estimate", "lower_ci", "upper_ci") := NULL]
 
 
 # Reshape
 eff_tab <- dcast(
   eff_tab,
   parameter + trait ~ xp_level,
   value.var = "estimate"
 )
 
 # Reorder columns
 eff_tab <- eff_tab[, c(1,2,5,4,3)]
 
 # Reorder rows
 eff_tab
 
# ===========================================================================
# ===========================================================================





# ===========================================================================
# 3. Random effects table
# ===========================================================================


# Prepare data --------------------------------------------------------------
 
 # Extract the random effect variances
 var_draws <- data.table(
    as_draws_df(
       fit, variable = "^sd_", regex = TRUE
    )
 )
 
 var_draws[, c(4,6,8,10,12,14,28:30) := NULL]

 
 # Extract the residual variance (intercepts)
 var_draws1 <- data.table(
    as_draws_df(
       fit, variable = c(
          "b_sigma_speednovice_Intercept",
          "b_sigma_speedinterm_Intercept",
          "b_sigma_speedadvanced_Intercept",
          "b_sigma_preyspeednovice_Intercept",
          "b_sigma_preyspeedinterm_Intercept",
          "b_sigma_preyspeedadvanced_Intercept"
       )
    )
 )
 
 var_draws1[, c(7:9) := NULL]
 
 
 # Combine the tables
 var_draws <- cbind(var_draws, var_draws1)



# Calculate parameters -------------------------------------------------------
 
 # Take the exponent of sigma parameters
 var_draws[
    , c("b_sigma_speednovice_Intercept",
        "b_sigma_speedinterm_Intercept",
        "b_sigma_speedadvanced_Intercept",
        "b_sigma_preyspeednovice_Intercept",
        "b_sigma_preyspeedinterm_Intercept",
        "b_sigma_preyspeedadvanced_Intercept"
    ) 
    := lapply(.SD, function (x) {exp(x)}),
    .SDcols = c(
       "b_sigma_speednovice_Intercept",
       "b_sigma_speedinterm_Intercept",
       "b_sigma_speedadvanced_Intercept",
       "b_sigma_preyspeednovice_Intercept",
       "b_sigma_preyspeedinterm_Intercept",
       "b_sigma_preyspeedadvanced_Intercept"
    )
 ]
 
 
 # Square values to obtain the variance
 var_draws[
    , c(1:27) := 
       lapply(.SD, function (x) {(x)^2}),
    .SDcols = c(1:27)]



# Compute Intra class correlation coefficients --------------------------------
 
 # Calculate total variance for each trait
 var_draws[
    , ":=" (
       var_total_speednovice = rowSums(var_draws[,c(1,2,3,22)]),
       var_total_preyspeednovice = rowSums(var_draws[,c(6,16,17,25)]),
       var_total_speedinterm = rowSums(var_draws[,c(4,12,13,23)]),
       var_total_preyspeedinterm = rowSums(var_draws[,c(7,18,19,26)]),
       var_total_speedadvanced = rowSums(var_draws[,c(5,15,14,24)]),
       var_total_preyspeedadvanced = rowSums(var_draws[,c(8,20,21,27)])
    )
 ]
 
 
 # Ratio of variance (ICC)
 var_draws[
    , ":=" (
       icc_env_speednovice = sd_environment_id__speednovice_Intercept / var_total_speednovice,
       icc_avatar_speednovice = sd_avatar_id__speednovice_Intercept / var_total_speednovice,
       icc_id_speednovice = sd_predator_id__speednovice_Intercept / var_total_speednovice,
       icc_env_speedinterm = sd_environment_id__speedinterm_Intercept / var_total_speedinterm,
       icc_avatar_speedinterm = sd_avatar_id__speedinterm_Intercept / var_total_speedinterm,
       icc_id_speedinterm = sd_predator_id__speedinterm_Intercept / var_total_speedinterm,
       icc_env_speedadvanced = sd_environment_id__speedadvanced_Intercept / var_total_speedadvanced,
       icc_avatar_speedadvanced = sd_avatar_id__speedadvanced_Intercept / var_total_speedadvanced,
       icc_id_speedadvanced = sd_predator_id__speedadvanced_Intercept / var_total_speedadvanced,
       icc_env_preyspeednovice = sd_environment_id__preyspeednovice_Intercept / var_total_preyspeednovice,
       icc_avatar_preyspeednovice = sd_avatar_id__preyspeednovice_Intercept / var_total_preyspeednovice,
       icc_id_preyspeednovice = sd_predator_id__preyspeednovice_Intercept / var_total_preyspeednovice,
       icc_env_preyspeedinterm = sd_environment_id__preyspeedinterm_Intercept / var_total_preyspeedinterm,
       icc_avatar_preyspeedinterm = sd_avatar_id__preyspeedinterm_Intercept / var_total_preyspeedinterm,
       icc_id_preyspeedinterm = sd_predator_id__preyspeedinterm_Intercept / var_total_preyspeedinterm,
       icc_env_preyspeedadvanced = sd_environment_id__preyspeedadvanced_Intercept / var_total_preyspeedadvanced,
       icc_avatar_preyspeedadvanced = sd_avatar_id__preyspeedadvanced_Intercept / var_total_preyspeedadvanced,
       icc_id_preyspeedadvanced = sd_predator_id__preyspeedadvanced_Intercept / var_total_preyspeedadvanced
    )
 ]



# Reshape the table -----------------------------------------------------------
 
 # Only keep ICC columns + success var
 var_draws <- var_draws[, c(9:11, 34:51)]
 
  ranef_tab <- melt(
     var_draws,
     variable.name = "parameter"
  )



# Summarize the values (means + 95% CI) --------------------------------------
 
 # Intervals
 lower_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[1]}
 upper_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[2]}
 
 
 # Summarize values
 ranef_tab[, ":=" (mean = mean(value),
                   lower_ci = lower_interval(value),
                   upper_ci = upper_interval(value)),
           by = .(parameter)]
 
 ranef_tab <- unique(ranef_tab[, .(parameter, mean, lower_ci, upper_ci)])
 ranef_tab[, c(2:4) := round(ranef_tab[,c(2:4)], 3)]



# Rearrange the table -------------------------------------------------------- 
 
 # Add a column that specifies the experience level
 ranef_tab[parameter %like% "novice", xp_level := "novice"]
 ranef_tab[parameter %like% "interm", xp_level := "intermediate"]
 ranef_tab[parameter %like% "advanced", xp_level := "advanced"]
 
 
 # Paste upper and lower ci with estimate
 ranef_tab[ , estimate := paste(format(mean, digits = 3), "(")]
 ranef_tab[, estimate := paste(estimate, format(lower_ci, digits = 3), sep = "")]
 ranef_tab[, estimate := paste(estimate, ",", sep = "")]
 ranef_tab[, estimate := paste(estimate, format(upper_ci, digits = 3), sep = " ")]
 ranef_tab[, estimate := paste(estimate, ")", sep = "")]
 ranef_tab[, c("mean", "lower_ci", "upper_ci") := NULL]
 
 # Parameter and trait column
 ranef_tab[parameter %like% "id", parameter1 := "ICC predator ID"]
 ranef_tab[parameter %like% "avatar", parameter1 := "ICC avatar ID"]
 ranef_tab[parameter %like% "env", parameter1 := "ICC environment ID"]
 ranef_tab[parameter %like% "success", parameter1 := "Variance predator ID"]
 
 ranef_tab[parameter %like% "speed", trait := "mu predator speed"]
 ranef_tab[parameter %like% "preyspeed", trait := "mu prey speed"]
 ranef_tab[parameter %like% "success", trait := "mu success"]
 
 ranef_tab[, parameter := NULL]
 setnames(ranef_tab, "parameter1", "parameter")
 
 # Reshape
 ranef_tab <- dcast(
    ranef_tab,
    parameter + trait ~ xp_level,
    value.var = "estimate"
 )
 
 # Reorder columns
 ranef_tab <- ranef_tab[, c(1,2,5,4,3)]
 
# ===========================================================================
# ===========================================================================

 
 
 
 
# ===========================================================================
# 4. Arrange the coefficient of variation table
# ===========================================================================

 # Paste upper and lower ci with estimate
 cv[ , estimate := paste(format(mean, digits = 3), "(")]
 cv[, estimate := paste(estimate, format(lower_ci, digits = 3), sep = "")]
 cv[, estimate := paste(estimate, ",", sep = "")]
 cv[, estimate := paste(estimate, format(upper_ci, digits = 3), sep = " ")]
 cv[, estimate := paste(estimate, ")", sep = "")]
 cv[, c("mean", "lower_ci", "upper_ci") := NULL]
 
 # Change character values
 cv[
   , variable := c(
     rep("mu predator speed", 3),
       rep("mu prey speed", 3),
       rep("sigma predator speed", 3),
       rep("sigma prey speed", 3)
     )
 ]
 
 # Rename variable to parameter
 cv[, Parameter := NULL]
 setnames(cv, "variable", "trait")
 
 # Add parameter column
 cv[
   , parameter := c(
     rep("CVi predator ID", 6), rep("CPp predator ID", 6)
   )
 ]
 
 # Reshape the table into wide format
 cv <- dcast(
   cv,
   parameter + trait ~ xp_level,
   value.var = "estimate"
 )
 
 # Reorder columns and rows
 cv <- cv[, c(1,2,5,4,3)]
 cv <- cv[c(3,4,1,2)]
 setnames(cv, "interm", "intermediate")
 
# ===========================================================================
# ===========================================================================



 
 
# ===========================================================================
# 5. Create the table using flextable
# ===========================================================================
 
 # Put the trait column first instead
 eff_tab <- eff_tab[, c(2,1,3,4,5)]
 ranef_tab <- ranef_tab[, c(2,1,3,4,5)]
 cv <- cv[, c(2,1,3,4,5)]
 
 
 # Combine the two tables together
 table <- rbind(
   data.frame(
     trait = "Fixed effects",
     parameter = NA,
     novice = NA,
     intermediate = NA,
     advanced = NA),
   eff_tab,
   data.frame(
     trait = "Random effects",
     parameter = NA,
     novice = NA,
     intermediate = NA,
     advanced = NA),
   ranef_tab,
   data.frame(
     trait = "Coefficient of variation",
     parameter = NA,
     novice = NA,
     intermediate = NA,
     advanced = NA),
   cv
 )
 
 

# Prepare the table parameters ----------------------------------------------
 
 # Custom header
 my_header <- data.frame(
   col_keys = c("trait", "parameter",
                "novice", "intermediate", "advanced"),
   line1 = c("Trait",
             "Parameter",
             "Novice",
             "Intermediate",
             "Advanced"),
   stringsAsFactors = FALSE
 )
 
 # Custom theme
 my_theme <- function(x, ...) {
   x <- colformat_double(x, big.mark = " ", 
                         decimal.mark = ".",
                         digits = 3)
   x <- set_table_properties(x, layout = "fixed")
   x <- border_remove(x)
   std_border <- fp_border(width = 1, color = "black")
   x <- hline_top(x, part = "all", border = std_border)
   x <- hline_bottom(x, part = "all", border = std_border)
   autofit(x)
 }
 
 

# Create the table ----------------------------------------------------------
 
 mdhglm_table <- 
   table %>%
   select(trait, parameter,
          novice, intermediate, advanced) %>%
   flextable(col_keys = my_header$col_keys) %>%
   set_header_df(mapping = my_header, key = "col_keys") %>%
   my_theme() %>%
   merge_v(part = "header") %>%
   merge_h(part = "header") %>%
   fontsize(size = 10, part = "all") %>%
   font(fontname = "Times New Roman", part = "all") %>%
   italic(i = c(1, 12, 20), j = 1, part = "body") %>%
   align(align = "left", part = "all", j = 1) %>%
   align(align = "left", part = "all", j = 2) %>%
   align(align = "center", part = "all", j = 3) %>%
   align(align = "center", part = "all", j = 4) %>%
   align(align = "center", part = "all", j = 5)
   
 # Save the table
 path1 <- file.path("./outputs/05_outputs_tables")
 
 # Save R object
 saveRDS(mdhglm_table, file = file.path(path1, "05_table2.rds"))
 
# ===========================================================================
# ===========================================================================