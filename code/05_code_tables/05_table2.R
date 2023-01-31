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
     "mean predator speed", "sigma predator speed", 
     "mean prey speed", "sigma prey speed", "mean success"), 
     6
   )
 ]
 
 # Rename parameter
 eff_tab[
    , parameter := rep(
       c(
          rep("intercept", 5),
          rep("mean prey rank", 4),
          "match duration"
       ),
       3
    )
 ]
 
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
   trait + parameter ~ xp_level,
   value.var = "estimate"
 )
 
 # Reorder columns
 eff_tab <- eff_tab[, c(1,2,5,4,3)]
 
 # Reorder rows
 eff_tab <- eff_tab[c(1,2,7,8,3,4,9,10,5,6)]
 
# ===========================================================================
# ===========================================================================





# ===========================================================================
# 3. Random effects table
# ===========================================================================


# Prepare data --------------------------------------------------------------
 
 # Extract the random effect variances
 sd_draws <- data.table(
    as_draws_df(
       fit, variable = "^sd_", regex = TRUE
    )
 )
 
 sd_draws[, c(28:30) := NULL]



# Calculate parameters -------------------------------------------------------
 
 # Take the exponent of sigma parameters
 sd_draws[
    , c("sd_predator_id__sigma_speednovice_Intercept",
        "sd_predator_id__sigma_speedinterm_Intercept",
        "sd_predator_id__sigma_speedadvanced_Intercept",
        "sd_predator_id__sigma_preyspeednovice_Intercept",
        "sd_predator_id__sigma_preyspeedinterm_Intercept",
        "sd_predator_id__sigma_preyspeedadvanced_Intercept"
    ) 
    := lapply(.SD, function (x) {exp(x)}),
    .SDcols = c(
       "sd_predator_id__sigma_speednovice_Intercept",
       "sd_predator_id__sigma_speedinterm_Intercept",
       "sd_predator_id__sigma_speedadvanced_Intercept",
       "sd_predator_id__sigma_preyspeednovice_Intercept",
       "sd_predator_id__sigma_preyspeedinterm_Intercept",
       "sd_predator_id__sigma_preyspeedadvanced_Intercept"
    )
 ]



# Reshape the table -----------------------------------------------------------
 
  ranef_tab <- melt(
     sd_draws,
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
 
 # Parameter column
 ranef_tab[parameter %like% "id", parameter1 := "SD predator ID"]
 ranef_tab[parameter %like% "avatar", parameter1 := "SD avatar ID"]
 ranef_tab[parameter %like% "env", parameter1 := "SD environment ID"]
 ranef_tab[parameter %like% "success", parameter1 := "SD predator ID"]
 
 # Trait column
 ranef_tab[parameter %like% "speed", 
           trait := ifelse(
              parameter %like% "sigma_speed" ,
              "sigma predator speed",
              "mean predator speed"
           )
 ]
 ranef_tab[parameter %like% "preyspeed",
           trait := ifelse(
              parameter %like% "sigma_preyspeed" ,
              "sigma prey speed",
              "mean prey speed"
           )
 ]
 ranef_tab[parameter %like% "success", trait := "mean success"]
 
 # Remove old parameter column and replace by new
 ranef_tab[, parameter := NULL]
 setnames(ranef_tab, "parameter1", "parameter")
 
 # Reshape
 ranef_tab <- dcast(
    ranef_tab,
    trait + parameter ~ xp_level,
    value.var = "estimate"
 )
 
 # Reorder columns
 ranef_tab <- ranef_tab[, c(1,2,5,4,3)]
 
 # Reorder rows
 ranef_tab <- ranef_tab[c(1:3,8,4:6,9,7)]
 
# ===========================================================================
# ===========================================================================





# ===========================================================================
# 5. Create the table using flextable
# ===========================================================================
 
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
   ranef_tab
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
   italic(i = c(1, 12), j = 1, part = "body") %>%
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