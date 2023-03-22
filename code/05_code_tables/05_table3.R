# ===========================================================================

#                         Code to produce Table 3                           #

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
# 2. Random effects table
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
 ranef_tab[
    , parameter1 := ifelse(
        parameter %like% "predator_id__sigma",
        "predator ID (sigma)",
        "predator ID (mean)"
    )
 ]
 ranef_tab[parameter %like% "avatar", parameter1 := "avatar (mean)"]
 ranef_tab[parameter %like% "env", parameter1 := "environment (mean)"]
 
 # Trait column
 ranef_tab[
    , trait := ifelse(
        parameter %like% "preyspeed" ,
        "prey speed",
        "predator speed"
    )
 ]
 ranef_tab[parameter %like% "success", trait := "hunting success"]
 
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
 table <- ranef_tab[c(1:5, 6:9)]

# ===========================================================================
# ===========================================================================





# ===========================================================================
# 3. Create the table using flextable
# ===========================================================================

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
   
   # table structure
   table %>%
   select(trait, parameter,
          novice, intermediate, advanced) %>%
   flextable(col_keys = my_header$col_keys) %>%
   set_header_df(mapping = my_header, key = "col_keys") %>%
   my_theme() %>%

   # Align repeated rows on column 1
   merge_v(j = 1) %>%
   valign(j = 1, valign = "top", part = "body") %>%

   # font
   fontsize(size = 10, part = "all") %>%
   font(fontname = "Times New Roman", part = "all") %>%
   
   # Align text
   align(align = "left", part = "all", j = 1) %>%
   align(align = "left", part = "all", j = 2) %>%
   align(align = "center", part = "all", j = 3) %>%
   align(align = "center", part = "all", j = 4) %>%
   align(align = "center", part = "all", j = 5)
   
 # Save the table
 path1 <- file.path("./outputs/05_outputs_tables")
 
 # Save R object
 saveRDS(mdhglm_table, file = file.path(path1, "05_table3.rds"))
 
# ===========================================================================
# ===========================================================================