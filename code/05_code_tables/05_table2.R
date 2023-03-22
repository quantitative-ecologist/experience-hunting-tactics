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
     "predator speed", "predator speed", 
     "prey speed", "prey speed", "hunting success"), 
     6
   )
 ]

 # Rename parameter
 eff_tab[
    , parameter := rep(
       c(
          rep(c("intercept (mean)", "intercept (sigma)"), 2),
          "intercept (mean)",
          rep(c("prey rank (mean)", "prey rank (sigma)"), 2),
          "match duration (mean)"
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
 table <- eff_tab[c(3,5,4,6, 7,9,8,10, 1,2)]
 
 # Because I can't align hunting success properly
 table[10,1] <- " "

# ===========================================================================
# ===========================================================================





# ===========================================================================
# 5. Create the table using flextable
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
 saveRDS(mdhglm_table, file = file.path(path1, "05_table2.rds"))
 
# ===========================================================================
# ===========================================================================