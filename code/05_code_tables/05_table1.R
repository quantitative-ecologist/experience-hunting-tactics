
# ===========================================================================

#                         Code to produce Table 1                           #

# ===========================================================================





# ===========================================================================
# 1. Load librairies and import table
# ===========================================================================

 # Load libraries
 library(data.table)
 library(flextable)
 library(officer)
 library(dplyr)
 
 # Source the files
 path <- file.path(getwd(), "outputs", "03_outputs_model-validation")
 loo_tab <- data.table(readRDS(file.path(path, "03A_lootable.rds")))
 
 # Add model column
 loo_tab[, model := c("Global smoother (GS)",
                      "GS + group-level smoothers",
                      "Group-level smoothers only")]
 
 # Reorder columns
 loo_tab <- loo_tab[, c(9, 1:8)]

# ===========================================================================
# ===========================================================================





# ===========================================================================
# 3. Compute the table using flextable
# ===========================================================================

# Prepare the table parameters ----------------------------------------------

 # Custom header
 my_header <- data.frame(
   col_keys = c("model",
                "elpd_diff",
                "se_diff",
                "elpd_loo",
                "se_elpd_loo"),
   line1 = c("model",
             "elpd \ndifference",
             "sd \ndifference",
             "elpd loo \nvalue",
             "elpd loo \nstandard error"),
   stringsAsFactors = FALSE
 )
 
 # Custom theme
 my_theme <- function(x, ...) {
   x <- colformat_double(x, big.mark = " ", 
                         decimal.mark = ".",
                         digits = 2)
   x <- set_table_properties(x, layout = "fixed")
   x <- border_remove(x)
   std_border <- fp_border(width = 1, color = "black")
   x <- hline_top(x, part = "all", border = std_border)
   x <- hline_bottom(x, part = "all", border = std_border)
   autofit(x)
 }



# Create the table ----------------------------------------------------------

 loo_table <- loo_tab %>%
   select(model, elpd_diff, se_diff, elpd_loo, se_elpd_loo) %>%
   flextable(col_keys = my_header$col_keys) %>%
   set_header_df(mapping = my_header, key = "col_keys") %>%
   my_theme() %>%
   merge_v(part = "header") %>%
   merge_h(part = "header") %>%
   fontsize(size = 10, part = "all") %>%
   font(fontname = "Times New Roman", part = "all") %>%
   align(align = "left", part = "all", j = 1) %>%
   align(align = "center", part = "all", j = 2) %>%
   align(align = "center", part = "all", j = 3) %>%
   align(align = "center", part = "all", j = 4) %>%
   align(align = "center", part = "all", j = 5) 
 
 # Save the table
 path1 <- file.path(getwd(), "outputs", "05_outputs_tables")
 save_as_image(
   loo_table,
   file.path(path1, "05_table1.png"),
   webshot = "webshot2"
 )
 
 # Save R object
 saveRDS(loo_table, file = file.path(path1, "05_table1.rds"))

# ===========================================================================
# ===========================================================================