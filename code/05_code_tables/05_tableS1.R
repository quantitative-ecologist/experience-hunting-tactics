# ===========================================================================

#                         Code to produce Table S1                          #

# ===========================================================================





# ===========================================================================
# 1. Load librairies and import the model
# ===========================================================================

# Load libraries
library(data.table)
library(flextable)
library(officer)
library(dplyr)
library(brms)

# Import the model
path <- file.path("./outputs/02_outputs_models")
mod1 <- readRDS(file.path(path, "02A3_GAMM.rds"))
mod2 <- readRDS(file.path(path, "02A3_prey-GAMM.rds"))

# ===========================================================================
# ===========================================================================





# ===========================================================================
# 2. Extract the posterior means of each parameter
# ===========================================================================

# Extract the posterior estimates
estim <- data.table(posterior_summary(mod1)[1:6, ], keep.rownames = TRUE)

# Rename the parameters
estim[rn == "b_Intercept", rn := "Intercept"]
estim[rn == "b_Zgame_duration", rn := "Z game duration"]
estim[rn == "bs_sZcumul_xp_1", rn := "f(Z cumulative xp)"]
estim[rn == "sds_sZcumul_xp_1", rn := "SD f(Z cumulative xp)"]
estim[rn == "sds_spredator_id_1", rn := "SD predator ID"]

# Round values
estim[, c(2:5) := 
        lapply(.SD,
               function (x) round(x,digits = 3)),
      .SDcols = c(2:5)]

# ===========================================================================
# ===========================================================================





# ===========================================================================
# 3. Compute the table using flextable
# ===========================================================================

# Prepare the table parameters ----------------------------------------------

# Custom header
my_header <- data.frame(
  col_keys = c("rn", "Estimate",
               "Est.Error", "Q2.5", "Q97.5"),
  line1 = c("Parameter",
            "Posterior mean",
            "Est.Error",
            "2.5%",
            "97.5%"),
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

gamm_table <- estim %>%
  select(rn, Estimate,
         Est.Error, Q2.5, Q97.5) %>%
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
path1 <- file.path("./outputs/05_outputs_tables")
save_as_image(gamm_table,
              file.path(path1, "05_tableS1.png"),
              webshot = "webshot2")

# Save R object
saveRDS(gamm_table, file = file.path(path1, "05_tableS1.rds"))

# ===========================================================================
# ===========================================================================