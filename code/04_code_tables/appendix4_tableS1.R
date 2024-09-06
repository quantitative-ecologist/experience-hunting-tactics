# ===========================================================================

#                        Code to produce Table S1                           #

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
   file.path(path, "01_outputs_models", "B1_DHMLM-no-outlier.rds")
 )
# ===========================================================================
# ===========================================================================





# ===========================================================================
# 2. Create correlation matrix
# ===========================================================================


# Extract the posterior correlations ----------------------------------------

 # Taken from fit
 corrs <- data.table(
   as_draws_df(fit, variable = "^cor_", regex = TRUE)
 )
 # Remove MCMC columns
 corrs[, c(".chain", ".iteration", ".draw") := NULL]
 # Table to long format
 corrs1 <- melt(corrs)



# Summarize posterior correlations --------------------------------------

 # HPD intervals functions
 lower_95 <- function(x) {
   coda::HPDinterval(as.mcmc(x), 0.95)[1]
 }
 upper_95 <- function(x) {
   coda::HPDinterval(as.mcmc(x), 0.95)[2]
 }

 # Summary of posterior
 corrs1[
   , ":=" (
     median_cor = median(value),
     lower_95 = lower_95(value),
     upper_95 = upper_95(value)
   ),
   by = variable
 ]

 # Round the values to 2 digits
 corrs1[
   , c(3:5) := lapply(.SD, function(x) round(x, digits = 2)),
   .SDcols = c(3:5)
 ]

 # Keep unique values
 cor_tab <- unique(corrs1[, c(1, 3:5)])

 # Paste estimate with lower and upper ci into a single column
 cor_tab[, estim := paste(format(median_cor, digits = 2), "(")]
 cor_tab[, estim := paste(estim, format(lower_95, digits = 2), sep = "")]
 cor_tab[, estim := paste(estim, ",", sep = "")]
 cor_tab[, estim := paste(estim, format(upper_95, digits = 2), sep = "")]
 cor_tab[, estim := paste(estim, ")", sep = "")]



# Compute the matrix --------------------------------------------------------

 # Empty 15x15 matrix
 full_mat <- matrix(nrow = 15, ncol = 15)

 # Row names
 rownames(full_mat) <- c(
   "mean speed", "IIV speed",
   "mean prey speed", "IIV prey speed",
   "mean success",
   "mean speed", "IIV speed",
   "mean prey speed", "IIV prey speed",
   "mean success",
   "mean speed", "IIV speed",
   "mean prey speed", "IIV prey speed",
   "mean success"
 )

 # Column names
 colnames(full_mat) <- c(
   "mean speed", "IIV speed",
   "mean prey speed", "IIV prey speed",
   "mean success",
   "mean speed", "IIV speed",
   "mean prey speed", "IIV prey speed",
   "mean success",
   "mean speed", "IIV speed",
   "mean prey speed", "IIV prey speed",
   "mean success"
 )

# ===========================================================================
# ===========================================================================





# ===========================================================================
# 3. Modify the correlation matrix
# ===========================================================================


# Decompose matrix sections -------------------------------------------------

# Correlations within xp levels
 # novice correlations
 nov <- cor_tab[variable %like% "novice"][
   !(variable %like% "interm|advanced")
 ]
 # intermediate correlations
 int <- cor_tab[variable %like% "interm"][
   !(variable %like% "novice|advanced")
 ]
 # advanced correlations
 adv <- cor_tab[variable %like% "advanced"][
   !(variable %like% "novice|interm")
 ]

# Correlations across xp levels
 # novice x intermediate
 nov2 <- cor_tab[variable %like% "novice"][
   !(variable %like% "advanced")][
     variable %like% "interm"
   ]
 # novice x advanced
 nov3 <- cor_tab[variable %like% "novice"][
   !(variable %like% "interm")][
     variable %like% "advanced"
   ]
 # intermediate x advanced
 int2 <- cor_tab[variable %like% "interm"][
   !(variable %like% "novice")][
     variable %like% "advanced"
   ]

# Add values to matrix section-wise -----------------------------------------

 # Add 1 to the diagonal
 diag(full_mat) <- 1

 # Start with novices
 full_mat[c(2, 3, 4, 5), 1] <- nov[c(1, 2, 4, 7), estim]
 full_mat[c(3, 4, 5), 2] <- nov[c(3, 5, 8), estim]
 full_mat[c(4, 5), 3] <- nov[c(6, 9), estim]
 full_mat[5, 4] <- nov[10, estim]

 # intermediate
 full_mat[c(7, 8, 9, 10), 6] <- int[c(1, 2, 4, 7), estim]
 full_mat[c(8, 9, 10), 7] <- int[c(3, 5, 8), estim]
 full_mat[c(9, 10), 8] <- int[c(6, 9), estim]
 full_mat[10, 9] <- int[10, estim]

 # advanced
 full_mat[c(12, 13, 14, 15), 11] <- adv[c(1, 2, 4, 7), estim]
 full_mat[c(13, 14, 15), 12] <- adv[c(3, 5, 8), estim]
 full_mat[c(14, 15), 13] <- adv[c(6, 9), estim]
 full_mat[15, 14] <- adv[10, estim]

 # novice x intermediate
 full_mat[c(6, 7, 8, 9, 10), 1] <- nov2[c(1, 3, 9, 13, 21), estim]
 full_mat[c(6, 7, 8, 9, 10), 2] <- nov2[c(2, 4, 10, 14, 22), estim]
 full_mat[c(6, 7, 8, 9, 10), 3] <- nov2[c(5, 6, 11, 15, 23), estim]
 full_mat[c(6, 7, 8, 9, 10), 4] <- nov2[c(7, 8, 12, 16, 24), estim]
 full_mat[c(6, 7, 8, 9, 10), 5] <- nov2[c(17, 18, 19, 20, 25), estim]

 # novice x advanced
 full_mat[c(11, 12, 13, 14, 15), 1] <- nov3[c(1, 3, 9, 13, 21), estim]
 full_mat[c(11, 12, 13, 14, 15), 2] <- nov3[c(2, 4, 10, 14, 22), estim]
 full_mat[c(11, 12, 13, 14, 15), 3] <- nov3[c(5, 6, 11, 15, 23), estim]
 full_mat[c(11, 12, 13, 14, 15), 4] <- nov3[c(7, 8, 12, 16, 24), estim]
 full_mat[c(11, 12, 13, 14, 15), 5] <- nov3[c(17, 18, 19, 20, 25), estim]

 # intermediate x advanced
 full_mat[c(11, 12, 13, 14, 15), 6] <- int2[c(1, 3, 9, 13, 21), estim]
 full_mat[c(11, 12, 13, 14, 15), 7] <- int2[c(2, 4, 10, 14, 22), estim]
 full_mat[c(11, 12, 13, 14, 15), 8] <- int2[c(5, 6, 11, 15, 23), estim]
 full_mat[c(11, 12, 13, 14, 15), 9] <- int2[c(7, 8, 12, 16, 24), estim]
 full_mat[c(11, 12, 13, 14, 15), 10] <- int2[c(17, 18, 19, 20, 25), estim]

# ===========================================================================
# ===========================================================================





# ===========================================================================
# 4. Create the table using flextable
# ===========================================================================


 # Prepare the table parameters ----------------------------------------------

 # Matrix as a dataframe
 table <- data.frame(full_mat)
 table$row <- rownames(table)
 table <- table[, c(16, 1:15)]

 # Compute custom column names
 column_names <- c(
   NA,
   rep(c(
     "mean speed", "IIV speed",
     "mean prey speed", "IIV prey speed",
     "mean success"
   ),
   3
   )
 )

 # Custom header
 my_header <- data.frame(
   col_keys = colnames(table),
   label = column_names
 )

 # Custom theme
 my_theme <- function(x, ...) {
   x <- colformat_double(x, big.mark = " ", 
                         decimal.mark = ".",
                         digits = 3)
   x <- set_table_properties(x, layout = "fixed")
   x <- border_remove(x)
   autofit(x)
 }



 # Create the table ----------------------------------------------------------

 cor_table <-
    # Select table content
    table %>%
    select(my_header$col_keys) %>%
    flextable() %>%

    # modify column names
    set_header_df(mapping = my_header, key = "col_keys") %>%
    # modify row names on column 1
    mk_par(j = 1, value = as_paragraph(column_names[-c(1)])) %>%
    # Add header rows (for XP levels)
    add_header_row(
       values = c(NA, "Novice", "Intermediate", "Advanced"),
       colwidths = c(1, 5, 5, 5)
    ) %>%

    # Add theme
    my_theme() %>%
    # Add borders
    hline(
       i = 1, part = "header",
       border = fp_border(width = 1, color = "black")
    ) %>%
    hline_top(
       part = "header",
       border = fp_border(width = 1, color = "black")
    ) %>%
    hline_bottom(
       part = "body", 
       border = fp_border(width = 1, color = "black")
    ) %>%

    # Customizing the table layout
    fontsize(size = 8, part = "all") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    align(align = "left", part = "all", j = 1) %>%
    align(align = "center", part = "all", j = c(2:16)) %>%

    # Color for novices
    bg(i = c(1:5), j = c(2:6), bg = "#999999", part = "body") %>%
    color(j = c(1:6), color = "#999999", part = "header") %>%
    color(i = c(1:5), j = c(1), color = "#999999", part = "body") %>%
    # Color for intermediates
    bg(i = c(6:10), j = c(7:11), bg = "#E69F00", part = "body") %>%
    color(j = c(7:11), color = "#E69F00", part = "header") %>%
    color(i = c(6:10), j = c(1), color = "#E69F00", part = "body") %>%
    # Color for advanced
    bg(i = c(11:15), j = c(12:16), bg = "#00AFBB", part = "body") %>%
    color(j = c(12:16), color = "#00AFBB", part = "header") %>%
    color(i = c(11:15), j = c(1), color = "#00AFBB", part = "body")

 # Image was saved from the Rstudio export option
 # width = 1630  height = 570 on large computer screen from the viewer in Rstudio


# ===========================================================================
# ===========================================================================