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
   file.path(path, "02_outputs_models", "02B_DHMLM.rds")
 )
# ===========================================================================
# ===========================================================================





# ===========================================================================
# 2. Correlation matrix
# ===========================================================================


# Prepare the correlation matrix --------------------------------------------

 # Extract the full object
 ar <- VarCorr(fit)

 # Extract the covariances from the object
 cov <- ar$predator_id$cov

 # Extract the covariance matrix
 vcov_mat <- cov[, 1, ] # covariance
 upper_vcov_mat <- cov[, 4, ] # upper ci
 lower_vcov_mat <- cov[, 3, ] # lower ci

 # Transform to correlation matrix
 cormat <- cov2cor(vcov_mat)
 upper_cormat <- cov2cor(upper_vcov_mat)
 lower_cormat <- cov2cor(lower_vcov_mat)



# Arrange the correlation matrix ----------------------------------------

 # Reorder by XP level in order
 cormat_ord <- cormat[
   c(1, 2, 7, 8, 13,
     3, 4, 9, 10, 14,
     5, 6, 11, 12, 15),
   c(1, 2, 7, 8, 13,
     3, 4, 9, 10, 14,
     5, 6, 11, 12, 15)
 ]

 upper_cormat_ord <- upper_cormat[
   c(1, 2, 7, 8, 13,
     3, 4, 9, 10, 14,
     5, 6, 11, 12, 15),
   c(1, 2, 7, 8, 13,
     3, 4, 9, 10, 14,
     5, 6, 11, 12, 15)
 ]

 lower_cormat_ord <- lower_cormat[
   c(1, 2, 7, 8, 13,
     3, 4, 9, 10, 14,
     5, 6, 11, 12, 15),
   c(1, 2, 7, 8, 13,
     3, 4, 9, 10, 14,
     5, 6, 11, 12, 15)
 ]

# ===========================================================================
# ===========================================================================





# ===========================================================================
# 3. Modify the correlation matrix
# ===========================================================================


 # Round values to 2 digits
 cormat_ord <- round(cormat_ord, digits = 2)
 upper_cormat_ord <- round(upper_cormat_ord, digits = 2)
 lower_cormat_ord <- round(lower_cormat_ord, digits = 2)


# Combine the matrices together ---------------------------------------------

 # Paste opening parentheses
 full_mat <- matrix(
   paste(
     format(cormat_ord, digits = 2),
     "(",
     sep = " "
   ),
   15, 15
 )

 # Paste lower CI
 full_mat <- matrix(
    paste(
      format(full_mat, digits = 2),
      format(lower_cormat_ord, digits = 2),
      sep = ""
    ),
    15, 15
  )

 # Paste upper CI
 full_mat <- matrix(
   paste(
     format(full_mat, digits = 2),
     format(upper_cormat_ord, digits = 2),
     sep = ","
   ),
   15, 15
 )

 # Paste closing parentheses
 full_mat <- matrix(
   paste(
     format(full_mat, digits = 2),
     ")",
     sep = ""
   ),
   15, 15
 )


 # Add NAs to values on the upper diagonal
 full_mat[upper.tri(full_mat)] <- NA

 # Add 1 to the diagonal
 diag(full_mat) <- 1



# Add names to the columns and rows -----------------------------------------

 # Change rownames
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

 # Change colnames
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

 # Save the table
 path1 <- file.path("./outputs/05_outputs_tables")

 # Save R object
 saveRDS(cor_table, file = file.path(path1, "05_tableS1.rds"))
 # Image was saved from the Rstudio export option
 # width = 1630  height = 570 on large computer screen from the viewer in Rstudio


# ===========================================================================
# ===========================================================================