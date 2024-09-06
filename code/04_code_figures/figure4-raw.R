# =======================================================================

#                    Plot the full correlation matrix

# =======================================================================





# =======================================================================
# 1. Import data and libraries
# =======================================================================


# Load libraries and model data -----------------------------------------

 # Libraries
 library(brms)
 library(corrplot)
 library(export)

 # Model
 path <- file.path(getwd(), "outputs", "01_outputs_models")
 fit <- readRDS(file.path(path, "B1_DHMLM-no-outlier.rds"))

# =======================================================================
# =======================================================================





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

# Add 1 to the diagonal
 diag(full_mat) <- 1

 # Start with novices
 full_mat[c(2, 3, 4, 5), 1] <- nov[c(1, 2, 4, 7), median_cor]
 full_mat[c(3, 4, 5), 2] <- nov[c(3, 5, 8), median_cor]
 full_mat[c(4, 5), 3] <- nov[c(6, 9), median_cor]
 full_mat[5, 4] <- nov[10, median_cor]

 # intermediate
 full_mat[c(7, 8, 9, 10), 6] <- int[c(1, 2, 4, 7), median_cor]
 full_mat[c(8, 9, 10), 7] <- int[c(3, 5, 8), median_cor]
 full_mat[c(9, 10), 8] <- int[c(6, 9), median_cor]
 full_mat[10, 9] <- int[10, median_cor]

 # advanced
 full_mat[c(12, 13, 14, 15), 11] <- adv[c(1, 2, 4, 7), median_cor]
 full_mat[c(13, 14, 15), 12] <- adv[c(3, 5, 8), median_cor]
 full_mat[c(14, 15), 13] <- adv[c(6, 9), median_cor]
 full_mat[15, 14] <- adv[10, median_cor]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Plot the correlations
# =======================================================================


# Make the plot and export ----------------------------------------------

 # Define the color gradient
 COL2(
   diverging = c(
      "RdBu", "BrBG", "PiYG",
      "PRGn", "PuOr", "RdYlBu"
   ),
   n = 200
 )

 # File path
 path <- file.path(getwd(), "outputs", "04_outputs_figures")

 # Produce the plots
 cm1 <- corrplot(
    full_mat[1:5,1:5], method = "circle",
    tl.col = "black",
    tl.cex = 1.5,
    tl.srt = 45,
    cl.cex = 1.5,
    number.cex = 1.5,
    diag = FALSE,
    type = "lower",
    addCoef.col = "black",
    col = COL2("RdBu", 10)
 )

  # Export to powerpoint
 graph2ppt(
    file = file.path(path, "figure4-raw.pptx"),
    width = 10, height = 6
 )

 cm2 <- corrplot(
    cormat_interm, method = "circle",
    tl.col = "black",
    tl.cex = 1.5,
    tl.srt = 45,
    cl.cex = 1.5,
    number.cex = 1.5,
    diag = FALSE,
    type = "lower",
    addCoef.col = "black",
    col = COL2("RdBu", 10)
 )

 # add 2nd slide
 graph2ppt(
    file = file.path(path, "figure4-raw.pptx"),
    width = 10, height = 6, append = TRUE
 )

 cm3 <- corrplot(
    cormat_adv, method = "circle",
    tl.col = "black",
    tl.cex = 1.5,
    tl.srt = 45,
    cl.cex = 1.5,
    number.cex = 1.5,
    diag = FALSE,
    type = "lower",
    addCoef.col = "black",
    col = COL2("RdBu", 10)
 )

 # add 3rd slide
 graph2ppt(
    file = file.path(path, "figure4-raw.pptx"),
    width = 10, height = 6, append = TRUE
 )

 # After exporting the plots to ppt, I modify them there and save
 # the image as a .png file in the same directory

# =======================================================================
# =======================================================================