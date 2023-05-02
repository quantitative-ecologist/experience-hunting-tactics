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





# =======================================================================
# 2. Prepare 3 matrices to plot
# =======================================================================


# Prepare the full correlation matrix -----------------------------------

 # Extract the full object
 ar <- VarCorr(fit)

 # Extract the covariances from the object
 cov <- ar$predator_id$cov

 # Extract the covariance matrix
 vcovmat <- cov[, 1, ]

 # Transform to correlation matrix
 cormat <- cov2cor(vcovmat)

 # Reorder by XP level in order
 cormat_ord <- cormat[c(1, 2, 7, 8, 13,
                        3, 4, 9, 10, 14,
                        5, 6, 11, 12, 15),
                      c(1, 2, 7, 8, 13,
                        3, 4, 9, 10, 14,
                        5, 6, 11, 12, 15)]

 # Separate each matrix
 cormat_novice <- cormat_ord[c(1:5), c(1:5)]
 cormat_interm <- cormat_ord[c(6:10), c(6:10)]
 cormat_adv <- cormat_ord[c(11:15), c(11:15)]



# Arrange the correlation matrices --------------------------------------

 # Names
 names <- c(
    "mean speed", "IIV speed",
    "mean prey speed", "IIV prey speed",
    "mean success"
 )

 # Change rownames
 rownames(cormat_novice) <- names
 rownames(cormat_interm) <- names
 rownames(cormat_adv) <- names


 # Change colnames
 colnames(cormat_novice) <- names
 colnames(cormat_interm) <- names
 colnames(cormat_adv) <- names

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
    cormat_novice, method = "circle",
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