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
 path <- file.path(getwd(), "outputs", "02_outputs_models")
 fit <- readRDS(file.path(path, "02B_DHMLM.rds"))
 
# =======================================================================
# =======================================================================
 
 
 
 
 
# =======================================================================
# 2. Prepare a matrix to plot
# =======================================================================
 
 
# Prepare the correlation matrix ----------------------------------------
 
 # Extract the full object
 ar <- VarCorr(fit)
 
 # Extract the covariances from the object
 cov <- ar$predator_id$cov
 
 # Extract the covariance matrix
 vcovmat <- cov[,1,]
 
 # Transform to correlation matrix
 cormat <- cov2cor(vcovmat)



# Arrange the correlation matrix ----------------------------------------
 
 # Change rownames
 rownames(cormat) <- c("mean speed", "IIV speed",
                       "mean speed", "IIV speed",
                       "mean speed", "IIV speed",
                       "mean prey speed", "IIV prey speed",
                       "mean prey speed", "IIV prey speed",
                       "mean prey speed", "IIV prey speed",
                       "mean success",
                       "mean success",
                       "mean success")
 
 # Change colnames
 colnames(cormat) <- c("mean speed", "IIV speed",
                       "mean speed", "IIV speed",
                       "mean speed", "IIV speed",
                       "mean prey speed", "IIV prey speed",
                       "mean prey speed", "IIV prey speed",
                       "mean prey speed", "IIV prey speed",
                       "mean success",
                       "mean success",
                       "mean success")

 # Reorder by XP level in order
 cormat_ord <- cormat[c(1, 2, 7, 8, 13,
                        3, 4, 9, 10, 14,
                        5, 6, 11, 12, 15),
                      c(1, 2, 7, 8, 13,
                        3, 4, 9, 10, 14,
                        5, 6, 11, 12, 15)] 

# =======================================================================
# =======================================================================

 


 
# =======================================================================
# 3. Plot the correlations
# =======================================================================


# Make the plot ---------------------------------------------------------

 # Define the color gradient
 COL2(diverging = c("RdBu", "BrBG", "PiYG",
                    "PRGn", "PuOr", "RdYlBu"),
      n = 200)

 # Define the colors for the XP levels
 cols <- c("#999999", "#E69F00", "#00AFBB")
 
 # Produce the plot
 cm <- corrplot(cormat_ord, method = "circle",
                tl.col = c(rep(cols[1], 5),
                           rep(cols[2], 5),
                           rep(cols[3], 5)),
                tl.cex = 0.8,
                tl.srt = 45,
                diag = T,
                type = "lower",
                col = COL2("RdBu", 10))



# Save the figure -----------------------------------------------------
 
 # File path
 path <- file.path(getwd(), "outputs", "05_outputs_figures")
 
 # Export to powerpoint
 graph2ppt(file = file.path(path, "05_figure3raw.pptx"), 
          width = 10, height = 6)

 # After exporting the plot to ppt, I modify it there and save
 # the image as a .png file in the same directory
 
# =======================================================================
# =======================================================================