# =========================================================================

#           Principal component analysis on predator behaviour            #

# =========================================================================

# Code to produce the PCA

# Activate project environment
renv::activate()

# -------------------------------------------------------------------------





# =========================================================================
# 1. Load libraries
# =========================================================================



library(data.table)
library(ggcorrplot)
library(factoextra)
library(FactoMineR)
library(cowplot)
library(ggpubr)

# =========================================================================
# =========================================================================





# =========================================================================
# 2. Load data
# =========================================================================

# Load original data ------------------------------------------------------

data <- fread("C:/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Data Behaviour/03_final-data/03_final-data_2021/final-data.csv",
              select = c("game_duration", "pred_speed", "pred_amount_tiles_visited",
                         "ambush_time_close", "latency_1st_capture",
                         "chase_count", "total_chase_duration"),
              stringsAsFactors = TRUE)

# Divide the data by match duration ----------------------------------------

data[, pred_amount_tiles_visited := pred_amount_tiles_visited/game_duration]
data[, chase_count := chase_count/game_duration]
data[, total_chase_duration := total_chase_duration/game_duration]
data[, latency_1st_capture := latency_1st_capture/game_duration]
data[, ambush_time_close := ambush_time_close/game_duration]



# Transform the data -------------------------------------------------------

data[ , ":=" (ambush_time_close = sqrt(ambush_time_close),
              total_chase_duration = sqrt(total_chase_duration),
              chase_count = log(chase_count + 1),
              pred_amount_tiles_visited = sqrt(pred_amount_tiles_visited),
              latency_1st_capture = log(latency_1st_capture + 1))]



# Standardize the variables -----------------------------------------------

# Create the function
standardize <- function (x) {(x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}

# Apply the function and create new columns
# The function standardizes the variables by group :
# in this case, by level of experience

data[, c("pred_speed", "pred_amount_tiles_visited", 
         "ambush_time_close", "latency_1st_capture",
         "chase_count", "total_chase_duration") :=
       lapply(.SD, standardize), 
       .SDcols = c("pred_speed", "pred_amount_tiles_visited", 
                   "ambush_time_close", "latency_1st_capture",
                   "chase_count", "total_chase_duration")]



# Change variable names ---------------------------------------------------

setnames(data, "pred_speed", "travel speed")
setnames(data, "pred_amount_tiles_visited", "rate of space covered")
setnames(data, "ambush_time_close", "ambush time")
setnames(data, "latency_1st_capture", "latency 1st capture")
setnames(data, "chase_count", "amount of chases")
setnames(data, "total_chase_duration", "time spent chasing")

data[, game_duration := NULL]

# =========================================================================
# =========================================================================





# =========================================================================
# 3. Visualize correlation matrix
# =========================================================================

# Correlation -------------------------------------------------------------

full_matrix_pearson <- cor(data)
full_matrix_pearson <- round(full_matrix_pearson, 2)



# Correlation plot --------------------------------------------------------

full_p_val_cor <- cor_pmat(data)

full_cor_plot <- ggcorrplot(full_matrix_pearson, hc.order = T, type = "lower", lab = TRUE, p.mat = cor_pmat(data), digits = 1)

full_cor_plot <- ggpubr::ggpar(full_cor_plot,
                          title = "\nCorrelation plot of behavior variables\n") +
  scale_fill_gradient2(name = "Pearson\ncorrelation\n",
                       low = "blue", mid = "white", high = "red",
                       midpoint = 0, limit = c(-1, 1))

# =========================================================================
# =========================================================================





# =========================================================================
# 4. Compute PCA
# =========================================================================

# FactoMineR method : singular value decomposition ------------------------

pca <- PCA(data, graph = FALSE, scale.unit = FALSE)

# spectral decomposition (gives the same results)
#PCA_fullZ1 <- princomp(full_Zmatrix, cor = FALSE, scores = TRUE)

# Extract variable results
variables <- get_pca_var(pca)

# =========================================================================
# =========================================================================





# =========================================================================
# 5. Kaiser-Guttman criterion + scree plot
# =========================================================================

get_eigenvalue(pca)



# Scree plot --------------------------------------------------------------

scree_plot <- fviz_eig(pca, addlabels = TRUE) +
  theme(axis.line = element_line(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain")) +
  ylab("Percentage of explained variances\n") +
  xlab("\nPC axes")

# =========================================================================
# =========================================================================





# =========================================================================
# 6. Contribution of variables to selected PCA axes
# =========================================================================


# Contribution of variables to PC1 ----------------------------------------

contrib_PC1 <- fviz_contrib(pca, choice = "var", axes = 1) +
              theme(axis.line = element_line(),
                     axis.title = element_text(size = 14, face = "bold"),
                     axis.text.x = element_text(face = "bold", color = "black",
                                                size = 14, angle = 45)) +
                ylab("Contribution (%)\n")



# Contribution of variables to PC2 ---------------------------------------

contrib_PC2 <- fviz_contrib(pca, choice = "var", axes = 2) +
               theme(axis.line = element_line(),
                     axis.title = element_text(size = 14, face = "bold"),
                     axis.text.x = element_text(face = "bold", color = "black",
                                                size = 14, angle = 45)) +
               ylab("Contribution (%)\n")

# =========================================================================
# =========================================================================





# =========================================================================
# 7. Generate PCA biplots for each matrix
# =========================================================================
# col.var = "contrib",
# gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # (if I used cos2)
# gradient.cols = c("pink", "purple1", "purple4", "orchid4"), # (if I used cos2)
# col.ind = grp, # (cos2) to have a contribution gradient
# palette = c("blueviolet", "orchid", "slateblue2", "violetred"),

# PCA Biplot (individuals + variables)
biplot12 <- fviz_pca_biplot(pca,
                            col.var = "black",
                            col.ind = "dimgray", #571A44 #96579e
                            alpha.ind = 0.2,
                            arrowsize = 1,
                            geom.ind = "point",
                            pointshape = 16,
                            pointsize = 1,
                            labelsize = 6,
                            select.var = list(contrib = 13),
                            repel = TRUE) + # no text overlap
  #scale_y_continuous(breaks = seq(-7.5, 7.5, 2.5), limits = c(-8.5, 8.5)) +
  #scale_x_continuous(breaks = seq(-7.5, 7.5, 2.5), limits = c(-9, 8.5)) +
  #scale_y_continuous(breaks = seq(-10, 10, 5), limits = c(-10, 10)) +
  #scale_x_continuous(breaks = seq(-10, 10, 5), limits = c(-10, 10)) +
  background_grid(major = "none") +
  theme(panel.border = element_rect(fill = NA, size = 0.95),
        axis.text.x = element_text(face = "plain", size = 14, color = "black"),
        axis.text.y = element_text(face = "plain", size = 14, color = "black"),
        axis.ticks.length = unit(.15, "cm"),
        axis.ticks = element_line(size = 0.90),
        plot.margin = margin(0.1, 0.5, 0.2, 0.3, "cm"))

biplot12 <- ggpubr::ggpar(biplot12,
                          xlab = "\nPC1 (36.9.0% of explained variance)",
                          ylab = "PC2 (27.3% of explained variance)\n",
                          title = "",
                          font.x = c(15, "plain"),
                          font.y = c(15, "plain"))

# ------------------------------------------------

biplot23 <- fviz_pca_biplot(pca,
                            col.var = "black",
                            axes = c(2,3),
                            col.ind = "dimgray", # firebrick3
                            alpha.ind = 0.2,
                            arrowsize = 1,
                            geom.ind = "point",
                            pointshape = 16,
                            pointsize = 1,
                            labelsize = 6,
                            select.var = list(contrib = 13),
                            repel = TRUE) + # no text overlap
  #scale_y_continuous(breaks = seq(-5, 5, 2.5), limits = c(-6, 5)) +
  #scale_x_continuous(breaks = seq(-7.5, 5, 2.5), limits = c(-8.5, 7)) +
  #scale_y_continuous(breaks = seq(-10, 10, 5), limits = c(-10, 10)) +
  #scale_x_continuous(breaks = seq(-10, 10, 5), limits = c(-10, 10)) +
  background_grid(major = "none") +
  theme(panel.border = element_rect(fill = NA, size = 0.95),
        axis.text.x = element_text(face = "plain", size = 14, color = "black"),
        axis.text.y = element_text(face = "plain", size = 14, color = "black"),
        axis.ticks.length = unit(.15, "cm"),
        axis.ticks = element_line(size = 0.90),
        plot.margin = margin(0.1, 0.5, 0.2, 0.5, "cm"))

biplot23 <- ggpubr::ggpar(biplot23,
                          xlab = "\nPC2 (27.3% of explained variance)",
                          ylab = "PC3 (17.7% of explained variance)\n",
                          title = "",
                          font.x = c(15, "plain"),
                          font.y = c(15, "plain"))

# =========================================================================
# =========================================================================





# =========================================================================
# 8. Calculate variables having the highest loadings
# =========================================================================

contrib_table <- data.table(format(round(variables$contrib,
                                         digits = 2),
                                   nsmall = 2), 
                            keep.rownames = TRUE)

corr_table <- data.table(format(round(variables$cor,
                                      digits = 2), 
                                nsmall = 2),
                         keep.rownames = TRUE)

# =========================================================================
# =========================================================================





# =========================================================================
# 9. Save plots in a PDF file or other png file****
# =========================================================================

# Ran this code once

#ggexport(plotlist = list(scree_plot,
#                         contrib_PC1, contrib_PC2,
#                         contrib_PC3),
#         nrow = 1, ncol = 1,
#         filename = "./outputs/02_PCA_diagnostics-plots.pdf") # as PDF file

# ggexport(PCA_fullZ_biplot12, filename = "./outputs/02_figureS1.png",
#          width = 2000, height = 1800, res = 300)


# as one figure with the two plots
PCA_figure <- ggarrange(biplot12,
                        biplot23,
                        ncol = 2, 
                        nrow = 1)

ggexport(PCA_figure, filename = "./outputs/01_PCA-biplot.tiff", 
         width = 4000, height = 1800, res = 300)


# End of script ==========================================================