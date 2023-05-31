# =======================================================================

#             Plot the success of specialists vs generalists

# =======================================================================





# =======================================================================
# 1. Prepare the script
# =======================================================================

# load libraries
library(brms)
library(data.table)
library(ggplot2)
library(ggpubr)

# import model
fit <- readRDS(
    "./outputs/01_outputs_models/B1_DHMLM-no-outlier.rds"
)

# Import ID table
id_tab <- readRDS(
    "./outputs/03_outputs_model-processing/DHMLM_id-draws.rds"
)

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Custom theme for plots
# =======================================================================


custom_theme <- theme(
   # axis values size
   axis.text = element_text(face = "plain",
                            size = 14,
                            color = "black"),
   # axis ticks lenght
   axis.ticks.length = unit(.15, "cm"),
   # axis ticks width
   axis.ticks = element_line(linewidth = 0.90,
                             color = "black"),
   # axis titles size
   axis.title = element_text(size = 16,
                             face = "plain",
                             color = "black"),
   axis.line = element_line(linewidth = 0.95,
                            color = "black"),
   legend.position = "none",
   panel.grid = element_blank(),
   panel.background = element_blank()
 )

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Compute the plot
# =======================================================================


# Novices table ---------------------------------------------------------

id_tab1 <- id_tab[xp_level == "novice" &
                  variable == "pred_speed" &
                  sigma == 1][, ":=" (sigma = NULL,
                                      xp_level = NULL,
                                      variable = NULL)]
setnames(id_tab1,
         c("mean_estimated", "lower_ci_estimated", "upper_ci_estimated"),
         c("sigma_pred_speed", "pred_speed_lower_ci", "pred_speed_upper_ci"))

id_tab1 <- unique(id_tab1[, c(2:5)])


id_tab2 <- id_tab[xp_level == "novice" &
                  variable == "success"][, ":=" (sigma = NULL,
                                                 xp_level = NULL,
                                                 variable = NULL)]
setnames(id_tab2,
         c("mean_estimated", "lower_ci_estimated", "upper_ci_estimated"), 
         c("mean_success", "success_lower_ci", "success_upper_ci"))

id_tab2 <- unique(id_tab2[, c(2:5)])

table1 <- merge(id_tab1, id_tab2, by = "predator_id")



# advanced table  ---------------------------------------------------------

id_tab3 <- id_tab[xp_level == "advanced" &
                  variable == "pred_speed" &
                  sigma == 1][, ":=" (sigma = NULL,
                                      xp_level = NULL,
                                      variable = NULL)]
setnames(id_tab3,
         c("mean_estimated", "lower_ci_estimated", "upper_ci_estimated"),
         c("sigma_pred_speed", "pred_speed_lower_ci", "pred_speed_upper_ci"))

id_tab3 <- unique(id_tab3[, c(2:5)])


id_tab4 <- id_tab[xp_level == "advanced" &
                  variable == "success"][, ":=" (sigma = NULL,
                                                 xp_level = NULL,
                                                 variable = NULL)]
setnames(id_tab4,
         c("mean_estimated", "lower_ci_estimated", "upper_ci_estimated"),
         c("mean_success", "success_lower_ci", "success_upper_ci"))

id_tab4 <- unique(id_tab4[, c(2:5)])

table2 <- merge(id_tab3, id_tab4, by = "predator_id")



# Plot for novices ------------------------------------------------------

plot1 <- ggplot() +
  geom_segment(data = table1,
               aes(x = pred_speed_lower_ci,
                   xend = pred_speed_upper_ci,
                   y = mean_success,
                   yend = mean_success),
               color = "#999999", alpha = 0.2) +
  geom_segment(data = table1,
               aes(x = sigma_pred_speed,
                   xend = sigma_pred_speed,
                   y = success_lower_ci,
                   yend = success_upper_ci),
               color = "#999999", alpha = 0.2) +
  geom_point(data = table1,
             aes(x = sigma_pred_speed,
                 y = mean_success,
                 fill = "#999999"),
             size = 2,
             color = "#999999",
             alpha = 0.8,
             stroke = 0) +
   scale_y_continuous(
    breaks = seq(-5, 5, 2.5),
    limits = c(-5.1, 5)
   ) +
   scale_x_continuous(
    breaks = seq(-1, 2, 1),
    limits = c(-1.2, 2.3)
   ) +
   ylab("Hunting success\n") +
   xlab("\nIIV in predator speed") +
   labs(title = "Novice: r = -0.18 (-0.29, -0.06)") +
   custom_theme



# Plot for advanceds ------------------------------------------------------

plot2 <- ggplot() +
  geom_segment(data = table2,
               aes(x = pred_speed_lower_ci,
                   xend = pred_speed_upper_ci,
                   y = mean_success,
                   yend = mean_success),
               color = "#00AFBB", alpha = 0.2) +
  geom_segment(data = table2,
               aes(x = sigma_pred_speed,
                   xend = sigma_pred_speed,
                   y = success_lower_ci,
                   yend = success_upper_ci),
               color = "#00AFBB", alpha = 0.2) +
  geom_point(data = table2,
             aes(x = sigma_pred_speed,
                 y = mean_success,
                 fill = "#00AFBB"),
             size = 2,
             color = "#00AFBB",
             alpha = 0.8,
             stroke = 0) +
   scale_y_continuous(
    breaks = seq(-5, 5, 2.5),
    limits = c(-5.1, 5)
   ) +
   scale_x_continuous(
    breaks = seq(-1, 2, 1),
    limits = c(-1.2, 2.3)
   ) +
   ylab("Hunting success\n") +
   xlab("\nIIV in predator speed") +
   labs(title = "Advanced: r = -0.24 (-0.34, -0.13)") +
   custom_theme

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Combine the plots as one figure
# =======================================================================

# Combine as one figure -------------------------------------------------

# Combine plots
figure <- ggarrange(plot1, plot2,
                    labels = c("(A)", "(B)"),
                    ncol = 2, nrow = 1)

# Folder path
path <- file.path(getwd(), "outputs", "04_outputs_figures")

# Save figure
ggexport(
  figure,
  filename = file.path(path, "appendix1_figureS2.png"),
  width = 2500,
  height = 1200,
  res = 300
)

# =======================================================================
# =======================================================================