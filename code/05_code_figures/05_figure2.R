# =======================================================================

#                   Plot the coefficients of variation

# =======================================================================





# =======================================================================
# 1. Import the libraries and data
# =======================================================================

 # Libraries
 library(data.table)
 library(ggplot2)
 library(ggpubr)

 # CV table
 path <- "./outputs/04_outputs_model-processing"
 tab <- readRDS(file.path(path, "04_CV-table.rds"))
 
 # Rename the XP variable
 tab[xp_level == "novice", xp_level := "Below 150"]
 tab[xp_level == "interm", xp_level := "Between 150 and 300"]
 tab[xp_level == "advanced", xp_level := "Above 300"]
 
 # Rename the behaviour names
 tab[variable == "Zspeed", variable := "Predator speed"]
 tab[variable == "Zpreyspeed", variable := "Prey speed"]

 # Reorder factors
 tab[, xp_level := factor(xp_level, levels = c("Below 150",
                                               "Between 150 and 300",
                                               "Above 300"))]
 
 # Encode Parameter and variable as factor
 tab[, ":=" (Parameter = as.factor(Parameter),
             variable = as.factor(variable))]
 
# =======================================================================
# =======================================================================





# =======================================================================
# 2. Prepare the figure options
# =======================================================================


# Set custom theme -----------------------------------------------------

 custom_theme <- theme(# axis values size
                       axis.text.x = element_text(face = "plain", 
                                                  size = 15,
                                                  color = "black"),
                       axis.text.y = element_text(face = "plain", 
                                                  size = 15,
                                                  color = "black"),
                       # axis ticks lenght
                       axis.ticks.length = unit(.15, "cm"),
                       # axis ticks width
                       axis.ticks = element_line(size = 0.90, 
                                                 color = "black"),
                       # axis titles size
                       axis.title = element_text(size = 17, 
                                                 face = "plain",
                                                 color = "black"),
                       axis.line = element_line(size = 0.95,
                                                color = "black"),
                       panel.grid = element_blank(),
                       panel.background = element_blank())

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Make the plot
# =======================================================================


# Plot mu --------------------------------------------------------------
 
 cv_plot1 <- ggplot(tab[Parameter == "mu"],
                    aes(x = variable, y = mean,
                        color = xp_level,
                        shape = xp_level)) +
 
     geom_pointrange(aes(ymin = lower_ci,
                         ymax = upper_ci),
                     size = 0.8,
                     position = position_dodge(width = 0.3)) +
 
     scale_shape_manual(name = "Total experience :",
                         values = c(15, 16, 17)) +
     scale_color_manual(name = "Total experience :",
                        values = c("#999999", "#E69F00", "#00AFBB")) +
     
     scale_y_continuous(breaks = seq(0.1, 0.4, 0.1),
                        limits = c(0.02, 0.48)) +

     ylab("CV (mean)\n") +
     xlab("\nBehavior") +
     
     custom_theme +
      theme(axis.title.x = element_blank(),
            legend.position = "top",
            legend.key = element_rect(fill = "transparent"),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 14))



# Plot sigma ------------------------------------------------------------
 
 cv_plot2 <- ggplot(tab[Parameter == "sigma"],
                    aes(x = variable, y = mean,
                        color = xp_level,
                        shape = xp_level)) +
 
     geom_pointrange(aes(ymin = lower_ci,
                         ymax = upper_ci),
                     size = 0.8,
                     position = position_dodge(width = 0.3)) +
 
     scale_shape_manual(name = "Total experience :",
                         values = c(15, 16, 17)) +
     scale_color_manual(name = "Total experience :",
                        values = c("#999999", "#E69F00", "#00AFBB")) +
     scale_y_continuous(breaks = seq(0.1, 0.4, 0.1),
                        limits = c(0.02, 0.48)) +

     ylab("CV (sigma)\n") +
     xlab("\nBehavior") +
     
     custom_theme +
     theme(axis.title.x = element_blank(),
           legend.position = "top",
           legend.key = element_rect(fill = "transparent"),
           legend.title = element_text(size = 15),
           legend.text = element_text(size = 14))



# Combine as one figure ------------------------------------------------

 # Folder path
 path <- "./outputs/05_outputs_figures"
 
 # Arrange paneled figure
 figure <- ggarrange(cv_plot1, cv_plot2,
                     ncol = 2, nrow = 1,
                     labels = c("(A)", "(B)"),
                     common.legend = TRUE,
                     legend = "top")


 # Save figure
 ggexport(figure,
          filename = file.path(path, "05_figure2.png"),
          width = 3500,
          height = 1600,
          res = 300)

# =======================================================================
# =======================================================================










