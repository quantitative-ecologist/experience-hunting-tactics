# =======================================================================

#          Plot distribution of players from novice to advanced

# =======================================================================





# =======================================================================
# 1. Prepare the script
# =======================================================================


# Load libraries and model ----------------------------------------------

 # Libraries
 library(brms)
 library(data.table)
 library(ggplot2)
 library(ggpubr)
 library(ggridges)
 
 # import model
 path <- file.path(getwd(), "outputs", "02_outputs_models")
 fit <- readRDS(file.path(path, "02B_DHMLM.rds"))
 


# Load data ------------------------------------------------------------
 
 # Data
 data <- fread("./data/FraserFrancoetalXXXX-data.csv",
               select = c("predator_id",
                          "cumul_xp_pred",
                          "pred_speed"))
 
 # Extract standard deviation of speed
 sd_speed1 <- sd(data[cumul_xp_pred < 100]$pred_speed)
 sd_speed2 <- sd(data[cumul_xp_pred >= 300]$pred_speed)

 # Filter only for advanced players
 data <- data[cumul_xp_pred >= 300]
 
 # Predator id as factor
 data[, predator_id := as.factor(predator_id)]
 
# =======================================================================
# =======================================================================





# =======================================================================
# 2. Extract posterior draws for predator speed
# =======================================================================


# Extract sigma values --------------------------------------------------

 draws <- data.table(
     as_draws_df(
         fit,
         variable = c("r_predator_id__sigma_speed"),
         regex = TRUE)
 )

 draws[, c(1:33, 287:540) := NULL]
 draws[, c(506:508) := NULL]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Reshape the draws table
# =======================================================================

# Reshape sigma speed ---------------------------------------------------

 # Long format
 draws <- melt(draws,
               measure = patterns("^r_predator_id"),
               variable.name = "predator_id")
 
 # Add experience level
 draws[, xp_level := ifelse(predator_id %like% "novice",
                            "novice",
                            "advanced")]
 
 # Add predator ID
 draws[, predator_id := as.character(predator_id)]
 draws[, predator_id := gsub("[]_,a-zA-Z,[]", "", predator_id)]
 draws[, predator_id := gsub(" ", "", paste("pred", predator_id))]
 draws[, predator_id := as.factor(predator_id)]
 
 # Rename value to speed
 setnames(draws, "value", "speed_sigma")



# Transform  ------------------------------------------------------------

# Add population intercept
int1 <- fixef(fit, pars = "sigma_speednovice_Intercept")[1]
int2 <- fixef(fit, pars = "sigma_speedadvanced_Intercept")[1]

draws[xp_level == "novice", speed_sigma := speed_sigma + int1]
draws[xp_level == "advanced", speed_sigma := speed_sigma + int2]

# Back transform to original scale (sigma is on log scale)
draws[, exp_speed_sigma := exp(speed_sigma)]


# Calculate mean predicted value for each individual
draws <- draws[, average_speed_sigma := mean(exp_speed_sigma),
                 by = c("predator_id",
                        "xp_level")]

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Compute the two plots
# =======================================================================

# Set custom theme ------------------------------------------------------

 custom_theme <- theme(# axis values size
                        axis.text.x = element_text(face = "plain", 
                                                   size = 12,
                                                   color = "black"),
                        axis.text.y = element_text(face = "plain", 
                                                   size = 5,
                                                   color = "black"),
                        # axis ticks lenght
                        axis.ticks.length = unit(.15, "cm"),
                        # axis ticks width
                        axis.ticks = element_line(size = 0.90, 
                                                  color = "black"),
                        # axis titles size
                        axis.title = element_text(size = 14, 
                                                  face = "plain",
                                                  color = "black"),
                        axis.line = element_line(size = 0.95,
                                                 color = "black"),
                        panel.grid = element_blank(),
                        panel.background = element_blank())
 
 # 1 digits to axis
 scaleFUN <- function(x) sprintf("%.1f", x)



# Plot novices ----------------------------------------------------------

 #scaled_breaks1 <- c(0.2 / sd_speed1,
 #                    0.6 / sd_speed1,
 #                    1.0 / sd_speed1,
 #                    1.4 / sd_speed1)
 
 # Extract 95% CI of intercepts
 int1min <- exp(fixef(fit, pars = "sigma_speednovice_Intercept")[3])
 int1max <- exp(fixef(fit, pars = "sigma_speednovice_Intercept")[4])

 # Plot the distributions of novice players
 plot1 <- ggplot() +
     
     annotate("rect",
              fill = "firebrick2",
              xmin = int2min,
              xmax = int2max,
              ymin = -Inf,
              ymax = Inf, 
              alpha = .5) +
 
     geom_density_ridges(data = draws[xp_level == "novice"],
                         rel_min_height = 0.005,
                         fill = "#999999",
                         aes(x = exp_speed_sigma,
                             y = predator_id,
                             height = ..density..,
                             scale = 3)) +
     
     geom_point(data = unique(draws[xp_level == "novice", c(1, 5)]),
                aes(x = average_speed_sigma, 
                    y = predator_id),
                size = 1,
                color = "black") +
     scale_x_continuous(breaks = seq(0, 2, 0.5),
                        limits = c(0, 2.5)) +
     #scale_x_continuous(breaks = scaled_breaks1,
     #                   labels = scaleFUN,
     #                   limits = c(0, 4)) +
     
     ylab("Predator ID\n") +
     xlab("\nIntra individual standard deviation (m/s)") +
     labs(title = "Novice \nIntercept = 0.273 (0.260, 0.288)") +
     
     custom_theme +
     theme(axis.text.y = element_blank(),
           axis.ticks.y = element_blank(),
           plot.title = element_text(size = 15,
                                     face = "bold"))



# Plot experts ----------------------------------------------------------

 #scaled_breaks2 <- c(0.2 / sd_speed2,
 #                    0.6 / sd_speed2,
 #                    1.0 / sd_speed2,
 #                    1.4 / sd_speed2)


 int2min <- exp(fixef(fit, pars = "sigma_speedadvanced_Intercept")[3])
 int2max <- exp(fixef(fit, pars = "sigma_speedadvanced_Intercept")[4])

 # Plot the distributions of advanced players
 plot2 <- ggplot() +
 
     annotate("rect",
              fill = "firebrick2",
              xmin = int1min,
              xmax = int1max,
              ymin = -Inf,
              ymax = Inf, 
              alpha = .5) +
     
     geom_density_ridges(data = table[xp_level == "advanced"],
                         rel_min_height = 0.005,
                         fill = "#00AFBB",
                         aes(x = exp_speed_sigma,
                             y = predator_id,
                             height = ..density..,
                             scale = 3)) +
     
     geom_point(data = unique(table[xp_level == "advanced", c(1, 5)]),
                aes(x = average_speed_sigma, 
                    y = predator_id),
                size = 1,
                color = "black") +
     
     scale_x_continuous(breaks = seq(0, 2, 0.5),
                        limits = c(0, 2.5)) +
     #scale_x_continuous(breaks = scaled_breaks2,
     #                   labels = c(0.5, 1.5, 2.5, 3.5),
     #                   limits = c(0, 4)) +
 
     #scale_x_continuous(#breaks = c(-1, 0, 1),
     #                   limits = c(0.1, 3.5),
     #                   #expand = c(0, 0),
     #                   sec.axis = sec_axis(trans = ~.,
     #                                       breaks = scaled_breaks2,
     #                                       labels = c(0.2, 0.4, 0.6, 0.8, 1),
     #                                       name = "Within individual variance (m/s)\n")) +
 
     ylab("Predator ID\n") +
     xlab("\nIntra individual standard deviation (m/s)") +
     labs(title = "Advanced \nIntercept = 0.282 (0.267, 0.299)") +
 
     custom_theme +
     theme(axis.text.y = element_blank(),
           axis.ticks.y = element_blank(),
           plot.title = element_text(size = 15,
                                     face = "bold"))

# =======================================================================
# =======================================================================





# =======================================================================
# 5. Combine plots into 1 figure
# =======================================================================

# Combine as one figure -------------------------------------------------

 # Combine plots
 figure <- ggarrange(NULL, plot1, NULL, plot2,
                     labels = c("(A)", "", "(B)", ""),
                     widths = c(0.15, 1.5, 0.15, 1.5),
                     ncol = 4, nrow = 1)

 # Folder path
 path <- file.path(getwd(), "outputs", "05_outputs_figures")

 # Save figure
 ggexport(figure,
          filename = file.path(path, "05_figure3.png"),
          width = 3500,
          height = 1800,
          res = 300)

# =======================================================================
# =======================================================================