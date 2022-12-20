# =======================================================================

#                Plot the correlations between variables

# =======================================================================





# =======================================================================
# 1. Import data and libraries
# =======================================================================


# Load libraries and model data -----------------------------------------

 # Libraries
 library(brms)
 library(data.table)
 library(ggplot2)
 library(ggpubr)
 
 # Model
 #fit <- readRDS("./outputs/02_outputs_models/02B_DHMLM.rds")
 fit <- readRDS("./tests/02B_DHMLM.rds")
 
 # ID table
 path <- "./outputs/04_outputs_model-processing"
 id_tab <- readRDS(file.path(path, "04_id-draws.rds"))
 


# Load raw data ---------------------------------------------------------

 # Data
 data <- fread("./data/FraserFrancoetalXXXX-data.csv",
               select = c("predator_id",
                          "pred_game_mode",
                          "pred_game_duration",
                          "pred_speed",
                          "prey_avg_speed",
                          "cumul_xp_killer",
                          "total_xp_killer",
                          "hunting_success"))
 
 data <- unique(data)
 data <- data[pred_game_mode == "Online"]

 # Calculate total xp
 data[total_xp_killer < 100,
      xp_level := "novice"]
 
 data[total_xp_killer %between% c(100, 299),
      xp_level := "interm"]
 
 data[total_xp_killer > 300,
      xp_level := "advanced"]

 # Encode variables as a factor
 data[, xp_level := as.factor(xp_level)]
 data[, predator_id := as.factor(predator_id)]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Custom theme for plots
# =======================================================================

 # Custom plot theme
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
# 4. Prepare synthetic tables for plots
# =======================================================================

 # Intervals
 lower_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[1]}
 upper_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[2]}



# For plot 1 ------------------------------------------------------------

 # Filter and create table
 speed1 <- id_tab[xp_level == "novice" &
                  variable %in% c("pred_speed", "success") & 
                  sigma == 0]
 

 # Copy value column to back transform
 speed1[, value_bt := value]
 
 mean1 <- mean(data[xp_level == "novice", pred_speed])
 sd1 <- sd(data[xp_level == "novice", pred_speed])
 
 speed1[variable == "pred_speed",
        value_bt := (value_bt * sd1) + mean1]

 speed1[variable == "success",
        value_bt := (value_bt + 4) / 2]


 # Synthetic table
 speed1 <- speed1[, .(mean = mean(value),
                      mean_bt = mean(value_bt),
                      lower_ci = lower_interval(value),
                      upper_ci = upper_interval(value),
                      lower_ci_bt = lower_interval(value_bt),
                      upper_ci_bt = upper_interval(value_bt)),
                    by = .(predator_id, variable)]



# For plot 2 ------------------------------------------------------------

 # Filter and create table
 speed2 <- id_tab[xp_level == "novice" &
                  variable == "pred_speed" & 
                  sigma == 1]
 bind <-  id_tab[xp_level == "novice" &
                 variable == "success"]
 speed2 <- rbind(speed2, bind)


 # Copy value column to back transform
 speed2[, value_bt := value]
 
 speed2[variable == "pred_speed",
        value_bt := exp(value)]

 speed2[variable == "success",
        value_bt := (value_bt + 4) / 2]


 # Synthetic table
 speed2 <- speed2[, .(mean = mean(value),
                      mean_bt = mean(value_bt),
                      lower_ci = lower_interval(value),
                      upper_ci = upper_interval(value),
                      lower_ci_bt = lower_interval(value_bt),
                      upper_ci_bt = upper_interval(value_bt)),
                    by = .(predator_id, variable)]



# For plot 3 ------------------------------------------------------------

 # Filter and create table
 speed3 <- id_tab[xp_level == "advanced" &
                  variable %in% c("pred_speed", "success") & 
                  sigma == 0]
 id <- unique(data[xp_level == "advanced", .(predator_id)])
 
 speed3 <- merge(speed3, id, by = "predator_id")


 # Copy value column to back transform
 speed3[, value_bt := value]
 
 mean3 <- mean(data[xp_level == "advanced", pred_speed])
 sd3 <- sd(data[xp_level == "advanced", pred_speed])
 
 speed3[variable == "pred_speed",
        value_bt := (value_bt * sd3) + mean3]

 speed3[variable == "success",
        value_bt := (value_bt + 4) / 2]


 # Synthetic table
 speed3 <- speed3[, .(mean = mean(value),
                      mean_bt = mean(value_bt),
                      lower_ci = lower_interval(value),
                      upper_ci = upper_interval(value),
                      lower_ci_bt = lower_interval(value_bt),
                      upper_ci_bt = upper_interval(value_bt)),
                    by = .(predator_id, variable)]



# For plot 4 ------------------------------------------------------------

# Filter and create table
 speed4 <- id_tab[xp_level == "novice" &
                  variable == "pred_speed" & 
                  sigma == 1]
 bind4 <-  id_tab[xp_level == "novice" &
                 variable == "success"]
 speed4 <- rbind(speed4, bind4)
 speed4 <- merge(speed4, id, by = "predator_id")


 # Copy value column to back transform
 speed4[, value_bt := value]
 
 speed4[variable == "pred_speed",
        value_bt := exp(value)]

 speed4[variable == "success",
        value_bt := (value_bt + 4) / 2]


 # Synthetic table
 speed4 <- speed4[, .(mean = mean(value),
                      mean_bt = mean(value_bt),
                      lower_ci = lower_interval(value),
                      upper_ci = upper_interval(value),
                      lower_ci_bt = lower_interval(value_bt),
                      upper_ci_bt = upper_interval(value_bt)),
                    by = .(predator_id, variable)]



# Extract correlations for plot labels ----------------------------------

 # Correlations
 corr <- VarCorr(fit)$predator_id$cor
 
 # Success novice
 corr_novice <- corr[13, c(1, 3, 4), c(1, 2)]
 
 # Success advanced
 corr_advanced <- corr[15, c(1, 3, 4), c(5, 6)]



# Extract values to plot the line ---------------------------------------

# Plot 1
cov1 <- (
  as_draws_matrix(
       fit,
       variable = "cor_predator_id__speednovice_Intercept__successnovice_Intercept"
       ) *
  
  sqrt((as_draws_matrix(
       fit,
       variable = "sd_predator_id__speednovice_Intercept"
       )^2)
  ) *
  sqrt((as_draws_matrix(
       fit,
       variable = "sd_predator_id__successnovice_Intercept"
       )^2)
  )
)

var1 <- (as_draws_matrix(
       fit,
       variable = "sd_predator_id__speednovice_Intercept")
)^2

slope1 <- cov1 / var1


# Plot 2
cov2 <- (
  as_draws_matrix(
       fit,
       variable = "cor_predator_id__sigma_speednovice_Intercept__successnovice_Intercept"
       ) *
  
  sqrt((as_draws_matrix(
       fit,
       variable = "sd_predator_id__sigma_speednovice_Intercept"
       )^2)
  ) *
  sqrt((as_draws_matrix(
       fit,
       variable = "sd_predator_id__successnovice_Intercept"
       )^2)
  )
)

var2 <- (as_draws_matrix(
       fit,
       variable = "sd_predator_id__sigma_speednovice_Intercept")
)^2

slope2 <- cov2 / var2


# Plot 3
cov3 <- (
  as_draws_matrix(
       fit,
       variable = "cor_predator_id__speedadvanced_Intercept__successadvanced_Intercept"
       ) *
  
  sqrt((as_draws_matrix(
       fit,
       variable = "sd_predator_id__speedadvanced_Intercept"
       )^2)
  ) *
  sqrt((as_draws_matrix(
       fit,
       variable = "sd_predator_id__successadvanced_Intercept"
       )^2)
  )
)

var3 <- (as_draws_matrix(
       fit,
       variable = "sd_predator_id__speedadvanced_Intercept")
)^2

slope3 <- cov3 / var3


# Plot 4
cov4 <- (
  as_draws_matrix(
       fit,
       variable = "cor_predator_id__sigma_speedadvanced_Intercept__successadvanced_Intercept"
       ) *
  
  sqrt((as_draws_matrix(
       fit,
       variable = "sd_predator_id__sigma_speedadvanced_Intercept"
       )^2)
  ) *
  sqrt((as_draws_matrix(
       fit,
       variable = "sd_predator_id__successadvanced_Intercept"
       )^2)
  )
)

var4 <- (as_draws_matrix(
       fit,
       variable = "sd_predator_id__sigma_speedadvanced_Intercept")
)^2

slope4 <- cov4 / var4

# =======================================================================
# =======================================================================





# =======================================================================
# 5. Compute plots
# =======================================================================


# Speed-success novices -------------------------------------------------

plot1 <- ggplot() + 
   
   geom_segment(aes(y = speed1[variable == "success"]$lower_ci_bt,
                    yend = speed1[variable == "success"]$upper_ci_bt,
                    x = speed1[variable == "pred_speed"]$mean_bt,
                    xend = speed1[variable == "pred_speed"]$mean_bt),
                color = "#999999",
                alpha = 0.2) +
   geom_segment(aes(y = speed1[variable == "success"]$mean_bt,
                    yend = speed1[variable == "success"]$mean_bt,
                    x = speed1[variable == "pred_speed"]$lower_ci_bt,
                    xend = speed1[variable == "pred_speed"]$upper_ci_bt), 
                color = "#999999",
                alpha = 0.2) +
   
   geom_point(aes(y = speed1[variable == "success"]$mean_bt, 
                  x = speed1[variable == "pred_speed"]$mean_bt),
              shape = 16,
              size = 2,
              color = "#999999",
              alpha = 0.8,
              stroke = 0) +
   
   geom_segment(aes(x = 2.5,
                    xend = 3.55,
                    y = ((-0.01 + 4) / 2) + mean(slope1) * -1, 
                    yend = ((-0.01 + 4) / 2) + mean(slope1) * 1),
                color = "purple4", size = 1, alpha = 0.8) +
    
    scale_x_continuous(breaks = seq(2, 3.5, 0.5),
                       limits = c(1.8, 3.7)) +
    
    xlab("\nPredator speed (individual mean)") +
    ylab("Hunting success (individual mean)\n") +
    labs(title = "Novices \nCorrelation = 0.107 (0.006, 0.210)") +
    
    custom_theme +
    theme(plot.title = element_text(size = 15,
                                    face = "bold",
                                    color = "#999999"))



# Sigmaspeed-success novices --------------------------------------------

plot2 <- ggplot() + 
   
   geom_segment(aes(y = speed2[variable == "success"]$lower_ci_bt,
                    yend = speed2[variable == "success"]$upper_ci_bt,
                    x = speed2[variable == "pred_speed"]$mean_bt,
                    xend = speed2[variable == "pred_speed"]$mean_bt),
                color = "#999999",
                alpha = 0.2) +
   geom_segment(aes(y = speed2[variable == "success"]$mean_bt,
                    yend = speed2[variable == "success"]$mean_bt,
                    x = speed2[variable == "pred_speed"]$lower_ci_bt,
                    xend = speed2[variable == "pred_speed"]$upper_ci_bt), 
                color = "#999999",
                alpha = 0.2) +
   
   geom_point(aes(y = speed2[variable == "success"]$mean_bt, 
                  x = speed2[variable == "pred_speed"]$mean_bt),
              shape = 16,
              size = 2,
              color = "#999999",
              alpha = 0.8,
              stroke = 0) +
    
   geom_segment(aes(x = 0,
                      xend = 6,
                      y = ((-0.01 + 4) / 2) + mean(slope2) * -1, 
                      yend = ((-0.01 + 4) / 2) + mean(slope2) * 1),
                  color = "purple4", size = 1, alpha = 0.8) +

   scale_x_continuous(breaks = seq(0, 8, 2),
                      limits = c(0, 8.5)) +
   xlab("\nPredator speed (IIV)") +
   ylab("Hunting success (individual mean)\n") +
   labs(title = "Novices \nCorrelation = -0.216 (-0.313, -0.117)") +
   
   custom_theme +
   theme(plot.title = element_text(size = 15,
                                   face = "bold",
                                   color = "#999999"))



# Speed-success advanced ------------------------------------------------

plot3 <- ggplot() + 
   
   geom_segment(aes(y = speed3[variable == "success"]$lower_ci_bt,
                    yend = speed3[variable == "success"]$upper_ci_bt,
                    x = speed3[variable == "pred_speed"]$mean_bt,
                    xend = speed3[variable == "pred_speed"]$mean_bt),
                color = "#00AFBB",
                alpha = 0.2) +
   geom_segment(aes(y = speed3[variable == "success"]$mean_bt,
                    yend = speed3[variable == "success"]$mean_bt,
                    x = speed3[variable == "pred_speed"]$lower_ci_bt,
                    xend = speed3[variable == "pred_speed"]$upper_ci_bt), 
                color = "#00AFBB",
                alpha = 0.2) +
   
   geom_point(aes(y = speed3[variable == "success"]$mean_bt, 
                  x = speed3[variable == "pred_speed"]$mean_bt),
              shape = 16,
              size = 2,
              color = "#00AFBB",
              alpha = 0.8,
              stroke = 0) +

   geom_segment(aes(x = 2,
                    xend = 3.55,
                    y = ((-0.01 + 4) / 2) + mean(slope3) * -1, 
                    yend = ((-0.01 + 4) / 2) + mean(slope3) * 1),
                color = "purple4", size = 1, alpha = 0.8) +

   scale_x_continuous(breaks = seq(2, 3.5, 0.5),
                       limits = c(1.8, 3.7)) + 
   scale_y_continuous(breaks = seq(0, 4, 1),
                      limits = c(0, 4)) +
   
   xlab("\nPredator speed (individual mean)") +
   ylab("Hunting success (individual mean)\n") +
   labs(title = "Advanced \nCorrelation = 0.192 (0.060, 0.312)") +
    
   custom_theme +
   theme(plot.title = element_text(size = 15,
                                   face = "bold",
                                   color = "#00AFBB"))



# Sigmaspeed-success advanced -------------------------------------------

plot5 <- ggplot() + 
   
   geom_segment(aes(y = speed4[variable == "success"]$lower_ci_bt,
                    yend = speed4[variable == "success"]$upper_ci_bt,
                    x = speed4[variable == "pred_speed"]$mean_bt,
                    xend = speed4[variable == "pred_speed"]$mean_bt),
                color = "#00AFBB",
                alpha = 0.2) +
   geom_segment(aes(y = speed4[variable == "success"]$mean_bt,
                    yend = speed4[variable == "success"]$mean_bt,
                    x = speed4[variable == "pred_speed"]$lower_ci_bt,
                    xend = speed4[variable == "pred_speed"]$upper_ci_bt), 
                color = "#00AFBB",
                alpha = 0.2) +
   
   geom_point(aes(y = speed4[variable == "success"]$mean_bt, 
                  x = speed4[variable == "pred_speed"]$mean_bt),
              shape = 16,
              size = 2,
              color = "#00AFBB",
              alpha = 0.8,
              stroke = 0) +
    
   geom_segment(aes(x = 0,
                    xend = 6,
                    y = ((-0.01 + 4) / 2) + mean(slope4) * -1, 
                    yend = ((-0.01 + 4) / 2) + mean(slope4) * 1),
                color = "purple4", size = 1, alpha = 0.8) +

    scale_x_continuous(breaks = seq(0, 8, 2),
                       limits = c(0, 8.5)) +

    xlab("\nPredator speed (IIV)") +
    ylab("Hunting success (individual mean)\n") +
    labs(title = "Advanced \nCorrelation = -0.236 (-0.365, -0.102)") +
    
    custom_theme +
    theme(plot.title = element_text(size = 15,
                                    face = "bold",
                                    color = "#00AFBB"))


# =======================================================================
# =======================================================================





# =======================================================================
# 5. Arrange figure
# =======================================================================

# Combine as one figure ------------------------------------------------
 
 # Arrange paneled figure
 figure <- ggarrange(plot1, plot2, plot3, plot4,
                     ncol = 2, nrow = 2,
                     labels = c("(A)", "(B)", "(C)", "(D)"))



# Save figure ----------------------------------------------------------

 # Folder path
 path <- "./outputs/05_outputs_figures"

 # Save figure
 ggexport(figure,
          filename = file.path(path, "05_figure4.png"),
          width = 3500,
          height = 3500,
          res = 300)

# =======================================================================
# =======================================================================