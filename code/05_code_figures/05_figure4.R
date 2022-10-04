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
 fit <- readRDS("./outputs/02_outputs_models/02B_DHMLM.rds")
 fit <- readRDS("./tests/02B_DHMLM.rds")
 
 # ID table
 id_tab <- readRDS("./tests/04_id-draws.rds")
 


# Load raw data ---------------------------------------------------------

 # Data
 data <- fread("./data/FraserFrancoetalXXXX-data.csv",
               select = c("predator_id",
                          "pred_game_duration",
                          "pred_speed",
                          "prey_avg_speed",
                          "cumul_xp_killer",
                          "total_xp_killer",
                          "hunting_success"))
 
 data <- unique(data)

 # Predator id as factor
 data[, predator_id := as.factor(predator_id)]

 # Calculate total xp
 data[total_xp_killer < 100,
      xp_level := "novice"]
 
 data[total_xp_killer %between% c(100, 299),
      xp_level := "interm"]
 
 data[total_xp_killer > 300,
      xp_level := "advanced"]

 # Encode the variable as a factor
 data[, xp_level := as.factor(xp_level)]

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
# 4. Prepare synthetic table for plots
# =======================================================================

 # Intervals
 lower_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[1]}
 upper_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[2]}
 
 # Summary table
 id_tab <- id_tab[, .(mean = mean(value),
                      lower_ci = lower_interval(value),
                      upper_ci = upper_interval(value)),
                     by = .(xp_level, predator_id, variable, sigma)]

# =======================================================================
# =======================================================================





# =======================================================================
# 5. Compute plots
# =======================================================================


# Extract correlations for plot labels ----------------------------------

 # Correlations
 corr <- VarCorr(fit)$predator_id$cor
 
 # Success novice
 corr_novice <- corr[13, c(1, 3, 4), c(1, 2)]
 
 # Success advanced
 corr_advanced <- corr[15, c(1, 3, 4), c(5, 6)]



# Speed-success novices -------------------------------------------------

 # average speed table
 speed1 <- id_tab[xp_level == "novice" &
                  variable %in% c("pred_speed", "success") & 
                  sigma == 0]

plot1 <- ggplot() + 
   
   geom_segment(aes(x = speed1[variable == "success"]$lower_ci,
                    xend = speed1[variable == "success"]$upper_ci,
                    y = speed1[variable == "pred_speed"]$mean,
                    yend = speed1[variable == "pred_speed"]$mean),
                color = "#999999",
                alpha = 0.2) +
   geom_segment(aes(x = speed1[variable == "success"]$mean,
                    xend = speed1[variable == "success"]$mean,
                    y = speed1[variable == "pred_speed"]$lower_ci,
                    yend = speed1[variable == "pred_speed"]$upper_ci), 
                color = "#999999",
                alpha = 0.2) +
   
   geom_point(aes(x = speed1[variable == "success"]$mean, 
                  y = speed1[variable == "pred_speed"]$mean),
              shape = 16,
              size = 2,
              color = "#999999",
              alpha = 0.8,
              stroke = 0) +
    
    #scale_y_continuous(breaks = seq(-2, 2, 2),
    #                   limits = c(-2.5, 2.5)) +
    #scale_x_continuous(breaks = seq(-2, 2, 2),
    #                   limits = c(-3.5, 2.5)) +
    
    ylab("Predator speed (mean)\n") +
    xlab("\nHunting success (mean)") +
    labs(title = "Novices \nCorrelation = 0.099 (-0.012, 0.201)") +
    
    custom_theme +
    theme(plot.title = element_text(size = 15,
                                    face = "bold",
                                    color = "#999999"))



# Sigmaspeed-success novices --------------------------------------------

 # sigma speed table
 speed2 <- id_tab[xp_level == "novice" &
                  variable == "pred_speed" & 
                  sigma == 1]
 bind <- id_tab[xp_level == "novice" &
                variable == "success"]
 speed2 <- rbind(speed2, bind)

plot2 <- ggplot() + 
   
   geom_segment(aes(x = speed2[variable == "success"]$lower_ci,
                    xend = speed2[variable == "success"]$upper_ci,
                    y = speed2[variable == "pred_speed"]$mean,
                    yend = speed2[variable == "pred_speed"]$mean),
                color = "#999999",
                alpha = 0.2) +
   geom_segment(aes(x = speed2[variable == "success"]$mean,
                    xend = speed2[variable == "success"]$mean,
                    y = speed2[variable == "pred_speed"]$lower_ci,
                    yend = speed2[variable == "pred_speed"]$upper_ci), 
                color = "#999999",
                alpha = 0.2) +
   
   geom_point(aes(x = speed2[variable == "success"]$mean, 
                  y = speed2[variable == "pred_speed"]$mean),
              shape = 16,
              size = 2,
              color = "#999999",
              alpha = 0.8,
              stroke = 0) +
    
    #scale_y_continuous(breaks = seq(-2, 2, 2),
    #                   limits = c(-3, 2.5)) +
    #scale_x_continuous(breaks = seq(-2, 2, 2),
    #                   limits = c(-3.5, 2.5)) +
    
    ylab("Predator speed (IIV)\n") +
    xlab("\nHunting success (mean)") +
    labs(title = "Novices \nCorrelation = -0.206 (-0.304, -0.101)") +
    
    custom_theme +
    theme(plot.title = element_text(size = 15,
                                    face = "bold",
                                    color = "#999999"))



# Speed-success advanced ------------------------------------------------

# average speed table
 speed3 <- id_tab[xp_level == "advanced" &
                  variable %in% c("pred_speed", "success") & 
                  sigma == 0]
 id <- unique(data[xp_level == "advanced", .(predator_id)])
 
 speed3 <- merge(speed3, id, by = "predator_id")

plot3 <- ggplot() + 
   
   geom_segment(aes(x = speed3[variable == "success"]$lower_ci,
                    xend = speed3[variable == "success"]$upper_ci,
                    y = speed3[variable == "pred_speed"]$mean,
                    yend = speed3[variable == "pred_speed"]$mean),
                color = "#00AFBB",
                alpha = 0.2) +
   geom_segment(aes(x = speed3[variable == "success"]$mean,
                    xend = speed3[variable == "success"]$mean,
                    y = speed3[variable == "pred_speed"]$lower_ci,
                    yend = speed3[variable == "pred_speed"]$upper_ci), 
                color = "#00AFBB",
                alpha = 0.2) +
   
   geom_point(aes(x = speed3[variable == "success"]$mean, 
                  y = speed3[variable == "pred_speed"]$mean),
              shape = 16,
              size = 2,
              color = "#00AFBB",
              alpha = 0.8,
              stroke = 0) +
    
    #scale_y_continuous(breaks = seq(-2, 2, 2),
    #                   limits = c(-2.5, 2.5)) +
    #scale_x_continuous(breaks = seq(-2, 2, 2),
    #                   limits = c(-3.5, 2.5)) +
    
    ylab("Predator speed (mean)\n") +
    xlab("\nHunting success (mean)") +
    labs(title = "Advanced \nCorrelation = -0.186 (0.059, 0.316)") +
    
    custom_theme +
    theme(plot.title = element_text(size = 15,
                                    face = "bold",
                                    color = "#00AFBB"))



# Sigmaspeed-success advanced -------------------------------------------

 # sigma speed table
 speed4 <- id_tab[xp_level == "advanced" &
                  variable == "pred_speed" & 
                  sigma == 1]
 bind <- id_tab[xp_level == "advanced" &
                variable == "success"]
 speed4 <- rbind(speed4, bind)
 speed4 <- merge(speed4, id, by = "predator_id")


plot4 <- ggplot() + 
   
   geom_segment(aes(x = speed4[variable == "success"]$lower_ci,
                    xend = speed4[variable == "success"]$upper_ci,
                    y = speed4[variable == "pred_speed"]$mean,
                    yend = speed4[variable == "pred_speed"]$mean),
                color = "#00AFBB",
                alpha = 0.2) +
   geom_segment(aes(x = speed4[variable == "success"]$mean,
                    xend = speed4[variable == "success"]$mean,
                    y = speed4[variable == "pred_speed"]$lower_ci,
                    yend = speed4[variable == "pred_speed"]$upper_ci), 
                color = "#00AFBB",
                alpha = 0.2) +
   
   geom_point(aes(x = speed4[variable == "success"]$mean, 
                  y = speed4[variable == "pred_speed"]$mean),
              shape = 16,
              size = 2,
              color = "#00AFBB",
              alpha = 0.8,
              stroke = 0) +
    
    scale_y_continuous(breaks = seq(-2, 2, 2),
                       limits = c(-3, 2.5)) +
    scale_x_continuous(breaks = seq(-2, 2, 2),
                       limits = c(-3.5, 2.5)) +
    
    ylab("Predator speed (IIV)\n") +
    xlab("\nHunting success (mean)") +
    labs(title = "Advanced \nCorrelation = -0.226 (-0.359, -0.095)") +
    
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

 # Folder path
 path <- "./outputs/05_outputs_figures"
 
 # Arrange paneled figure
 figure <- ggarrange(plot1, plot2, plot3, plot4,
                     ncol = 2, nrow = 2,
                     labels = c("(A)", "(B)", "(C)", "(D)"))


 # Save figure
 ggexport(figure,
          filename = file.path(path, "05_figure4.png"),
          width = 3500,
          height = 3500,
          res = 300)

# =======================================================================
# =======================================================================