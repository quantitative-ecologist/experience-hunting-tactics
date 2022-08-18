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
 fit <- readRDS("./outputs/02_outputs_models/02B_DHMLM.rds")
 


# Load data ------------------------------------------------------------
 
 # Data
 data <- fread("./data/FraserFrancoetalXXXX-data.csv",
               select = c("predator_id",
                          "total_xp_killer",
                          "pred_speed"))
 
 # Extract standard deviation of speed
 sd_speed1 <- sd(data[total_xp_killer < 150]$pred_speed)
 sd_speed2 <- sd(data[total_xp_killer >= 300]$pred_speed)

 # Filter only for advanced players
 data <- data[total_xp_killer >= 300]
 
 data <- unique(data)
 
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
         variable = c("r_predator_id__sigma_Zspeed"),
         regex = TRUE)
 )

 draws[, c(1:33, 308:581) := NULL]
 draws[, c(549:551) := NULL]

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
draws[, predator_id := as.factor(predator_id)]

# Rename value to speed
setnames(draws, "value", "Zspeed_sigma")



# Transform  ------------------------------------------------------------

# Back transform to original scale (sigma is on log scale)
draws[, exp_Zspeed_sigma := exp(Zspeed_sigma)]


# Calculate mean predicted value for each individual
draws <- draws[, average_Zspeed_sigma := mean(exp_Zspeed_sigma),
                 by = c("predator_id",
                        "xp_level")]

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Compute the two plots
# =======================================================================


# Merging draws table with data -----------------------------------------

# Only keep players that played above 300 matches
table <- merge(unique(data[, "predator_id"]),
               draws,
               by = "predator_id")



# Set custom theme ------------------------------------------------------

 custom_theme <- theme(# axis values size
                        axis.text.x = element_text(face = "plain", 
                                                   size = 15,
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
                        axis.title = element_text(size = 17, 
                                                  face = "plain",
                                                  color = "black"),
                        axis.line = element_line(size = 0.95,
                                                 color = "black"),
                        panel.grid = element_blank(),
                        panel.background = element_blank())
 
 # 1 digits to axis
 scaleFUN <- function(x) sprintf("%.1f", x)


# Plot experts ----------------------------------------------------------

scaled_breaks1 <- c(0.2 / sd_speed1,
                    0.6 / sd_speed1,
                    1.0 / sd_speed1,
                    1.4 / sd_speed1)


# Plot the distributions of advanced players
plot1 <- ggplot() +
    
    #geom_vline(xintercept = exp(fixef(fit)[6]),
    #               linetype = "dashed",
    #               size = 1) +

    geom_density_ridges(data = table[xp_level == "advanced"],
                        rel_min_height = 0.005,
                        fill = "#00AFBB",
                        aes(x = exp_Zspeed_sigma,
                            y = predator_id,
                            height = ..density..,
                            scale = 3)) +
    
    geom_point(data = unique(table[xp_level == "advanced", c(1, 5)]),
               aes(x = average_Zspeed_sigma, 
                   y = predator_id),
               size = 1,
               color = "black") +
    
    scale_x_continuous(breaks = scaled_breaks1,
                       labels = scaleFUN,
                       limits = c(0, 4)) +

    #scale_x_continuous(#breaks = c(-1, 0, 1),
    #                   limits = c(0.1, 3.5),
    #                   #expand = c(0, 0),
    #                   sec.axis = sec_axis(trans = ~.,
    #                                       breaks = scaled_breaks1,
    #                                       labels = c(0.2, 0.4, 0.6, 0.8, 1),
    #                                       name = "Within individual variance (m/s)\n")) +
    
    ylab("Predator ID\n") +
    xlab("\nWithin individual variance (m/s)") +
    labs(title = "After playing 300 matches") +
    
    custom_theme +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(size = 15))



# Plot novices ----------------------------------------------------------

scaled_breaks2 <- c(0.2 / sd_speed2,
                    0.6 / sd_speed2,
                    1.0 / sd_speed2,
                    1.4 / sd_speed2)


# Plot the distributions of advanced players when they were novice
plot2 <- ggplot() +

    #geom_vline(xintercept = exp(fixef(fit)[2]),
    #           linetype = "dashed",
    #           size = 1) +
    
    geom_density_ridges(data = table[xp_level == "novice"],
                        rel_min_height = 0.005,
                        fill = "#999999",
                        aes(x = exp_Zspeed_sigma,
                            y = predator_id,
                            height = ..density..,
                            scale = 3)) +
    
    geom_point(data = unique(table[xp_level == "novice", c(1, 5)]),
               aes(x = average_Zspeed_sigma, 
                   y = predator_id),
               size = 1,
               color = "black") +
    
    scale_x_continuous(breaks = scaled_breaks2,
                       labels = c(0.5, 1.5, 2.5, 3.5),
                       limits = c(0, 4)) +

    #scale_x_continuous(#breaks = c(-1, 0, 1),
    #                   limits = c(0.1, 3.5),
    #                   #expand = c(0, 0),
    #                   sec.axis = sec_axis(trans = ~.,
    #                                       breaks = scaled_breaks2,
    #                                       labels = c(0.2, 0.4, 0.6, 0.8, 1),
    #                                       name = "Within individual variance (m/s)\n")) +

    ylab("Predator ID\n") +
    xlab("\nWithin individual variance (ms/s)") +
    labs(title = "First 150 matches") +

    custom_theme +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(size = 15))

# =======================================================================
# =======================================================================





# =======================================================================
# 5. Combine plots into 1 figure
# =======================================================================

# Combine as one figure -------------------------------------------------

 # Combine plots
 figure <- ggarrange(plot2, plot1,
                     labels = c("(A)", "(B)"),
                     ncol = 2, nrow = 1)

 # Folder path
 path <- "./outputs/05_outputs_figures"

 # Save figure
 ggexport(figure,
          filename = file.path(path, "05_figure3.png"),
          width = 3500,
          height = 1800,
          res = 300)

# correlation between sigma intercept = 0.40 [0.24, 0.55]

# =======================================================================
# =======================================================================