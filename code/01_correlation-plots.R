
# =======================================================================

#                     Behavioural correlations plot                     #

# =======================================================================

# Code to produce Figure 2
# Powerpoint was used to produce the final adjustements
# -----------------------------------------------------------------------





# =======================================================================
# 1. Load libraries, and export dataset
# =======================================================================


# Librairies ------------------------------------------------------------

library(data.table)
library(brms)
library(corrplot)
library(export)



# Import the data -------------------------------------------------------

data <- fread("C:/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Data Behaviour/03_final-data/03_final-data_2021/final-data.csv",
              select = c("player_encode_id", "match_encode_id", 
                          "game_duration", "hook_count",
                         "hunting_success", "cumul_xp_pred_bins",
                         "pred_speed", "pred_amount_tiles_visited",
                         "ambush_time_close", "latency_1st_capture",
                         "prey_avg_speed", "prey_avg_amount_tiles_visited", 
                         "prey_total_heal_count", "prey_total_unhook_count"),
              stringsAsFactors = TRUE)

# Load model
model <- readRDS("./outputs/multivariate_model.rds")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. ICC table
# =======================================================================


# Extract the sd and correlation parameters -----------------------------

draws_tab <- data.table(as_draws_df(model,
                                    variable = c("sd", "cor", "sigma"), 
                                    regex = TRUE))

# Remove parameters that I don't need
draws_tab[, c(45:51, 61:897) := NULL]



# Standard deviations to variances for each random effect ---------------

draws_tab[, c(1:15) := 
          lapply(.SD, function(x) {x^2}),
            .SDcols = c(1:15)]



# Compute total variance ------------------------------------------------       

draws_tab[, ":=" (speednovice_var_total     = rowSums(draws_tab[, c(1, 45)]),
                  ambushnovice_var_total    = rowSums(draws_tab[, c(2, 48)]),
                  latencynovice_var_total   = rowSums(draws_tab[, c(3, 51)]),
                  speedinterm_var_total     = rowSums(draws_tab[, c(6, 46)]),
                  ambushinterm_var_total    = rowSums(draws_tab[, c(7, 49)]),
                  latencyinterm_var_total   = rowSums(draws_tab[, c(8, 52)]),
                  speedadvanced_var_total   = rowSums(draws_tab[, c(11, 47)]),
                  ambushadvanced_var_total  = rowSums(draws_tab[, c(12, 50)]),
                  latencyadvanced_var_total = rowSums(draws_tab[, c(13, 53)]))]



# Calculate ICCs --------------------------------------------------------
# ID
draws_tab[, ":=" (speednovice_icc = sd_player_encode_id__Zspeednovice_Intercept / speednovice_var_total,
                  ambushnovice_icc = sd_player_encode_id__Zambushnovice_Intercept / ambushnovice_var_total,
                  latencynovice_icc = sd_player_encode_id__Zambushnovice_Intercept / latencynovice_var_total,
                  speedinterm_icc = sd_player_encode_id__Zspeedinterm_Intercept / speedinterm_var_total,
                  ambushinterm_icc = sd_player_encode_id__Zambushinterm_Intercept / ambushinterm_var_total,
                  latencyinterm_icc = sd_player_encode_id__Zlatencyinterm_Intercept / latencyinterm_var_total,
                  speedadvanced_icc = sd_player_encode_id__Zspeedadvanced_Intercept / speedadvanced_var_total,
                  ambushadvanced_icc = sd_player_encode_id__Zambushadvanced_Intercept / ambushadvanced_var_total,
                  latencyadvanced_icc = sd_player_encode_id__Zlatencyadvanced_Intercept / latencyadvanced_var_total)]



# Create table with mean icc and credibility interval -------------------

# lower and upper interval functions
lower_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[1]}
upper_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[2]}

# Compute the table
icc_tab <- data.table(group = c("speednovice_icc", "ambushnovice_icc", "latencynovice_icc",
                                "speedinterm_icc", "ambushinterm_icc", "latencyinterm_icc",
                                "speedadvanced_icc", "ambushadvanced_icc", "latencyadvanced_icc"),
                       mean = as.numeric(draws_tab[, lapply(.SD, mean),
                                                   .SDcols = c(66:74)]),
                       lower = as.numeric(draws_tab[, lapply(.SD, lower_interval),
                                                   .SDcols = c(66:74)]),
                       upper = as.numeric(draws_tab[, lapply(.SD, upper_interval),
                                                   .SDcols = c(66:74)])
                        )

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Correlation table
# =======================================================================


# Create table with mean cor values and intervals -----------------------

cor_tab <- data.table(group = c("speed~ambush-novice", "speed~latency-novice", "speed~prey_speed-novice", 
                                "speed~ambush-interm", "speed~latency-interm", "speed~prey_speed-interm",
                                "speed~ambush-advanced", "speed~latency-advanced", "speed~prey_speed-advanced",
                                "speed~prey_sigspeed-novice", "speed~prey_sigspeed-interm", "speed~prey_sigspeed-advanced",
                                "ambush~latency-novice", "ambush~prey_speed-novice", "ambush~prey_sigspeed-novice",
                                "ambush~latency-interm", "ambush~prey_speed-interm", "ambush~prey_sigspeed-interm",
                                "ambush~latency-advanced","ambush~prey_speed-advanced", "ambush~prey_sigspeed-advanced",
                                "latency~prey_speed-novice", "latency~prey_speed-interm", "latency~prey_speed-advanced",
                                "latency~prey_sigspeed-novice", "latency~prey_sigspeed-interm", "latency~prey_sigspeed-advanced"),
                      mean = as.numeric(draws_tab[, lapply(.SD, mean),
                                                  .SDcols = c(16, 17, 19,
                                                              26, 27, 29,
                                                              36, 37, 39,
                                                              22, 32, 42,
                                                              18, 20, 23,
                                                              28, 30, 33,
                                                              38, 40, 43,
                                                              21, 31, 41,
                                                              24, 34, 44)]),
                      lower = as.numeric(draws_tab[, lapply(.SD, lower_interval),
                                                  .SDcols = c(16, 17, 19,
                                                              26, 27, 29,
                                                              36, 37, 39,
                                                              22, 32, 42,
                                                              18, 20, 23,
                                                              28, 30, 33,
                                                              38, 40, 43,
                                                              21, 31, 41,
                                                              24, 34, 44)]),
                      upper = as.numeric(draws_tab[, lapply(.SD, upper_interval),
                                                  .SDcols = c(16, 17, 19,
                                                              26, 27, 29,
                                                              36, 37, 39,
                                                              22, 32, 42,
                                                              18, 20, 23,
                                                              28, 30, 33,
                                                              38, 40, 43,
                                                              21, 31, 41,
                                                              24, 34, 44)])
                       )



# Add experience grouping variable --------------------------------------

cor_tab[, xp_group := c("novice", "novice", "novice",
                        "interm", "interm", "interm",
                        "advanced", "advanced", "advanced",
                        "novice", "interm", "advanced",
                        "novice", "novice", "novice",
                        "interm", "interm", "interm",
                        "advanced", "advanced", "advanced",
                        "novice", "interm", "advanced",
                        "novice", "interm", "advanced")]

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Variance-correlation  matrixes
# =======================================================================

# Novice ----------------------------------------------------------------
novice_cor <- cor_tab[xp_group %in% "novice", .(group, mean)]

# ICC values are on the diagonals
speed <- c(
      as.numeric(icc_tab[1,2]),
      as.numeric(cor_tab[1,2]),
      as.numeric(cor_tab[2,2]),
      as.numeric(cor_tab[4,2])
)

ambush <- c(
      as.numeric(cor_tab[8,2]),
      as.numeric(cor_tab[9,2]),
      as.numeric(icc_tab[3,2]),
      as.numeric(cor_tab[6,2])
)

latency <- c(
      as.numeric(cor_tab[10,2]),
      as.numeric(cor_tab[11,2]),
      as.numeric(cor_tab[12,2]),
      as.numeric(icc_tab[4,2])
)

prey_speed <- c(
      as.numeric(cor_tab[10,2]),
      as.numeric(cor_tab[11,2]),
      as.numeric(cor_tab[12,2]),
      "")
)

prey_sigmaspeed <- c(
      as.numeric(cor_tab[10,2]),
      as.numeric(cor_tab[11,2]),
      as.numeric(cor_tab[12,2]),
      "")
)

cor_matrix_novice <- cbind(speed, ambush, latency, prey_speed, prey_sigmaspeed)
rownames(id_cor_matrix) <- c("speed", "time ambush", "latency 1st cap.")
colnames(id_cor_matrix) <- c("speed", "time ambush", "latency 1st cap.")



# Intermediate ----------------------------------------------------------




# Advanced --------------------------------------------------------------

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Create different correlation plots and save in figure 2
# =======================================================================

# --------------------------------------------------
# Version with colors
# --------------------------------------------------
corrplot(id_cor_matrix, type = "full", method = "ellipse", 
                        cl.pos = "r", cl.cex = .85, tl.pos = "lt", tl.col = "black", 
                        tl.cex = 1.1, tl.srt = 45, number.digits = 3,
                        addCoef.col = "black", 
                        col = RColorBrewer::brewer.pal(n = 10, name = "RdBu"), 
                        mar = c(0,0,0,0))
graph2ppt(file = "./outputs/04_figure2.pptx", 
          width = 10, height = 6)

corrplot(map_cor_matrix, type = "lower", method = "ellipse", 
                        cl.pos = "r", cl.cex = .85, tl.pos = "lt", tl.col = "black", 
                        tl.cex = 1.1, tl.srt = 45, number.digits = 3,
                        addCoef.col = "black", 
                        col = RColorBrewer::brewer.pal(n = 10, name = "RdBu"), 
                        mar = c(0,0,0,0))
graph2ppt(file = "./outputs/04_figure2.pptx", 
          width = 10, height = 6, append = TRUE)

corrplot(char_cor_matrix, type = "lower", method = "ellipse", 
                        cl.pos = "r", cl.cex = .85, tl.pos = "lt", tl.col = "black", 
                        tl.cex = 1.1, tl.srt = 45, number.digits = 3,
                        addCoef.col = "black", 
                        col = RColorBrewer::brewer.pal(n = 10, name = "RdBu"), 
                        mar = c(0,0,0,0))
graph2ppt(file = "./outputs/04_figure2.pptx", 
          width = 10, height = 6, append = TRUE)
# --------------------------------------------------
# --------------------------------------------------



# --------------------------------------------------
# Black and gray version
# --------------------------------------------------
corrplot(id_cor_matrix, type = "full", method = "ellipse", 
         cl.pos = "r", cl.cex = .85, tl.pos = "lt", tl.col = "black", 
         tl.cex = 1.1, tl.srt = 45, number.digits = 3,
         addCoef.col = "#7e7c7c", 
         col = c("black", "lightgray"), 
         mar = c(0,0,0,0))
graph2ppt(file = "./outputs/04_figure2.pptx", 
          width = 10, height = 6, append = TRUE)

corrplot(map_cor_matrix, type = "full", method = "ellipse", 
         cl.pos = "r", cl.cex = .85, tl.pos = "lt", tl.col = "black", 
         tl.cex = 1.1, tl.srt = 45, number.digits = 3,
         addCoef.col = "#7e7c7c", 
         col = c("black", "lightgray"), 
         mar = c(0,0,0,0))
graph2ppt(file = "./outputs/04_figure2.pptx", 
          width = 10, height = 6, append = TRUE)

# --------------------------------------------------
# --------------------------------------------------

# =======================================================================
# =======================================================================