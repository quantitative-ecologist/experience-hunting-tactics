# =======================================================================

#                       Code to produce Figure S3                       #

# =======================================================================





# =======================================================================
# 1. Load libraries and import model
# =======================================================================

# Load libraries
library(brms)
library(ggplot2)
library(viridis)

path <- file.path(getwd(), "outputs", "02_outputs_models")
fit <- readRDS(file.path(path, "02B_DHMLM.rds"))

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Estimate differences between experience levels
# =======================================================================

# a -> Compare novices vs intermediate
# b -> Compare intermediate with advanced
# c-> Compare novices with advanced


# Do predators become increasingly flexible? ----------------------------

hyp1a <- "exp(sigma_speednovice_Intercept) < exp(sigma_speedinterm_Intercept)"
hyp1b <- "exp(sigma_speedinterm_Intercept) > exp(sigma_speedadvanced_Intercept)"
hyp1c <- "exp(sigma_speednovice_Intercept) > exp(sigma_speedadvanced_Intercept)"
hyp1 <- hypothesis(x = fit, hypothesis = c(hyp1a, hyp1b, hyp1c))



# Does individual variation in avg speed change with XP? ----------------

hyp2a <- "exp(predator_id__speednovice_Intercept) > exp(predator_id__speedinterm_Intercept)"
hyp2b <- "exp(predator_id__speedinterm_Intercept) < exp(predator_id__speedadvanced_Intercept)"
hyp2c <- "exp(predator_id__speednovice_Intercept) < exp(predator_id__speedadvanced_Intercept)"
hyp2 <- hypothesis(x = fit, hypothesis = c(hyp2a, hyp2b, hyp2c), class = "sd")



# Does individual variation in speed var. change with XP? ---------------

hyp3a <- "exp(predator_id__sigma_speednovice_Intercept) < exp(predator_id__sigma_speedinterm_Intercept)"
hyp3b <- "exp(predator_id__sigma_speedinterm_Intercept) < exp(predator_id__sigma_speedadvanced_Intercept)"
hyp3c <- "exp(predator_id__speednovice_Intercept) < exp(predator_id__speedadvanced_Intercept)"
hyp3 <- hypothesis(x = fit, hypothesis = c(hyp3a, hyp3b, hyp3c), class = "sd")



# Do predators differ in the avg speed of prey encountered? -------------

hyp4a <- "exp(predator_id__preyspeednovice_Intercept) > exp(predator_id__preyspeedinterm_Intercept)"
hyp4b <- "exp(predator_id__preyspeedinterm_Intercept) < exp(predator_id__preyspeedadvanced_Intercept)"
hyp4c <- "exp(predator_id__preyspeednovice_Intercept) < exp(predator_id__preyspeedadvanced_Intercept)"
hyp4 <- hypothesis(x = fit, hypothesis = c(hyp4a, hyp4b, hyp4c), class = "sd")



# Do predators differ in the var. speed of prey encountered? ------------

hyp5a <- "exp(predator_id__sigma_preyspeednovice_Intercept) < exp(predator_id__sigma_preyspeedinterm_Intercept)"
hyp5b <- "exp(predator_id__sigma_preyspeedinterm_Intercept) < exp(predator_id__sigma_preyspeedadvanced_Intercept)"
hyp5c <- "exp(predator_id__sigma_preyspeednovice_Intercept) < exp(predator_id__sigma_preyspeedadvanced_Intercept)"
hyp5 <- hypothesis(x = fit, hypothesis = c(hyp5a, hyp5b, hyp5c), class = "sd")



# Does individual variation in success change with XP? ------------------

hyp6a <- "exp(predator_id__successnovice_Intercept) > exp(predator_id__successinterm_Intercept)"
hyp6b <- "exp(predator_id__successinterm_Intercept) > exp(predator_id__successadvanced_Intercept)"
hyp6c <- "exp(predator_id__successnovice_Intercept) > exp(predator_id__successadvanced_Intercept)"
hyp6 <- hypothesis(x = fit, hypothesis = c(hyp6a, hyp6b, hyp6c), class = "sd")

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Plot the hypotheses
# =======================================================================


# Combine estimates as 1 table ------------------------------------------

tab <- rbind(
    # Test 1
    data.frame(
    trait = rep("sigma predator speed", 3),
    test = c("intercept < intercept",
             "intercept > intercept",
             "intercept > intercept"),
    hyp1$hypothesis[,c(2,4,5)]
    ),
    # Test 2
    data.frame(
        trait = rep("mean predator speed", 3),
        test = c("SD predator ID > SD predator ID",
                 "SD predator ID < SD predator ID",
                 "SD predator ID < SD predator ID"),
        hyp2$hypothesis[,c(2,4,5)]
    ),
    # Test 3
    data.frame(
        trait = rep("sigma predator speed", 3),
        test = c("SD predator ID < SD predator ID",
                 "SD predator ID < SD predator ID",
                 "SD predator ID < SD predator ID"),
        hyp3$hypothesis[,c(2,4,5)]
    ),
    # Test 4
    data.frame(
        trait = rep("mean prey speed", 3),
        test = c("SD predator ID > SD predator ID",
                 "SD predator ID < SD predator ID",
                 "SD predator ID < SD predator ID"),
        hyp4$hypothesis[,c(2,4,5)]
    ),
    # Test 5
    data.frame(
        trait = rep("sigma prey speed", 3),
        test = c("SD predator ID < SD predator ID",
                 "SD predator ID < SD predator ID",
                 "SD predator ID < SD predator ID"),
        hyp5$hypothesis[,c(2,4,5)]
    ),
    # Test 6
    data.frame(
        trait = rep("mean success", 3),
        test = c("SD predator ID > SD predator ID",
                 "SD predator ID > SD predator ID",
                 "SD predator ID > SD predator ID"),
        hyp6$hypothesis[,c(2,4,5)]
    )
)

# Add grouping column
tab <- cbind(
    group = rep(
        c("novice vs intermediate",
          "intermediate vs advanced",
          "novice vs advanced"),
        6
    ),
    tab
)

# Now round the values to 3 digits
tab[, c(4:6)] <- round(tab[, c(4:6)], digits = 3)



# Group variables as factor ---------------------------------------------

tab$group <- factor(
    tab$group,
    levels = c("novice vs intermediate",
               "intermediate vs advanced",
               "novice vs advanced")
)
tab$trait <- factor(
    tab$trait,
    levels = c("mean predator speed", "sigma predator speed",
               "mean prey speed", "sigma prey speed", "mean success")
)
tab$test <- as.factor(tab$test)



# Compute plots ---------------------------------------------------------

fig <- ggplot(
    tab,
    aes(x = test, y = Estimate, fill = trait, shape = trait)
) +
    geom_hline(
        yintercept = 0, size = 1, colour = "red",
        linetype = "dashed", alpha = 0.5
    ) +
    geom_pointrange(
        aes(ymin = CI.Lower, ymax = CI.Upper),
        size = 0.8, position = position_dodge(width = 0.8)
    ) +
    scale_shape_manual(
        name = "Trait :",
        values = c(22,23,24,25,21)
    ) +
    scale_fill_viridis(
        name = "Trait :",
        discrete = TRUE, option = "D"
    ) +
    ylab("\nEstimate") +
    coord_flip() +
    theme_bw() +
    facet_wrap(~ group) +
    theme(
        axis.title.y = element_blank(),
        axis.text = element_text(face = "plain", size = 11, color = "black"),
        axis.title = element_text(size = 13, face = "plain", color = "black"),
        strip.text = element_text(size = 11),
        legend.position = "top",
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11)
    )



# Export figure ---------------------------------------------------------

# Folder
path <- file.path(getwd(), "outputs", "05_outputs_figures")

# Export
ggsave(
    filename = file.path(path, "05_figureS4.png"),
    plot = fig,
    width = 32, height = 14, # 32 14
    units = "cm",
    dpi = 300, scale = 0.9
)

# =======================================================================
# =======================================================================