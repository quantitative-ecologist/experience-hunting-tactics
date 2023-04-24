# ==================================================================================

#                       Inspect prior and posterior draws
#                               from GAMM-GLMM models

# ==================================================================================





# ==================================================================================
# Prepare session
# ==================================================================================


# Load libraries -------------------------------------------------------------------

 library(brms)
 library(ggplot2)



# Prepare model draws --------------------------------------------------------------

 path <- file.path(getwd(), "outputs", "02_outputs_models")
 
 # Import model in R session
 mod1 <- readRDS(file.path(path, "02A1_GAMM.rds"))
 mod2 <- readRDS(file.path(path, "02A2_GAMM.rds"))
 mod3 <- readRDS(file.path(path, "02A3_GAMM.rds"))
 
 
 # Extract posterior draws
 posterior_fit1 <- as_draws_df(mod1)
 posterior_fit2 <- as_draws_df(mod2)
 posterior_fit3 <- as_draws_df(mod3)

# ==================================================================================
# ==================================================================================





# ==================================================================================
# 1. Model 1
# ==================================================================================



# Plot prior and posterior draws ---------------------------------------------------

 # Intercept
 ggplot(posterior_fit1) +
   geom_density(aes(prior_Intercept),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(Intercept),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()
 
 
 # Game duration
 ggplot(posterior_fit1) +
   geom_density(aes(prior_b_Zgame_duration),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(b_Zgame_duration),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()
 
 # slope term of cumul xp
 ggplot(posterior_fit1) +
   geom_density(aes(prior_bs_sZcumul_xp_1),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(bs_sZcumul_xp_1),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()
 
 
 # Standard deviation of cumul XP smooth term
 ggplot(posterior_fit1) +
   geom_density(aes(prior_sds_sZcumul_xp_1),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(sds_sZcumul_xp_1),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()
 
 # Standard deviation of predator ID
 ggplot(posterior_fit1) +
   geom_density(aes(prior_sds_spredator_id_1),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(sds_spredator_id_1),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()
 
 # Phi parameter
 ggplot(posterior_fit1) +
   geom_density(aes(prior_phi),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(phi),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()

# ==================================================================================
# ==================================================================================





# ==================================================================================
# 2. Model 4
# ==================================================================================



# Plot prior and posterior draws ---------------------------------------------------

 # Intercept
 ggplot(posterior_fit2) +
   geom_density(aes(prior_Intercept),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(Intercept),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()
 
 
 # Game duration
 ggplot(posterior_fit2) +
   geom_density(aes(prior_b_Zgame_duration),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(b_Zgame_duration),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()
 
 
 # slope term of cumul xp
 ggplot(posterior_fit2) +
   geom_density(aes(prior_bs_sZcumul_xp_1),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(bs_sZcumul_xp_1),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()
 
 
 # Standard deviation of cumul XP smooth term
 ggplot(posterior_fit2) +
   geom_density(aes(prior_sds_sZcumul_xp_1),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(sds_sZcumul_xp_1),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()
 
 # Standard deviation of predator ID
 ggplot(posterior_fit2) +
   geom_density(aes(prior_sds_spredator_id_1),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(sds_spredator_id_1),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()
 
# ==================================================================================
# ==================================================================================





# ==================================================================================
# 3. Model 7
# ==================================================================================



# Plot prior and posterior draws ---------------------------------------------------

 # Intercept
 ggplot(posterior_fit3) +
   geom_density(aes(prior_Intercept),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(Intercept),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()
 
 
 # Game duration
 ggplot(posterior_fit3) +
   geom_density(aes(prior_b_Zgame_duration),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(b_Zgame_duration),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()
 
 # slope term of cumul xp
 ggplot(posterior_fit3) +
   geom_density(aes(prior_bs_sZcumul_xp_1),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(bs_sZcumul_xp_1),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()
 
 
 # Standard deviation of cumul XP smooth term
 ggplot(posterior_fit3) +
   geom_density(aes(prior_sds_sZcumul_xp_1),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(sds_sZcumul_xp_1),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()
 
 # Standard deviation of predator ID
 ggplot(posterior_fit3) +
   geom_density(aes(prior_sds_spredator_id_1),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(sds_spredator_id_1),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()

 # Phi parameter
 ggplot(posterior_fit3) +
   geom_density(aes(prior_phi),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(phi),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()

# ==================================================================================
# ==================================================================================