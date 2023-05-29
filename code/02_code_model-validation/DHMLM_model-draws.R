# ==============================================================================

#                       Inspect prior and posterior draws
#                               from the MDHGLM model

# ==============================================================================





# ==============================================================================
# Prepare session
# ==============================================================================


# Load libraries ---------------------------------------------------------------

library(brms)
library(ggplot2)



# Prepare model draws ----------------------------------------------------------

path <- file.path(getwd(), "outputs", "01_outputs_models")

# Import model in R session
fit <- readRDS(file.path(path, "B1_DHMLM-no-outlier.rds"))


# Extract posterior draws
posterior_fit1 <- data.frame(
  as_draws_df(
    fit
  )[1:57]
)

posterior_fit2 <- data.frame(
  as_draws_df(
    fit
  )[163:180]
)

posterior_fit3 <- as_draws_df(
  fit,
  regex = TRUE,
  variable = "^prior"
)

posterior_fit <- cbind(posterior_fit1, posterior_fit2, posterior_fit3)

# ==============================================================================
# ==============================================================================





# ==============================================================================
# 1. Intercepts
# ==============================================================================


# Mean -------------------------------------------------------------------------

# Intercept speed
ggplot(posterior_fit) +
  geom_density(aes(prior_Intercept_speednovice),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(b_speednovice_Intercept),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) +
  theme_classic()


# Intercept prey speed
ggplot(posterior_fit) +
  geom_density(aes(prior_Intercept_preyspeednovice),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(b_preyspeednovice_Intercept),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) +
  theme_classic()

# Intercept success
ggplot(posterior_fit) +
  geom_density(aes(prior_Intercept_successnovice),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(b_successnovice_Intercept),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) +
  theme_classic()

ggplot(posterior_fit) +
  geom_density(aes(prior_Intercept_successadvanced),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(b_successadvanced_Intercept),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) +
  theme_classic()


# Sigma ------------------------------------------------------------------------

# Intercept speed
ggplot(posterior_fit) +
  geom_density(aes(prior_Intercept_sigma_speednovice),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(b_sigma_speednovice_Intercept),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) +
  theme_classic()


# Intercept prey speed
ggplot(posterior_fit) +
  geom_density(aes(prior_Intercept_sigma_preyspeednovice),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(b_sigma_preyspeednovice_Intercept),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) +
  theme_classic()

# ==============================================================================
# ==============================================================================





# ==============================================================================
# 2. Slope terms
# ==============================================================================


# Mean -------------------------------------------------------------------------

# slope rank pred speed
ggplot(posterior_fit) +
  geom_density(aes(prior_b_speednovice_Zprey_avg_rank),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(b_speednovice_Zprey_avg_rank),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) +
  theme_classic()

# slope rank prey speed
ggplot(posterior_fit) +
  geom_density(aes(prior_b_preyspeednovice_Zprey_avg_rank),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(b_preyspeednovice_Zprey_avg_rank),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) +
  theme_classic()

# game duration success
ggplot(posterior_fit) +
  geom_density(aes(prior_b_successnovice_Zgame_duration),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(b_successnovice_Zgame_duration),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) +
  theme_classic()



# Sigma ------------------------------------------------------------------------

# slope rank pred speed
ggplot(posterior_fit) +
  geom_density(aes(prior_b_sigma_speednovice_Zprey_avg_rank),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(b_sigma_speednovice_Zprey_avg_rank),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) +
  theme_classic()

# slope rank prey speed
ggplot(posterior_fit) +
  geom_density(aes(prior_b_sigma_preyspeednovice_Zprey_avg_rank),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(b_sigma_preyspeednovice_Zprey_avg_rank),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) +
  theme_classic()

# ==============================================================================
# ==============================================================================





# ==============================================================================
# 3. Standard deviations for random effects
# ==============================================================================


# Mean -------------------------------------------------------------------------

# pred speed predator ID
ggplot(posterior_fit) +
  geom_density(aes(prior_sd_predator_id__speednovice_Intercept),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(sd_predator_id__speednovice_Intercept),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) +
  theme_classic()

# prey speed predator ID
ggplot(posterior_fit) +
  geom_density(aes(prior_sd_predator_id__preyspeednovice_Intercept),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(sd_predator_id__preyspeednovice_Intercept),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) +
  theme_classic()

# success predator ID
ggplot(posterior_fit) +
  geom_density(aes(prior_sd_predator_id__successnovice_Intercept),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(sd_predator_id__successnovice_Intercept),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) +
  theme_classic()



# Sigma ------------------------------------------------------------------------

# pred speed predator ID
ggplot(posterior_fit) +
  geom_density(aes(prior_sd_predator_id__sigma_speednovice_Intercept),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(sd_predator_id__sigma_speednovice_Intercept),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) +
  theme_classic()

# prey speed predator ID
ggplot(posterior_fit) +
  geom_density(aes(prior_sd_predator_id__sigma_preyspeednovice_Intercept),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(sd_predator_id__sigma_preyspeednovice_Intercept),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) +
  theme_classic()

# ==============================================================================
# ==============================================================================