##########################################################################

#               Make STAN code for the Multivariate model                #

##########################################################################

# expérience comme effet aléatoire. Mirrors_id serait niché dans experience
# car certains sont expérimentés alors que d'autres ne le sont pas.


# =======================================================================
# 1. Load libraries, and import dataset
# =======================================================================
# Load libraries
library(data.table)
library(brms)


data <- fread("./data/merged_data.csv")

# notes sur comment calculer l'experience sur les donnees et importer ici
# call julia? not great because maybe I can't load the environment
# run julia script that creates the varianles here

# =======================================================================
# 2. Prepare variables for the model
# =======================================================================

# Normalize sqrt variables (Z-scores)
standardize <- function (x) {(x - mean(x)) / sd(x)}

data[, c("Zsqrtspeed", "Zsqrtspace_covered_rate", "Zsqrtprox_mid_guard",
         "Zsqrthook_start_time", "Zsqrtsurv_speed", 
         "Zsqrtsurv_space_covered_rate") :=
                lapply(.SD, standardize), 
                .SDcols = c(6:11)]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Build the multivariate model 
# =======================================================================

# Formula for each response variable
# Each model will fit a seperate var-cov matrix for each random effect
speed_form <- bf(Zsqrtspeed ~
                  Zsqrtsurv_speed +
                  Zsqrtsurv_space_covered_rate +
                  Zsqrtsurv_unhooks +
                  (1 |a| map_name) +
                  (1 |b| character_name) +
                  (1 |c| mirrors_id)) +
                  gaussian()

space_form <- bf(Zsqrtspace_covered_rate ~
                  Zsqrtsurv_speed +
                  Zsqrtsurv_space_covered_rate +
                  Zsqrtsurv_unhooks +
                  (1 |a| map_name) +
                  (1 |b| character_name) +
                  (1 |c| mirrors_id)) +
                  gaussian()

guard_form <- bf(Zsqrtprox_mid_guard ~
                  Zsqrtsurv_speed +
                  Zsqrtsurv_space_covered_rate +
                  Zsqrtsurv_unhooks +
                  (1 |a| map_name) +
                  (1 |b| character_name) +
                  (1 |c| mirrors_id)) +
                  gaussian()

hook_form <- bf(Zsqrthook_start_time ~
                  Zsqrtsurv_speed +
                  Zsqrtsurv_space_covered_rate +
                  Zsqrtsurv_unhooks +
                  (1 |a| map_name) +
                  (1 |b| character_name) +
                  (1 |c| mirrors_id)) +
                  gaussian()

# priors
priors <- c(
  set_prior("normal(0, 5)", 
            class = "b",
            coef = "Zsqrtsurv_speed",
            resp = c("Zsqrtspeed", "Zsqrtspacecoveredrate", 
                     "Zsqrtproxmidguard", "Zsqrthookstarttime")),
  set_prior("normal(0, 5)", 
            class = "b",
            coef = "Zsqrtsurv_space_covered_rate",
            resp = c("Zsqrtspeed", "Zsqrtspacecoveredrate", 
                     "Zsqrtproxmidguard", "Zsqrthookstarttime")),
  set_prior("lkj(2)", 
            class = "cor",
            group = "character_name"),
  set_prior("lkj(2)", 
            class = "cor",
            group = "map_name"),
  set_prior("lkj(2)", 
            class = "cor",
            group = "mirrors_id"))

make_stancode(speed_form +
              space_form +
              guard_form +
              hook_form,
              set_rescor(TRUE),
              data = data,
              threads = threading(10),
              backend = "cmdstanr"
              )