# =======================================================================

# Run only once

# This script retrieves a GAMM file from this OSF repository :
#  https://osf.io/hdv38/

# Original project repository :
#  https://github.com/quantitative-ecologist/predator-expertise

# =======================================================================


# Load osfr
library(osfr)

# Create dir
dir.create(file.path(getwd(), "data", "osf_folder"), showWarnings = FALSE)

# List all files in the OSF repository
files <- osf_retrieve_node("hdv38") %>% osf_ls_files()

# Identify the file ID
id <- files[files$name == "GAMM-V.rds", ]$id

# Specify path to save the file
path <- file.path(getwd(), "data", "osf_folder")

# Retrieve id and download to 'osf_folder'
osf_retrieve_file(id) %>%
  osf_download(path = path, conflicts = "overwrite")