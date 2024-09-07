# =======================================================================

# Retrieve GAMM file from OSF repository : https://osf.io/hdv38/
# Model output from : https://github.com/quantitative-ecologist/predator-expertise

# =======================================================================

# Load osfr
library(osfr)

# Create dir
dir.create("osf_folder", showWarnings = FALSE)

# List all files in the OSF repository
files <- osf_retrieve_node("hdv38") %>% osf_ls_files()

# Identify the file ID
id <- files[files$name=="A3_GAMM-speed-rank.rds",]$id

# retrieve id and download the 'osf_folder' folder
osf_retrieve_file(id) %>%
  osf_download(path = "osf_folder", conflicts = "overwrite")