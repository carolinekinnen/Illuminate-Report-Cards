 
library(ProjectTemplate)
load.project()

# Load Data ---------------------------------------------------------------

source(here::here("data", "02-A_GRADES_bq.R"))
source(here::here("data", "02-B_GRADES_manual.R"))
source(here::here("data", "02-C_GRADES_flat_files.R"))

# Munge Data --------------------------------------------------------------
source(here::here("munge", "02-A_GRADES_munge_current_quarter.R"))
source(here::here("munge", "02-B_GRADES_munge_final_output.R"))