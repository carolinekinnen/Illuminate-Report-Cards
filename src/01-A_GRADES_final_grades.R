 
library(ProjectTemplate)
load.project()


# Load Data ---------------------------------------------------------------

source(here::here("data", "01-A_GRADES_bq.R"))
source(here::here("data", "01-B_GRADES_manual.R"))
source(here::here("data", "01-C_GRADES_flat_files.R"))


# Munge Data --------------------------------------------------------------

source(here::here("munge", "01-A_GRADES_munge.R"))
source(here::here("munge", "01-B_GRADES_munge.R"))


