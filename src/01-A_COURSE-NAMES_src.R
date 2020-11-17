
library(ProjectTemplate)
load.project()

# Load Data ---------------------------------------------------------------

source(here::here("data", "01-A_COURSE-NAMES_manual_tables_1.R"))
source(here::here("data", "01-B_COURSE-NAMES_bq.R"))
source(here::here("data", "01-C_COURSE-NAMES_manual_tables_2.R"))

# Munge Data --------------------------------------------------------------

source(here::here("munge", "01-A_COURSE-NAMES_munge.R"))
source(here::here("munge", "01-B_COURSE-NAMES_munge.R"))
