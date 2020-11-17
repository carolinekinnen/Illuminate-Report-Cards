library(ProjectTemplate)
load.project()

# Load Data ---------------------------------------------------------------

source(here::here("data", "03_A_ATTENDANCE_bq.R"))
source(here::here("data", "03-B_ATTENDANCE_manual_tables.R"))

# Munge Data --------------------------------------------------------------

source(here::here("munge", "03-B_ATTENDANCE_munge.R"))

