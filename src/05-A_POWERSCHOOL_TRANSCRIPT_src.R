
library(ProjectTemplate)
load.project()

# Load Data ---------------------------------------------------------------

source(here::here("data", "02-A_GRADES_bq.R"))
source(here::here("data", "02-B_GRADES_manual.R"))
source(here::here("data", "02-C_GRADES_flat_files.R"))
source(here::here("data", "04-A_DEANSLIST_flatfiles.R"))
source(here::here("data", "04-B_DEANSLIST_bq.R"))

# Munge Data --------------------------------------------------------------

source(here::here("munge", "02-A_GRADES_munge.R"))
source(here::here("munge", "02-B_GRADES_munge.R"))
source(here::here("munge", "05-A_POWERSCHOOL_TRANSCRIPT_munge.R"))
source(here::here("munge", "05-B_POWERSCHOOL_TRANSCRIPT_munge.R"))