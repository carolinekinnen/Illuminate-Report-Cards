 
library(ProjectTemplate)
load.project()

# Load Data ---------------------------------------------------------------

source(here::here("data", "02-A_GRADES_bq.R"))
source(here::here("data", "02-B_GRADES_manual.R"))
source(here::here("data", "02-C_GRADES_flat_files.R"))

# Munge Data --------------------------------------------------------------

# Check for duplicate subjects for students.
# this source file will produce a file in the output folder. 
source(here::here("munge", "02-A_GRADES_check_grades_file_for_issues.R"))

source(here::here("munge", "02-A_GRADES_munge_full_file.R"))
source(here::here("munge", "02-B_GRADES_munge.R"))


