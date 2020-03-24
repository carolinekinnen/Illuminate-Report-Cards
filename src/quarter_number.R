
library('ProjectTemplate')
load.project()

source(here::here("data", "01-bq_files.R"))

# -------------------- ### Quarter Numbers to Upload to Illuminate and place on Templates ### ------------------

# Only need to run and upload to Illuminate once a year

# Generate quarter number for template 
quarter_number <- students %>%
  filter(enroll_status == 0) %>%
  select(student_number) %>%
  mutate(q1 = 1,
         q2 = 2,
         q3 = 3,
         q4 = 4)