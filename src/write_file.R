
library(ProjectTemplate)
load.project()

source(here::here("src", "Attendance.R"))


#--------------------- ### Write Attendance File ### ------------------

att_file <- sprintf(paste0("output/19-20 Files/attendance/SY19-20 ", 
                           rc_quarter,  
                           " RC Attendance %s.csv"), lubridate::today())

write.csv(attend_school_grade_student_totals %>% 
            as.data.frame(), 
          file = here::here(att_file),
          row.names = FALSE)
