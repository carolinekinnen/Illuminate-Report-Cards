#2. --------------------- ### Write Attendance File for Illuminate  ### ------------------
# Write day after quarter ends

att_file <- sprintf(paste0("output/",
                           "20-21/attendance/",
                           CURRENT_QUARTER,  
                           " RC Attendance %s.csv"), lubridate::today())

write.csv(attend_school_grade_student_totals %>% 
            as.data.frame(), 
          file = here::here(att_file),
          row.names = FALSE)

