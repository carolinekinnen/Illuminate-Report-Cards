#2. --------------------- ### Write Attendance File for Illuminate  ### ------------------
# Write day after quarter ends

att_file <- sprintf(paste0("output/",
                           sy_abbreviation,
                           " Files/attendance/",
                           rc_quarter,  
                           " RC Attendance %s.csv"), lubridate::today())

write.csv(attend_school_grade_student_totals %>% 
            as.data.frame(), 
          file = here::here(att_file),
          row.names = FALSE)

