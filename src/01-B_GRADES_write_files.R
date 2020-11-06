# 3. --------------------- ### Write Grade File for Illuminate  ### ------------------
# Write after gradebooks are finalized

final_grades_file <- sprintf(paste0("output/",
                                    sy_abbreviation,
                                    " Files/grades/",
                                    rc_quarter,  
                                    " RC Final Grades %s.csv"), lubridate::today())

write.csv(final_grades_gpa_illuminate_upload %>%
            as.data.frame(),
          file = here::here(final_grades_file),
          row.names = FALSE)
