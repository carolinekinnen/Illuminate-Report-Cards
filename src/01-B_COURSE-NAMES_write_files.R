
# COURSE FILES FOR ILLUMINATE ---------------------------------------------
# Write when sending out report card templates

# Middle

# Removing all NAs so they don't appear in import file
middle_course_teacher <- course_names_teachers_final %>%
  mutate_all(funs(replace(., is.na(.), "")))

middle_course_teacher_file <- sprintf(paste0("output/20-21/course_teacher/middle_course_teacher_name_",
                                             rc_quarter,
                                             "_%s.csv"),
                                      today())

write.csv(middle_course_teacher %>%
            as.data.frame(),
          file = here::here(middle_course_teacher_file), 
          row.names = FALSE)

# Primary

primary_course_teacher_file_name <- sprintf(paste0("output/20-21/course_teacher/primary_course_teacher_name_",
                                                   rc_quarter,
                                                   "_%s.csv"),
                                            today())

write.csv(primary_course_teacher %>%
            as.data.frame(),
          file = here::here(primary_course_teacher_file_name), 
          row.names = FALSE)