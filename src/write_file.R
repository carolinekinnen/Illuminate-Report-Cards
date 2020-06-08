
library(ProjectTemplate)
load.project()

# Write file will load project the project and run all the source scripts

# source(here::here("src", "category_comments.R"))
# source(here::here("src", "course_teacher_name.R"))
# source(here::here("src", "deanslist_upload.R"))
# source(here::here("src", "final_grades.R"))
# source(here::here("src", "KTC_data.R"))
# source(here::here("src", "powerschool_transcript_upload.R"))
# source(here::here("src", "quarter_number.R"))
# source(here::here("src", "retention_data.R"))


# --------------------- ### Write Quarter Number Files for Illuminate ### ------------------
# Only write and upload once a year

write.csv(quarter_number %>%
            as.data.frame(),
          file = here::here("output/19-20 Files/quarter_numbers/quarter_numbers.csv"), 
          row.names = FALSE)


# 1. --------------------- ### Write Courses Files for Illuminate ### ------------------
# Write when sending out report card templates

# Middle

# Removing all NAs so they don't appear in import file
middle_course_teacher <- course_names_teachers_final %>%
  mutate_all(funs(replace(., is.na(.), "")))

middle_course_teacher_file <- sprintf(paste0("output/19-20 Files/course_teacher/middle_course_teacher_name_",
                                             rc_quarter,
                                             "_%s.csv"),
                                      today())

write.csv(middle_course_teacher %>%
            as.data.frame(),
          file = here::here(middle_course_teacher_file), 
          row.names = FALSE)

# Primary

primary_course_teacher_file_name <- sprintf(paste0("output/19-20 Files/course_teacher/primary_course_teacher_name_",
                                              rc_quarter,
                                              "_%s.csv"),
                                       today())

write.csv(primary_course_teacher %>%
            as.data.frame(),
          file = here::here(primary_course_teacher_file_name), 
          row.names = FALSE)

primary_course_teacher_3_file_name <- sprintf(paste0("output/19-20 Files/course_teacher/primary_course_teacher_3_name_",
                                                     rc_quarter,
                                                     "_%s.csv"),
                                              today())

# only need for distance learning report card

# write.csv(course_3_teacher %>%
#             as.data.frame(),
#           file = here::here(primary_course_teacher_3_file_name), 
#           row.names = FALSE)


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


# 3. --------------------- ### Write Grade File for Illuminate  ### ------------------
# Write after gradebooks are finalized

final_grades_file <- sprintf(paste0("output/",
                                    sy_abbreviation,
                                    " Files/grades/",
                                    rc_quarter,  
                                    " RC Final Grades %s.csv"), lubridate::today())

# rename columns for easy update to Illuminate
final_grades_gpa_illuminate_upload <- final_grades_gpa_illuminate_upload %>%
  rename(`art grade` = grade_art,
         `art percent` = percent_art,
         `dance grade` = grade_dance,
         `dance percent` = percent_dance,
         `ela grade` = grade_ela,
         `ela percent` = percent_ela,
         `literacy centers grade` = grade_lit_centers,
         `literacy centers percent` = percent_lit_centers,
         `pe grade` = grade_pe,
         `pe percent` = percent_pe,
         `science grade` = grade_science,
         `science percent` = percent_science,
         `social studies grade` = grade_social,
         `social studies percent` = percent_social,
         `math grade` = grade_math,
         `math percent` = percent_math,
         `prealg grade` = grade_pre_algebra,
         `prealg percent` = percent_pre_algebra,
         `alg grade` = grade_algebra,
         `alg percent` = percent_algebra,
         gpa = cumulative_gpa)

write.csv(final_grades_gpa_illuminate_upload %>%
          as.data.frame(),
          file = here::here(final_grades_file),
          row.names = FALSE)


# 4. --------------------- ### Write Transcript File for Powerschool ### ------------------
# Write transcript file after grades are finalized 
# Also ps_upload_quarter needs to be saved in GCS to be pulled into Load GPAs code for On-Track Snapshots

ps_upload_file_name_quarter <- sprintf(paste0("output/",
                                    sy_abbreviation,
                                    " Files/transcripts/ps_upload_quarter_",
                                      rc_quarter,
                                      "_%s.csv"),
                               today())

write.csv(ps_upload_quarter %>%
            as.data.frame(),
          file = here::here(ps_upload_file_name_quarter),
          row.names = FALSE)

ps_upload_file_name_final <- sprintf(paste0("output/",
                                              sy_abbreviation,
                                              " Files/transcripts/ps_upload_final",
                                              "_%s.csv"),
                                       today())

write.csv(ps_upload_final %>%
            as.data.frame(),
          file = here::here(ps_upload_file_name_final),
          row.names = FALSE)

# 5. --------------------- ### Write Quarter Grades File for Deans List ### ------------------
# Write after grades are finalized 

dl_upload_file_name_input <- function(schoolname) {

  file <- sprintf(paste0("output/",
                         sy_abbreviation,
                         " Files/deanslist/deanslist_upload_",
                         schoolname, 
                         "_",
                         rc_quarter,
                         "_%s.csv"),
                  today())
}

write.csv(dl_upload_kac %>%
            as.data.frame(),
          file = dl_upload_file_name_input("KAC"),
          row.names = FALSE)

write.csv(dl_upload_kams %>%
            as.data.frame(),
          file = dl_upload_file_name_input("KAMS"),
          row.names = FALSE)

write.csv(dl_upload_kap %>%
            as.data.frame(),
          file = dl_upload_file_name_input("KAP"),
          row.names = FALSE)

write.csv(dl_upload_kbcp %>%
            as.data.frame(),
          file = dl_upload_file_name_input("KBCP"),
          row.names = FALSE)

write.csv(dl_upload_one %>%
            as.data.frame(),
          file = dl_upload_file_name_input("KOA"),
          row.names = FALSE)

# 6. --------------------#### Write Category Comments #### -------------------------
# Not writing yet because Maggie hasn't confirmed she's ok with plan
# also need to figure out Spanish


# 7. --------------------- ### Write File for KTC ### ------------------

ktc_7_request_file_name <- sprintf(paste0("output/",
                                              sy_abbreviation,
                                              " Files/ktc/ktc_7_",
                                              rc_quarter,
                                              "_%s.csv"),
                                       today())

write.csv(ktc_7_request %>%
          as.data.frame(),
          file = here::here(ktc_7_requests_file_name),
          row.names = FALSE)

ktc_6_request_file_name <- sprintf(paste0("output/",
                                        sy_abbreviation,
                                        " Files/ktc/ktc_6_",
                                        "_%s.csv"),
                                 today())

write.csv(ktc_students_6th_file %>%
          as.data.frame(),
          file = here::here(ktc_6_request_file_name),
          row.names = FALSE)

# 8. ------------------ ### Write File for Retention Data ### ---------

retention_file_name <- sprintf(paste0("output/",
                                        sy_abbreviation,
                                        " Files/retention/failing_or_below15_",
                                        rc_quarter,
                                        "_%s.csv"),
                                 today())

write_csv(failing_or_below15, 
          file = here::here(retention_file_name),
          row.names = FALSE)

# 9. ------------------ ### Write Google Sheets file for 8th Grade Promotion Data ### ---------

wb <- createWorkbook()

addWorksheet(wb, sheet = "KAMS")
addWorksheet(wb, sheet = "KBCP")
addWorksheet(wb, sheet = "KAC")
addWorksheet(wb, sheet = "KOA")

kams_final <- KAMS_8th_promotion_data %>% 
  as.data.frame() 
writeData(wb, sheet = "KAMS", kams_final)

kbcp_final <- KBCP_8th_promotion_data %>% 
  as.data.frame() 
writeData(wb, sheet = "KBCP", kbcp_final)

kac_final <- KAC_8th_promotion_data %>% 
  as.data.frame()
writeData(wb, sheet = "KAC", kac_final)

koa_final <- KOA_8th_promotion_data %>% 
  as.data.frame()
writeData(wb, sheet = "KAC", koa_final)

saveWorkbook(wb, 
             paste0(sy_abbreviation, "_Promotions_GPA.xlsx"),
             overwrite = TRUE)

