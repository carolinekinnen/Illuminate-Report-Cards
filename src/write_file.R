
library(ProjectTemplate)
load.project()

source(here::here("src", "category_comments.R"))
source(here::here("src", "course_teacher_name.R"))
source(here::here("src", "deanslist_upload.R"))
source(here::here("src", "final_grades.R"))
source(here::here("src", "KTC_data.R"))
source(here::here("src", "powerschool_transcript_upload.R"))
source(here::here("src", "quarter_number.R"))
source(here::here("src", "retention_data.R"))


# --------------------- ### Write Quarter Number Files for Illuminate ### ------------------
# Only write and upload once a year

write.csv(quarter_number %>%
            as.data.frame(),
          file = here::here("/19-20 Files/quarter_numbers/quarter_numbers.csv"), 
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

write.csv(final_grades_gpa_illuminate_upload %>%
          as.data.frame(),
          file = here::here(final_grades_file),
          row.names = FALSE)


# 4. --------------------- ### Write Transcript File for Powerschool ### ------------------
# Write transcript file after grades are finalized 

ps_upload_file_name <- sprintf(paste0("/19-20 Files/transcripts/ps_upload_",
                                      rc_quarter,
                                      "_%s.csv"),
                               today())

write.csv(ps_upload_quarter %>%
            as.data.frame(),
          file = "~/Downloads/ps_upload_quarter2_file.csv",
          # file = here::here(ps_upload_file_name),
          row.names = FALSE)

write.csv(ps_upload_final %>%
            as.data.frame(),
          file = "~/Downloads/ps_upload_final_file.csv",
          # file = here::here(ps_upload_file_name),
          row.names = FALSE)

# 5. --------------------- ### Write Quarter Grades File for Deans List ### ------------------
# Write after grades are finalized 

dl_upload_file_name_input <- function(schoolname) {

  file <- sprintf(paste0(#"/19-20 Files/deanslist/deanslist_upload_",
    "~/Downloads/",
                         schoolname, 
                         "_",
                         rc_quarter,
                         "_%s.csv"),
                  today())
}

write.csv(dl_upload_kac %>%
            as.data.frame(),
          file = dl_upload_file_name_input("KBCP"),
          row.names = FALSE)

write.csv(dl_upload_kams %>%
            as.data.frame(),
          file = dl_upload_file_name_input("KBCP"),
          row.names = FALSE)

write.csv(dl_upload_kap %>%
            as.data.frame(),
          file = dl_upload_file_name_input("KBCP"),
          row.names = FALSE)

write.csv(dl_upload_kbcp %>%
            as.data.frame(),
          file = dl_upload_file_name_input("KBCP"),
          row.names = FALSE)

write.csv(dl_upload_one %>%
            as.data.frame(),
          file = dl_upload_file_name_input("KBCP"),
          row.names = FALSE)

# 6. --------------------- ### Write Final Grades File for GCS Bucket ### ------------------
# Needs to be saved in GCS to be pulled into Load GPAs code for On-Track Snapshots

write.csv(ps_upload_final %>%
          as.data.frame(),
          file = "~/Downloads/ps_upload_final_file.csv",
          # file = here::here(ps_upload_file_name),
          row.names = FALSE)

# 7. --------------------#### Write Category Comments #### -------------------------
# Not writing yet because Maggie hasn't confirmed she's ok with plan
# also need to figure out Spanish


# 8. --------------------- ### Write File for KTC ### ------------------

write.csv(ktc_7_request %>% as.data.frame(),
          #file = here::here(ktc_7_request.csv"),
          file = "~/Downloads/ktc_7_request.csv",
          row.names = FALSE)

# 9. ------------------ ### Write File for Retention Data ### ---------

write_csv(failing_or_below15, 
          #file = here::here(failing_or_below15.csv"),
          file = "~/Downloads/failing_or_below15.csv",
          row.names = FALSE)



