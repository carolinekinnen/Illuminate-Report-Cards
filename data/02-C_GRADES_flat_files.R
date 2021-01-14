
# Credentials -------------------------------------------------------------

googleAuthR::gar_auth_service("/usr/local/etc/gcs/kipp-chicago-silo-2-3789ce3e3415.json")
gcs_global_bucket("raw_data_storage")


# Illuminate GradeBook Data -----------------------------------------------

gcs_get_object("Illuminate-Report-Cards/Illuminate-Grades/20-21/2021_q2_kipp_chicago_student_grades.csv",
               saveToDisk = "data/flatfiles/raw_grade_export_illuminate/2021_q2_kipp_chicago_student_grades.csv",
               overwrite = TRUE
)

kc_grades_full <- 
  read_csv(here::here("data", 
                      "flatfiles", 
                      "raw_grade_export_illuminate",
                      "2021_q2_kipp_chicago_student_grades.csv")
           )


# Previous Quarter Grades -------------------------------------------------

gcs_global_bucket("quarterly_final_grades")

gcs_get_object("20-21/ps_upload_final_grades_Q1_2021.csv",
               saveToDisk = "data/flatfiles/previous_quarter_grades/ps_upload_final_grades_Q1_2021.csv",
               overwrite = TRUE
)

previous_quarter_grades_q1 <- 
  read_csv(here::here("data", 
                      "flatfiles", 
                      "previous_quarter_grades",
                      "ps_upload_final_grades_Q1_2021.csv")
           )