
#-------------------------- ### Illuminate Report Card Data ###-------------------------------------

# Report card files (one per grade per school for grades 3-8. No primary because they
# don't calculate final grade) need to be exported from Illuminate,
# unzipped, and moved into GCS Storage/raw_data_storage/Illuminate-Report-Cards/Illuminate-Grades/SY/

# # Map get_objects function onto each item of the lists in order to move files from GCS bucket into local directory
# map2(
#   .x = schools_map,
#   .y = grades_map,
#   .f = ~ get_objects_report_card(.x, .y)
# )
# 
# # # Identifying the files that were just moved into the local directory
# file_list <- dir(
#   path = here::here("data/flatfiles/rc_export/"),
#   pattern = "\\.csv", full.names = TRUE
# )
# 
# # Read and save files from local directory as dataframes (one per school per grade) to a list
# grade_df_list <- file_list %>%
#   map(read_csv) %>%
#   map(clean_names)
# 
# # Data frame of data frames
# file_list_short_names <- dir(
#   path = here::here("data/flatfiles/rc_export/"),
#   pattern = "\\.csv", full.names = FALSE
# )
# 
# grade_df_df <- tibble(
#   file_name = file_list_short_names,
#   df = grade_df_list
# ) %>%
#   # removed for 20-21 because grade level no longer exists, not sure why it's still showing up
#   filter(file_name != "KACP_4.csv") %>%
#   # removed for 20-21 because not calculating final grades for 3rd grade in Q1, schools might request it later
#   filter(file_name != "KAP_3.csv" & file_name != "KOP_3.csv")
# 
# 
# dl_gradebook <- 
#   read_csv(here::here("data", "flatfiles", 
#                       "DL_roster_links", 
#                       "20-21_dl_roster_gb_name.csv"))



# Illuminate Report Card Data - NEW SOURCE --------------------------------

gcs_global_bucket("raw_data_storage")

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
               saveToDisk = "data/flatfiles/ps_upload_final_grades_Q1_2021.csv",
               overwrite = TRUE
)

previous_quarter_grades_q1 <- 
  read_csv(here::here("data", 
                      "flatfiles", 
                      "ps_upload_final_grades_Q1_2021.csv")
           )

# Temporary fix for courses file ------------------------------------------

# # this file should be pulled from bq
# cc <- read_csv(here::here("data", "flatfiles", "cc.csv"))
# 
# gcs_get_object("Illuminate-Report-Cards/DL_roster_links/2021_q1_modified_grade_scale_students.csv",
#                  saveToDisk = "data/flatfiles/2021_q1_modified_grade_scale_students.csv",
#                  overwrite = TRUE
#   )
# 
# dl_mod_grades_students <-
#   read_csv(here::here("data", "flatfiles", "2021_q1_modified_grade_scale_students.csv"))

