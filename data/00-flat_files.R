
#-------------------------- ### Illuminate Report Card Data ###-------------------------------------

# Report card files (one per grade per school for grades 3-8. No primary because they
# don't calculate final grade) need to be exported from Illuminate, 
# unzipped, and moved into GCS Storage/raw_data_storage/Illuminate-Report-Cards/Illuminate-Grades/SY/

gcs_global_bucket("raw_data_storage")

# Map get_objects function onto each item of the lists in order to move files from GCS bucket into local directory
map2(.x = schools_map,
     .y = grades_map,
     .f = ~get_objects_report_card(.x, .y))

# # Identifying the files that were just moved into the local directory
file_list <- dir(path = here::here("data/flatfiles/rc_export/"),
                 pattern = "\\.csv", full.names = TRUE)

# Read and save files from local directory as dataframes (one per school per grade) to a list
grade_df_list <- file_list %>%
  map(read_csv) %>%
  map(clean_names)

# Data frame of data frames
file_list_short_names <- dir(path = here::here("data/flatfiles/rc_export/"), 
                             pattern = "\\.csv", full.names = FALSE)

grade_df_df <- data_frame(
  file_name = file_list_short_names,
  df = grade_df_list
) %>%
  filter(file_name != "KACP_4.csv") %>%  # removed for 20-21 because grade level no longer exists, not sure why it's still showing up
  filter(file_name != "KAP_3.csv" & file_name != "KOP_3.csv") # removed for 20-21 because not calculating final grades for 3rd grade in Q1, schools might request it later


# ------------------------- ### DL Students with Modified Grading Scale ### --------------------------------
# Has to be updated once each year
# Commented out for now because get_y_avg function does not need to know who DL students are
# testing 19-20 Q3 and 20-21 Q1

# # KAMS 
# kams_title <- drive_find("KAMS Modified Grading: 19-20SY", n_max = 30)
# 
# kams_mod_grades <- bind_rows(ws(kams_title, "5th"), ws(kams_title, "6th"), 
#                              ws(kams_title, "7th"), ws(kams_title, "8th"))
# 
# # KBCP 
# kbcp_mod_grades <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/17GAPma7f1e0EsXlphbeDFq7_fhK_1F5AQ7vdkH9DusE/edit#gid=0") %>%
#   select(2) %>%
#   clean_names()
# 
# # KACP
# kacp_mod_grades <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1SwQhyioO9fULIz4gj_dhJ1_rktLiZHYzbqgsqkZ-Ov4/edit?ts=5d926196#gid=1171959828") %>%
#   select(2) %>%
#   clean_names()
# 
# # KAC
# kac_title <- drive_find("KAC Modified Grading: 19-20SY", n_max = 30)
# 
# kac_mod_grades <- bind_rows(ws(kac_title, "5th"), ws(kac_title, "6th"),
#                             ws(kac_title, "7th"), ws(kac_title, "8th"))
# 
# # All
# # Note: don't bring in KOP or KAP because no GPA
# 
# all_mod_grades <- bind_rows(kams_mod_grades,
#                             kbcp_mod_grades,
#                             kacp_mod_grades,
#                             kac_mod_grades) %>%
#   filter(!is.na(id_number))


#----------------------- ### SIS Roster Links from Deans List ###---------------------------
# Has to be updated once each year OR if new gradebooks are added
# Only KAC, KACP, KAMS, KBCP, and KOA need to have updated final quarter grades on Deans List

# map(.x = c("KAC", "KACP", "KAMS", "KBCP", "KOA"),
#     .f = get_objects_roster_links)
# 
# # Gradebook names that link subjects to grades, needs to be downloaded from Deans List
# sis_roster_links <- dir(path = here::here("data/flatfiles/DL_roster_links"), 
#                         pattern = "19-20_roster_links", full.names = TRUE)
# 
# dl_rosters <- sis_roster_links %>%
#   map_df(read_csv) %>%
#   clean_names() %>% 
#   filter(!is.na(gradebook_name_at_load)) %>% 
#   select(sec_id = secondary_integration_id_at_load,
#          gb_name = gradebook_name_at_load)

#----------------- ### Attendance and Final Grade Data from Last Year ###-------------------
# KTC needs grade and attendance data from 5th grade for current 6th graders
# files can be found in output directories



last_year_grades <- read.csv(paste0(here::here(), "/output/",
                                    sy_abbreviation_last_year,
                                    " Files/grades/SY",
                                    sy_abbreviation_last_year,
                                    "_Q4_GPAS_Final_Grades_",
                                    as.Date(terms_last_year$lastday),
                                    ".csv"))

# last_year_grades_sped <- read.csv(paste0(here::here(), "/output/",
#                                     sy_abbreviation_last_year,
#                                     " Files/grades/SY",
#                                     sy_abbreviation_last_year,
#                                     "_Q4_SpEd_GPAS_",
#                                     as.Date(terms_last_year$lastday),
#                                     ".csv"))
# only has GPAs, need letter grades

# last_year_attendance <- read.csv(paste0(here::here(), "/output/",
#                                         sy_abbreviation_last_year,
#                                         " Files/attendance/SY",
#                                         sy_abbreviation_last_year,
#                                         " Q4 RC Attendance 2019-07-16.csv"))
# date will need to change for SY 20-21
                                 


cc <- read_csv(here::here("data", "flatfiles", "cc.csv"))

