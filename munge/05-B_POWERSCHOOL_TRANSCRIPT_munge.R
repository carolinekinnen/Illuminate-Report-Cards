

# PARAMETERS --------------------------------------------------------------

STORE_CODE = "Q1"

#-------------------------- ### Grades for Powerschool Transcripts ###----------------------------------

# Quarter Grades - do not need to calculate for Illuminate but need it to upload to Powerschool
# only contains the current quarter's letter grades from Illuminate report cards so run once at the end of each quarter

ps_upload_quarter <- quarter_grades %>%
  dplyr::rename(student_number = student_id,
         StoreCode = store_code,
         Course_Name = course_long_name) %>%
  mutate(schoolname = str_c("KIPP ", schoolname)) %>%
  mutate(excludefromgpa = 0, 
         earnedcrhrs = 1.00,
         potentialcrhrs = 1.00,
         gpa_addedvalue = 0,
         gradescale_name = "default") %>%
  filter(!is.na(Course_Name)) %>%
  mutate(schoolname = case_when(
    course_school == "KBCP" ~ "KIPP Bloom College Prep",
    TRUE ~ schoolname
  )) %>%
  mutate(subject =case_when(
    subject == "social" ~ "social_studies",
    TRUE ~ subject
  ))


# Final Percentage
ps_upload_percentage <- final_percents %>%     
  dplyr::rename(student_number = student_id) %>% 
  left_join(ps_upload_quarter %>% select(-c(grade, percent, StoreCode)), 
            by = c("site_id", "student_number", "subject")) %>%
  mutate(StoreCode = STORE_CODE) %>%
  mutate(excludefromgpa = 0, 
         earnedcrhrs = 1.00,
         potentialcrhrs = 1.00,
         gpa_addedvalue = 0,
         gradescale_name = "default") %>%
  filter(!is.na(Course_Name)) %>%
  mutate(schoolname = case_when(
    course_school == "KBCP" ~ "KIPP Bloom College Prep",
    TRUE ~ schoolname
  ))

# Final Letter Grade
ps_upload_letter_grade <- final_grades %>% #final_grades %>%
  ungroup(site_id) %>%
  select(student_number = student_id,
         grade, 
         subject)

# Combined Final Table

ps_upload_final <- ps_upload_percentage %>% 
  left_join(ps_upload_letter_grade, by = c("student_number", "subject")) %>% 
  filter(grade_level > 4) %>%
  
  # need to change so heading will match what is tied to site_id in Powerschool
  mutate(schoolname = case_when(
    schoolname == "KIPP Ascend Middle" ~ "KIPP Ascend Middle School",
    TRUE ~ schoolname
  )) %>%
  filter(store_code == STORE_CODE) %>%
  mutate(subject = if_else(subject == "lit centers", "lit_centers", subject)) %>%
  rename(grade_org = grade, 
         percent_org = percent) %>%
  left_join(
    final_percent_grades %>% ungroup(site_id) %>% select(-c(site_id, store_code)), 
    by = c("student_number" = "student_id", 
           "subject")
  ) %>%
  select(-c(grade_org, percent_org)) %>%
  mutate(percent = str_remove(percent, "%"))
  

# fix student grades
  

# fix science for DL students

# ps_upload_final <- ps_upload_final %>%
#   mutate(grade = case_when(
#     student_number == 50307144 & subject == "science" ~ "B",
#     student_number == 50222630 & subject == "science" ~ "B",
#     student_number == 50244465 & subject == "science" ~ "B",
#     student_number == 50222840 & subject == "science" ~ "B",
#     student_number == 50255559 & subject == "science" ~ "C",
#     student_number == 50339951 & subject == "science" ~ "B",
#     student_number ==50320263 & subject == "science" ~ "B",
#     student_number == 50253158 & subject == "science" ~ "C", 
#     student_number == 50129792 & subject == "science" ~ "C", 
#     student_number == 50150164  & subject == "science" ~ "C", 
#     student_number ==  50231442 & subject == "science" ~ "C", 
#     student_number ==  50192084 & subject == "science" ~ "B",
#     student_number ==  50253861 & subject == "science" ~ "C",
#     student_number ==  50160150 & subject == "science" ~ "C",
#     student_number == 50179239 & subject == "science" ~ "B",
#     student_number ==  50303608 & subject == "science" ~ "B",
#     student_number == 50075471  & subject == "science" ~ "C",
#     student_number ==  50204542 & subject == "science" ~ "B",
#     student_number ==  50408898 & subject == "science" ~ "C",
#     student_number == 50232472 & subject == "science" ~ "B",
#     student_number == 50075462 & subject == "science" ~ "B",
#     student_number ==  50125880 & subject == "science" ~ "C",
#     student_number ==  50110514 & subject == "science" ~ "B",
#     student_number ==  50150163 & subject == "science" ~ "C",
#     student_number ==  50176273 & subject == "science" ~ "B",
#     student_number ==  50229624 & subject == "science" ~ "B",
#     student_number == 50177535 & subject == "science" ~ "C",
#     student_number == 50150187 & subject == "science" ~ "C+",
#     student_number ==  50465034 & subject == "science" ~ "C+",
#     student_number ==  50157318 & subject == "science" ~ "C",
#     student_number ==  32644805 & subject == "science" ~ "B-",
#     student_number ==  50298693 & subject == "science" ~ "A-",
#     student_number ==  50199284 & subject == "science" ~ "A",
#     student_number ==  50283026 & subject == "science" ~ "B+",
#     student_number ==  50237182 & subject == "science" ~ "B",
#     TRUE ~ grade
#   ))

# In Powerschool:
# 
# Special Functions -> Importing & Exporting -> Quick Import -> Historical Grades
# 
# Check if it updated by searching a student then clicking on Historical Grades in the left side menu
# 
# 
# Editing Transcripts:
#   
#   Start Page > Reports > Report Setup > Object Reports
# 
# Attendance dates needs to be updated yearly in GPA and Attendance Field


# Remember to ask EL Case Managers for list of students who earned CPS Bilingual Seal Award then upload an X
# to Powerschool in the fields students table -> elementary_seal or middle_seal


# fix final grades for KTC

# koa_fix_function <- function(grade_num) {
#   file <- ps_upload_final %>%
#     filter(site_id == 400180) %>%
#     filter(grade_level == grade_num) %>%
#     select(student_number, subject, grade, percent, grade_level) %>%
#     pivot_wider(names_from = subject,
#                 values_from = c(grade, percent)) %>%
#     left_join(students %>% select(student_number, first_name, last_name), by = "student_number") %>%
#     ungroup() %>%
#     select(-site_id) %>% 
#     unnest(cols = c(grade_ela, grade_math, grade_science, grade_social_studies, 
#                     percent_ela, percent_math, percent_science, percent_social_studies))
#   
#   write_csv(x = file, path = paste0("~/Downloads/Q4_request_",
#                                     grade_num,
#                                     "_koa_fix.csv"))  
#   
# }
# 
# koa_fix_function(5)
# koa_fix_function(6)
# koa_fix_function(7)
# koa_fix_function(8)
# 
# 
# kbcp_fix_function <- function(grade_num) {
#   file <- ps_upload_final %>%
#     filter(site_id == 400163) %>%
#      filter(grade_level == grade_num) %>%
#     select(student_number, subject, grade, percent, grade_level) %>%
#     pivot_wider(names_from = subject,
#                 values_from = c(grade, percent)) %>%
#     left_join(students %>% select(student_number, first_name, last_name), by = "student_number") %>%
#     ungroup() %>%
#     select(-site_id)
#   
#   write_csv(x = file, path = paste0("~/Downloads/Q4_request_",
#                                                grade_num,
#                                                "_kbcp_fix.csv"))  
#   
# }
# 
# kbcp_fix_function(5)
# kbcp_fix_function(7)
# kbcp_fix_function(8)
# 
#   
# kbcp_grades_fix <- ps_upload_final %>%
#   filter(site_id == 400163) %>%
#   filter(grade_level == 6) %>%
#   select(student_number, subject, grade, percent, grade_level) %>%
#   pivot_wider(names_from = subject,
#               values_from = c(grade, percent)) %>%
#   unnest(cols = c(grade_ela, `grade_lit centers`, grade_math, grade_science, 
#                   percent_ela, `percent_lit centers`, percent_math, percent_science)) %>%
#   left_join(students %>% select(student_number, first_name, last_name), by = "student_number")
# 
# write_csv(x = kbcp_grades_fix, path = "~/Downloads/Q4_request_6_kbcp_fix.csv")  
# 
# 
# kams_fix_function <- function(grade_num) {
#   file <- ps_upload_final %>%
#     filter(site_id == 7810) %>%
#     filter(grade_level == grade_num) %>%
#     select(student_number, subject, grade, percent, grade_level) %>%
#     pivot_wider(names_from = subject,
#                 values_from = c(grade, percent)) %>%
#     left_join(students %>% select(student_number, first_name, last_name), by = "student_number") %>%
#     ungroup() %>%
#     select(-site_id) %>% 
#     unnest() %>%
#     mutate(gpa_ela = case_when(
#       str_detect(grade_ela, "A") ~ 4,
#       str_detect(grade_ela, "B") ~ 3,
#       str_detect(grade_ela, "C") ~ 2,
#       str_detect(grade_ela, "D") ~ 1,
#       str_detect(grade_ela, "F") ~ 0),
#       gpa_social = case_when(
#         str_detect(grade_social_studies, "A") ~ 4,
#         str_detect(grade_social_studies, "B") ~ 3,
#         str_detect(grade_social_studies, "C") ~ 2,
#         str_detect(grade_social_studies, "D") ~ 1,
#         str_detect(grade_social_studies, "F") ~ 0),
#       gpa_math = case_when(
#         str_detect(grade_math, "A") ~ 4,
#         str_detect(grade_math, "B") ~ 3,
#         str_detect(grade_math, "C") ~ 2,
#         str_detect(grade_math, "D") ~ 1,
#         str_detect(grade_math, "F") ~ 0),
#       gpa_science = case_when(
#         str_detect(grade_science, "A") ~ 4,
#         str_detect(grade_science, "B") ~ 3,
#         str_detect(grade_science, "C") ~ 2,
#         str_detect(grade_science, "D") ~ 1,
#         str_detect(grade_science, "F") ~ 0)
#       )
#   
#   write_csv(x = file, path = paste0("~/Downloads/Q4_request_",
#                                     grade_num,
#                                     "_kams_fix_gpas.csv"))  
#   
# }
# 
# kams_fix_function(5)
# kams_fix_function(6)
# kams_fix_function(7)
# kams_fix_function(8)
# 
# 
# 
# isbe_grades <- ps_upload_final %>% 
#   select(site_id,
#          student_number,
#          subject,
#          grade,
#          percent,
#          Course_Name, 
#          teacher_full_name,
#          course_number,
#          grade_level,
#          home_room,
#          student_first,
#          student_last) %>%
#   ungroup() %>%
#   add_row(site_id = 7810,
#           student_number = 50043857,
#           subject = "math",
#           grade = "B-",
#           percent = 80,
#           Course_Name = "8th Mathematics",
#           teacher_full_name = "Alexus Williams",
#           course_number = "kams8math",
#           grade_level = 8,
#           home_room = "8th University of Illinois")

# write_csv(x = isbe_grades, path = "~/Downloads/powerschool_upload_final_grades.csv")

