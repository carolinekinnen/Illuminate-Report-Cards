library('ProjectTemplate')
load.project()


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
         gpa_added_value = 0,
         grade_scale_name = "default",
         termid = year_term_id) %>%
  filter(!is.na(Course_Name)) %>%
  mutate(schoolname = case_when(
    course_school == "KBCP" ~ "KIPP Bloom College Prep",
    TRUE ~ schoolname
  ))


# Final Percentage

ps_upload_percentage <- final_percents %>%     
  dplyr::rename(student_number = student_id) %>%
  left_join(ps_upload_quarter %>% select(-c(grade, percent, StoreCode)), by = c("site_id", "student_number", "subject")) %>%
  mutate(StoreCode = "Y1") %>%
  mutate(excludefromgpa = 0, 
         earnedcrhrs = 1.00,
         potentialcrhrs = 1.00,
         gpa_added_value = 0,
         grade_scale_name = "default",
         termid = year_term_id) %>%
  filter(!is.na(Course_Name)) %>%
  mutate(schoolname = case_when(
    course_school == "KBCP" ~ "KIPP Bloom College Prep",
    TRUE ~ schoolname
  )) 

# Final Letter Grade

ps_upload_letter_grade <- final_grades %>%
  select(student_number = student_id,
         grade, 
         subject)

# Combined Final Table

ps_upload_final <- ps_upload_percentage %>%
  left_join(ps_upload_letter_grade, by = c("student_number", "subject"))

