
library('ProjectTemplate')
load.project()

#-------------------------- ### Grades for Deans List Progress Reports ###----------------------------------

dl_upload <- quarter_grades %>%
  left_join(course_names_teachers %>% select(teacher_full_name, 
                                             student_id = student_number,
                                             course_long_name), 
            by = c("student_id", "course_long_name")) %>% 
  mutate(teacher_last_name = word(teacher_full_name, -1)) %>% 
   left_join(course_section %>% 
               dplyr::rename(student_id = student_number), by = c("student_id", "course_long_name")) %>% 
 left_join(dl_rosters,
            by = c("sectionid" = "sec_id")) %>% 
  left_join(student_enroll %>% 
              select(student_number,
                     schoolid),
            by = c("student_id" = "student_number")) %>% 
  filter(site_id == schoolid, 
         !is.na(gb_name),
         !is.na(grade)) %>% 
  ungroup() %>% 
  mutate(score_last_updated = as.Date(rc_quarter_last_day),
         grading_period = str_c(as.Date(rc_quarter_first_day), as.Date(rc_quarter_last_day), sep = " - ")) %>%
  select(site_id,
         `Student ID` = student_id, 
         last_name,
         first_name,
         teacher_full_name,
         GradeBook = gb_name,
         score_last_updated,
         Percentage = percent,
         Mark = grade,
         grading_period)

dl_upload_kac <- dl_upload %>%
  filter(site_id == 400146)

dl_upload_kacp <- dl_upload %>%
  filter(site_id == 4001462)

dl_upload_kams <- dl_upload %>%
  filter(site_id == 7810)

dl_upload_kbcp <- dl_upload %>%
  filter(site_id == 400163)

dl_upload_koa <- dl_upload %>%
  filter(site_id == 400180)



