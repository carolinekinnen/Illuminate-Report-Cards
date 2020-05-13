
# -------------------------------- ### 8th Grade Promotion ### --------------

# Need students' GPAs from 5th grade to 8th grade. 4 GPAs are averaged and sorted to find Valedictorian 

# -------------------------------- ### Tables ### --------------

eighth_students <- students %>%
  filter(grade_level == 8,
         enroll_status == 0) %>% 
  select(studentid = id, 
         student_number,
         first_name, 
         last_name, 
         grade_level,
         schoolid)

# Flat files loaded from output

grade_8_gpa_gen <- read.csv(here("output/19-20 Files/grades/SY19-20_Q2_GPAS_Final_Grades_2020-02-04.csv"))

grade_8_gpa_sped <- read.csv(here("output/19-20 Files/grades/SY19-20_Q2_SpEd_GPAS_2020-02-18.csv"))

# -------------------------------- ### Parameters ### --------------

# calculates terms current 8th graders were in 5th grade

current_first_year <- calc_academic_year(today(),
                                         format = "first_year")

current_last_year <- calc_academic_year(today(), 
                                        format = "second_year")

termids <- tribble(
  ~sy, ~termid, ~grade_level_for_8th_students,
 paste0(current_first_year, 
        "-",
        current_last_year), calc_ps_termid(current_first_year), 8,
 paste0(current_first_year - 1,
        "-",
        current_last_year - 1), calc_ps_termid(current_first_year - 1), 7,
 paste0(current_first_year - 2,
        "-",
        current_last_year - 2), calc_ps_termid(current_first_year - 2), 6,
 paste0(current_first_year - 3,
        "-",
        current_last_year - 3), calc_ps_termid(current_first_year - 3), 5)

# -------------------------------- ### Combining Tables ### --------------

storedgrades_8th <- storedgrades %>% 
  filter(studentid %in% eighth_students$studentid,
         termid %in% termids$termid, 
         storecode == "Y1")

storedgpas_8th <- storedgrades_8th %>% 
  mutate(grade = if_else(grade == "false", "F", grade)) %>%
  left_join(grade_scale, by = "grade") %>%                        # We changed our GPA scale in SY19-20. Old scale can be found in manual table file, but all transcript grades reflect the new scale 
  group_by(studentid, termid, schoolid) %>%
  summarize(year_gpa = mean(fixscale_gpa_points, na.rm = TRUE)) %>%
  mutate(grade_level = case_when(
    termid == calc_ps_termid(current_first_year - 1) ~ 7,
    termid == calc_ps_termid(current_first_year - 2) ~ 6,
    termid == calc_ps_termid(current_first_year - 3) ~ 5,
    TRUE ~ 0
  )) 

non_storedgrades_8th <- pgfinalgrades %>%
  filter(studentid %in% eighth_students$studentid,
         finalgradename %in% "Y1") %>%
  mutate(termid = case_when(
    str_detect(as.character(startdate), as.character(current_first_year)) ~ calc_ps_termid(current_first_year),
    str_detect(as.character(startdate), as.character(current_first_year - 1)) ~ calc_ps_termid(current_first_year - 1),
    str_detect(as.character(startdate), as.character(current_first_year - 2)) ~ calc_ps_termid(current_first_year - 2),
    str_detect(as.character(startdate), as.character(current_first_year - 3)) ~ calc_ps_termid(current_first_year - 3),
  )) %>% 
  filter(termid %in% termids$termid)
# not all students were included in stored grades table due to KCCP switching to KAC.
# Unclear if this table will be needed in SY 20-21

non_storedgpas_8th <- non_storedgrades_8th %>%
  mutate(grade = if_else(grade == "false", "F", grade)) %>%
  left_join(grade_scale, by = "grade") %>%                       
  group_by(studentid, termid, #schoolid   # pgfinalgrades table doesn't include school identifiers
           ) %>%
  summarize(year_gpa = mean(fixscale_gpa_points, na.rm = TRUE)) %>%
  mutate(grade_level = case_when(
    termid == calc_ps_termid(current_first_year - 1) ~ 7,
    termid == calc_ps_termid(current_first_year - 2) ~ 6,
    termid == calc_ps_termid(current_first_year - 3) ~ 5,
    TRUE ~ 0)) %>%
  anti_join(storedgpas_8th, by = c("studentid", "termid", "year_gpa")) %>%
  left_join(students %>% select(studentid = id, schoolid), by = "studentid")

gpa_pivot <- storedgpas_8th %>%
  bind_rows(non_storedgpas_8th) %>%
  ungroup() %>%
  select(#student_number, 
         #first_name, 
         #last_name, 
         studentid,
         grade_level, 
         site_id = schoolid,
         year_gpa) %>%
  pivot_wider(names_from = "grade_level", values_from = "year_gpa") %>%
  left_join(students %>% select(studentid = id, student_number), 
            by = "studentid")

current_gpa_8th <- grade_8_gpa_gen %>%
  rename(student_number = student_id) %>%
  select(student_number, gpa, site_id) %>%
  bind_rows(grade_8_gpa_sped %>%
              rename(student_number = student_id) %>%
              select(student_number, gpa, site_id)) %>%
  rename("8" = gpa)
  
all_gpas <- current_gpa_8th %>%
  left_join(gpa_pivot, by = c("student_number", "site_id")) %>% 
  left_join(students %>% select(student_number, first_name, last_name, grade_level), 
            by = "student_number") %>%
  filter(grade_level == 8) %>%
  left_join(schools, by = c("site_id" = "schoolid")) %>%
  select(schoolabbrev, studentid, student_number, first_name, last_name, `5`, `6`, `7`, `8`)


all_gpas %>% count(student_number, first_name, last_name) %>% filter(n > 1)
# use to find out transfer students, need to move data to current school

KAC_8th_promotion_data <- all_gpas %>% filter(schoolabbrev == "KAC")

KAMS_8th_promotion_data <- all_gpas %>% filter(schoolabbrev == "KAMS")

KBCP_8th_promotion_data <- all_gpas %>% filter(schoolabbrev == "KBCP")

KOA_8th_promotion_data <- all_gpas %>% filter(schoolabbrev == "KOA")

# ------------------ ###  Old Code, might need for reference ### -------------

# section_course <- cc %>%
#   select(sectionid,
#          course_number) %>%
#   mutate(sectionid = abs(sectionid)) %>%
#   unique()
# 
# y1_grades_2600 <- pgfinalgrades %>%
#   filter(studentid %in% eighth_students$studentid,
#          finalgradename %in% "Y1") %>%
#   mutate(grade = if_else(grade == "false", "F", grade)) %>%
#   left_join(section_course,
#             by = "sectionid") %>% 
#   filter(!is.na(grade),
#          !grepl("--", grade)) 
# 
# y1_grades_2500_2700 <- storedgrades %>% #ADD datestored!!
#   filter(studentid %in% eighth_students$studentid,
#          termid >= 2600) %>%   # change
#   select(studentid,
#          course_number,
#          sectionid,
#          storecode,
#          datestored,
#          grade, 
#          grade_level, 
#          termid) %>%
#   filter(storecode %in% "Y1") %>%
#   mutate(grade = if_else(grade == "false", "F", grade))
# 
# y1_5th_7th <- y1_grades_2500_2700 %>%
#   filter(!is.na(grade)) %>%
#   left_join(grade_scale,
#             by = "grade") %>%
#   group_by(studentid,
#            course_number,
#            termid,
#            datestored) %>%
#   filter(datestored == max(datestored),
#          !grepl("att", course_number)) #%>% filter(studentid %in% 13685)
# 
# # left_join(eigth_students %>%
# #             select(studentid,
# #                    schoolid),
# #           by = "studentid") %>%
# # left_join(schools %>%
# #             mutate(schoolabbrev = tolower(schoolabbrev)),
# #           by = "schoolid") %>%
# # mutate(grad_school = )
# 
# # corrections <- y1_5th %>%
# #   filter(studentid %in% c(13685, 13752)) %>%
# #   mutate(school = "kbcp") %>%
# #   filter(grepl("kbcp", course_number))
# 
# 
# gpa_5 <- y1_5th_7th %>%
#   filter(termid == 2600) %>%    # change
#   group_by(termid, 
#            grade_level, 
#            studentid) %>% 
#   summarise(grades_used = paste(grade, collapse = " "),
#             n_grades = n(),
#             gpa = mean(points),
#             n_courses = paste(course_number, collapse = " ")) 
# #n_grades less than 4, 14106, 15096, 15102, 12817
# 
# # pgfinalgrades %>%
# #   filter(studentid %in% eigth_students$studentid,
# #          finalgradename %in% "Y1",
# #          !grepl("--", grade)) %>% #select(grade) %>% table()
# #   mutate(grade = if_else(grade == "false", "F", grade)) %>%
# #   select(-points) %>%
# #   left_join(grade_scale,
# #             by = "grade") %>%
# #   group_by(enddate, 
# #            #grade_level, 
# #            studentid) %>%
# #   summarise(grades_used = paste(grade, collapse = " "),
# #             n_grades = n(),
# #             gpa = mean(points)
# #             #courses = paste(course_number, collapse = " ")
# #             ) %>% filter(n_grades > 5 | n_grades < 4) 
# 
# gpa_6th <- pgfinalgrades %>%
#   filter(studentid %in% eighth_students$studentid,
#          finalgradename %in% "Y1",
#          !grepl("--", grade)) %>% 
#   mutate(grade = if_else(grade == "false", "F", grade),
#          grade_level = 6) %>%
#   left_join(section_course,
#             by = "sectionid") %>% #filter(!grepl("7", course_number)) ALL 7TH GRADE
#   # mutate(grade = if_else(studentid %in% 13752 & grepl("ela", course_number) & grade == "B+", NA_character_, grade),
#   #        grade = if_else(studentid %in% 13752 & grepl("math", course_number) & grade == "B", NA_character_, grade),
#   #        grade = if_else(studentid %in% 13905 & grepl("ela", course_number) & grade == "C", NA_character_, grade),
#   #        grade = if_else(studentid %in% 13905 & grepl("math", course_number) & grade == "C+", NA_character_, grade),
#   #        grade = if_else(studentid %in% 13936 & grepl("ela", course_number) & grade == "C", NA_character_, grade)) %>%
#   select(-points) %>%
#   left_join(grade_scale,
#             by = "grade") %>% 
#   filter(!is.na(grade)) %>% 
#   group_by(studentid,
#            course_number) %>%
#   filter(lastgradeupdate == max(lastgradeupdate)) %>%
#   group_by(enddate, 
#            grade_level, 
#            studentid) %>% 
#   summarise(grades_used = paste(grade, collapse = " "),
#             n_grades = n(),
#             gpa = mean(points),
#             n_courses = paste(course_number, collapse = " ")) #%>%
# #filter(n_grades > 5 | n_grades < 4) %>%
# # View()
# 
# gpa_7 <- y1_5th_7th %>%
#   filter(termid == 2800) %>%    # change
#   group_by(termid, 
#            grade_level, 
#            studentid) %>%
#   summarise(grades_used = paste(grade, collapse = " "),
#             n_grades = n(),
#             gpa = mean(points),
#             n_courses = paste(course_number, collapse = " ")) 
# 
# gpa_8th <- grade_8_gpa_gen %>%
#   as_tibble() %>% 
#   select(site_id,
#          student_id,
#          gpa,
#          #n_courses_Q1,
#          #letter_grades,
#          #points_used
#   ) %>%
#   bind_rows(grade_8_gpa_sped %>% 
#               as_tibble()) %>%
#   filter(student_id %in% eighth_students$student_number) 
# 
# 
# #next steps: combine all parts, spread (col names grade level), avg gpa all 4 years
# #save as one xlsx with tabs for each school 
# 
# 
# final_gpa_5th <- gpa_5 %>%
#   ungroup() %>%
#   #filter(grade_level == 5) %>%
#   select(-c(n_courses,
#             n_grades,
#             grades_used,
#             termid)) %>%
#   left_join(eighth_students %>%
#               select(schoolid,
#                      student_number,
#                      studentid),
#             by = "studentid") %>%
#   left_join(schools,
#             by = c("schoolid")) %>%
#   select(-c(studentid,
#             schoolid)) 
# 
# final_gpa_6th <- gpa_6th %>%
#   ungroup() %>%
#   #filter(grade_level == 6) %>%
#   select(-c(n_courses,
#             n_grades,
#             grades_used)) %>%
#   left_join(eighth_students %>%
#               select(schoolid,
#                      student_number,
#                      studentid),
#             by = "studentid") %>%
#   left_join(schools,
#             by = c("schoolid")) %>%
#   select(-c(studentid,
#             schoolid)) 
# 
# final_gpa_7th <- gpa_7 %>%
#   ungroup() %>%
#   mutate(grade_level = as.integer(7)) %>%
#   select(-c(grades_used,
#             n_grades,
#             n_courses)) %>%
#   left_join(eighth_students %>%
#               select(schoolid,
#                      student_number,
#                      studentid),
#             by = "studentid") %>%
#   left_join(schools,
#             by = c("schoolid")) %>%
#   select(-c(studentid,
#             schoolid)) 
# 
# 
# final_gpa_8th <- gpa_8th %>%
#   left_join(schools,
#             by = c("site_id" = "schoolid")) %>%
#   select(-c(n_courses,
#             letter_grades,
#             points_used)) %>%
#   mutate(grade_level = as.integer(8)) %>%
#   select(-site_id) %>%
#   rename(student_number = student_id)
# 
# all_grades <-  final_gpa_5th %>%
#   filter(!student_number == 50484614) %>% #retained, keeping 16-17 6th grade scores
#   bind_rows(final_gpa_6th,
#             final_gpa_7th,
#             final_gpa_8th) 
# 
# avg_gpa <- all_grades %>%
#   group_by(student_number) %>%
#   summarise(gpa = mean(gpa)) %>%
#   left_join(eighth_students %>%
#               select(schoolid,
#                      student_number),
#             by = "student_number") %>%
#   left_join(schools,
#             by = c("schoolid")) %>%
#   select(-schoolid) %>%
#   mutate(grade_level = "avg_gpa")
# 
# final_gpa_promotion <- all_grades %>%
#   mutate(grade_level = as.character(grade_level)) %>% 
#   bind_rows(avg_gpa) %>%
#   select(-c(enddate, 
#             termid)) %>% #View()
#   spread(grade_level, gpa) %>%
#   left_join(eighth_students %>%
#               select(student_number,
#                      first_name,
#                      last_name),
#             by = "student_number") %>%
#   select(student_number,
#          first_name,
#          last_name,
#          schoolabbrev,
#          `5`,
#          `6`,
#          `7`,
#          `8`,
#          avg_gpa) %>% 
#   replace(., is.na(.), "") 
# 
# wb <- createWorkbook()
# 
# addWorksheet(wb, sheet = "KAMS")
# addWorksheet(wb, sheet = "KBCP")
# addWorksheet(wb, sheet = "KAC")
# 
# kams_final <- final_gpa_promotion %>% 
#   filter(schoolabbrev == "KAMS") %>% 
#   as.data.frame() 
# writeData(wb, sheet = "KAMS", kams_final)
# 
# 
# kbcp_final <- final_gpa_promotion %>% 
#   filter(schoolabbrev == "KBCP") %>% 
#   as.data.frame() 
# writeData(wb, sheet = "KBCP", kbcp_final)
# 
# 
# kac_final <- final_gpa_promotion %>% 
#   filter(schoolabbrev == "KAC") %>% 
#   as.data.frame()
# writeData(wb, sheet = "KAC", kac_final)
# 
# saveWorkbook(wb, "18-19_Promotions_GPA.xlsx", overwrite = TRUE)
# 
# storedgrades %>%
#   left_join(eigth_students %>%
#               select(student_number,
#                      studentid),
#             by = "studentid") %>%
#   filter(termid < 2500,
#          student_number == 43127845) %>% View()


