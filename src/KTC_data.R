
# ---------------- ### Data needed for 7th grade students ##### ----------------
#  -Winter MAP percentiles 
#  -Quarter final letter grades (core subjects: ELA, Math, Social Studies, Science)
#  -Cumulative attendance percentage 

ktc_7_request <- map_winter_pivot_students %>%
  left_join(quarter_grades_pivot_wide, by = "student_id") %>%
  left_join(attend_school_grade_student_totals %>%
              select(student_id = student_number,
                     rate), by = "student_id") %>%
  filter(grade_level == 7) %>%
  select(student_id,
         first_name,
         last_name,
         home_room,
         schoolabbreviation,
         attendance_rate = rate,
         grade_ela,
         percent_ela,
         grade_math,
         percent_math,
         grade_pre_algebra,
         percent_pre_algebra,
         grade_science,
         percent_science,
         grade_social,
         percent_social,
         map_math_percentile = test_percentile_Mathematics,
         map_reading_percentile = test_percentile_Reading,
         ) %>%
  unique()

# ---------------- ### Data needed for 6th grade students ##### ----------------
# needed for LINK Scholarship
# from their time in 5th grade
# - 5th Grade Spring MAP Percentiles
# - 5th Grade Final Grades (core subjects: ELA, Math, Social Studies, Science)
# - 5th Grade Cumulative Attendance percentage

students_6th <- students %>%
  filter(grade_level == 6,
         enroll_status == 0)

map_last_spring_6th <- map_last_spring_students %>% 
  filter(student_id %in% students_6th$student_number) %>%
  select(-c(test_ritscore_Mathematics, test_ritscore_Reading, id, schoolid,
            first_name, last_name, home_room, enroll_status, grade_level, schoolabbreviation))

grades_6th <- last_year_grades %>%
  filter(student_id %in% students_6th$student_number) %>%
  select(-c(site_id, schoolabbr, enroll_status, same_schoolid_course, gpa, n_courses, 
            letter_grades, points_used, art.grade, art.percent, dance.grade, dance.percent, 
            pe.grade, pe.percent))
  
attendance_6th <- last_year_attendance %>%
  filter(student_number %in% students_6th$student_number) %>%
  select(-c(grade_level, schoolabbreviation, first_name, last_name, tardy)) %>%
  rowwise() %>%
  mutate(rate = present/enrolled * 100) %>%
  select(student_number, rate)

ktc_students_6th_file <- students_6th %>%
  select(schoolid, student_number, first_name, last_name) %>%
  left_join(map_last_spring_6th %>% dplyr::rename(student_number = student_id), by = "student_number") %>% 
  left_join(grades_6th %>% dplyr::rename(student_number = student_id), by = "student_number") %>% 
  left_join(attendance_6th, by = "student_number") %>% 
  select(-term_name) %>%
  left_join(schools, by = "schoolid")
  

# --------------------- ### Data needed for 5th-8th ### -------------------

# 19-20 final GPAs (corrected from Illuminate)

final_grades_correct <- read_csv(here::here("output/19-20 Files/grades/Q4 RC Final Grades 2020-06-11.csv")) %>%
  clean_names() %>%
  select(student_id,
         first_name,
         last_name,
         grade_level,
         ela_grade,
         ela_percent,
         literacy_centers_grade,
         literacy_centers_percent,
         science_grade,
         science_percent,
         social_studies_grade,
         social_studies_percent,
         math_grade,
         math_percent,
         prealg_grade,
         prealg_percent,
         alg_grade,
         alg_percent,
         gpa)

# DL status

dl_students <- read_csv("~/Dropbox (KIPP Chicago Schools)/Distance_Learning_Analysis/new-project/data/flat_files/dl_students.csv") %>%
  select(student_id = student_number) %>%
  mutate(diverse_learner = "Yes")

# emails

email_7 <- read_table2("~/Downloads/student.export - 2020-06-15T151342.561.text") %>%
  rename(student_id = 1,
        student_email = 2,
        student_password = 3)

email_8 <- read_table2("~/Downloads/student.export - 2020-06-15T151816.787.text") %>%
  rename(student_id = 1,
         student_email = 2,
         student_password = 3)

# 5th grade
## -Spring 2019 Reading and Math percentiles
## -Absences, tardies, and attendance rate
## -Final gpa 
## -Final grades

fifth_ktc_request <- final_grades_correct %>% 
  filter(grade_level == 5) %>%
  select(-c(alg_grade, alg_percent, prealg_grade, prealg_percent, social_studies_grade, social_studies_percent)) %>%
  left_join(attend_school_grade_student_totals %>%
              select(student_id = student_number,
                     total_absent,
                     total_tardy,
                     rate), by = "student_id") %>%
  left_join(map_last_spring_pivot, by = "student_id") %>%
  left_join(students %>% select(student_id = student_number, schoolid), by = "student_id") %>%
  left_join(schools %>% select(-schoolname), by = "schoolid")
  
write_csv(x = fifth_ktc_request, path = here::here("/output/19-20 Files/ktc/Q4_fifth_request.csv"))
  
# 6th grade
## -Spring 2019 Reading and Math percentiles
## -Absences, tardies, and attendance rate
## -Final gpa 
## -Final grades

sixth_ktc_request <- final_grades_correct %>% 
  filter(grade_level == 6) %>%
  select(-c(alg_grade, alg_percent, prealg_grade, prealg_percent)) %>%
  left_join(attend_school_grade_student_totals %>%
              select(student_id = student_number,
                     total_absent,
                     total_tardy,
                     rate), by = "student_id") %>%
  left_join(map_last_spring_pivot, by = "student_id") %>%  
  left_join(students %>% select(student_id = student_number, schoolid), by = "student_id") %>%
  left_join(schools %>% select(-schoolname), by = "schoolid")

write_csv(x = sixth_ktc_request, path = here::here("/output/19-20 Files/ktc/Q4_sixth_request.csv"))


# 7th grade
## -Spring 2019 Reading and Math percentiles
## -Absences, tardies, and attendance rate
## -Final gpa 
## -Final grades
## -Student ID
## -Student DOB
## -Gender
## -Home address
## -Guardian email
## -Home phone number
## IEP/504 plan (Y or N)
## -Student email and password

seventh_ktc_request <- final_grades_correct %>% 
  filter(grade_level == 7) %>%
  select(-c(literacy_centers_grade,
         literacy_centers_percent)) %>%
  left_join(attend_school_grade_student_totals %>%
              select(student_id = student_number,
                     total_absent,
                     total_tardy,
                     rate), by = "student_id") %>%
  left_join(map_last_spring_pivot, by = "student_id") %>%
  left_join(students_ktc, by = "student_id") %>%
  left_join(dl_students, by = "student_id") %>%
  mutate(diverse_learner = case_when(
    is.na(diverse_learner) ~ "No",
    TRUE ~ diverse_learner
  )) %>%
  left_join(email_7, by = "student_id") %>%
  left_join(schools %>% select(-schoolname), by = "schoolid")

write_csv(x = seventh_ktc_request, path = here::here("/output/19-20 Files/ktc/Q4_seventh_request.csv"))

  

# 8th grade
## -Spring 2019 Reading and Math percentiles
## -Absences, tardies, and attendance rate
## -Final gpa 
## -Final grades
## -Home address
## -Guardian email
## -Home phone number

eighth_ktc_request <- final_grades_correct %>% 
  filter(grade_level == 8) %>%
  select(-c(literacy_centers_grade,
            literacy_centers_percent)) %>%
  left_join(attend_school_grade_student_totals %>%
              select(student_id = student_number,
                     total_absent,
                     total_tardy,
                     rate), by = "student_id") %>%
  left_join(map_last_spring_pivot, by = "student_id") %>%
  left_join(students_ktc, by = "student_id") %>%
  left_join(email_8, by = "student_id") %>%
  left_join(schools %>% select(-schoolname), by = "schoolid")

write_csv(x = eighth_ktc_request, path = here::here("/output/19-20 Files/ktc/Q4_eighth_request.csv"))


