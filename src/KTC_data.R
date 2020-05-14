
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
  



