
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


