
# Parameters --------------------------------------------------------------

QUARTER_1 = "Q1"
QUARTER_2 = "Q2"
QUARTER_3 = "Q3"
QUARTER_4 = "Q4"

CURRENT_QUARTER <- QUARTER_2


# Previous Quarter Grades -------------------------------------------------

q1_grades <- previous_quarter_grades_q1 %>% 
  select(student_number, 
         subject, 
         grade, 
         percent) %>%
  distinct()


# Combined Grade File -----------------------------------------------------

cumulative_grade_file <- 
  current_quarter_grades %>%
  rename(student_number = `Student ID`) %>%
  select(student_number, grade, subject, percentage) %>%
  mutate(quarter = CURRENT_QUARTER) %>%
  bind_rows(q1_grades %>%
              select(student_number, 
                     grade, 
                     subject, 
                     percentage = percent) %>%
              mutate(quarter = QUARTER_1)
  ) %>%
  left_join(grade_scale, by = "grade")

# GPA ---------------------------------------------------------------------

current_quarter_gpa <- current_quarter_grades %>%
  select(site_id, `Student ID`, grade, subject) %>%
  left_join(grade_scale, by = "grade") %>%
  group_by(`Student ID`) %>%
  summarize(current_quarter_gpa = round2(mean(points, na.rm = TRUE), digits = 2))

q1_gpa <- q1_grades %>%
  select(student_number, 
         grade) %>%
  left_join(grade_scale, by = "grade") %>%
  group_by(student_number) %>%
  summarize(gpa_Q1 = round2(mean(points, na.rm = TRUE), digits = 2))
  
cumulative_gpa <- 
  cumulative_grade_file %>%
  group_by(student_number) %>%
  summarize(cumulative_gpa = round2(mean(points, na.rm = TRUE), digits = 2))

# N Courses,  List Grades,  List Points -----------------------------------

# n_courses_QX
# letter_grades_QX
# points_used_QX

cumulative_course_grade_point <- 
  cumulative_grade_file %>%
  select(-subject) %>%
  pivot_wider(names_from = c(quarter), 
              values_from = c(grade, points, percentage)) %>%
  mutate(n_courses_Q1 = lengths(grade_Q1),
         n_courses_Q2 = lengths(grade_Q2),
         letter_grades_Q1 = map_chr(grade_Q1, str_c, collapse= ' '),
         letter_grades_Q2 = map_chr(grade_Q2, str_c, collapse= ' '),
         points_used_Q1 = map_chr(points_Q1, str_c, collapse = ' '),
         points_used_Q2 = map_chr(points_Q2, str_c, collapse = ' ')
         ) %>%
  select(student_number, 
         n_courses_Q1, 
         n_courses_Q2, 
         letter_grades_Q1, 
         letter_grades_Q2, 
         points_used_Q1, 
         points_used_Q2)


# Final Illuminate Upload -------------------------------------------------

final_grades_gpa_illuminate_upload_4_8 <- cumulative_grade_file %>%
  select(-points) %>%
  filter(quarter == CURRENT_QUARTER) %>%
  mutate(percent = paste0(as.character(round2(percentage)), "%")) %>%
  select(-percentage) %>%
  pivot_wider(names_from = subject, values_from = c(percent, grade)) %>%
  left_join(current_quarter_gpa, 
            by = c("student_number" = "Student ID")) %>%
  left_join(q1_gpa, 
            by = c("student_number")) %>%
  left_join(cumulative_gpa, 
            by = c("student_number")) %>%
  left_join(cumulative_course_grade_point, 
            by = c("student_number")) %>%
  left_join(current_quarter_grades %>%
              select(`Student ID`, site_id, school_abbr, grade_level) %>%
              distinct(), 
            by = c("student_number" = "Student ID")) %>%
  mutate(enroll_status = 0, 
         same_schoolid_course = TRUE
  ) %>%
  select(site_id, 
         grade_level,
         student_id = student_number, 
         course_school = school_abbr, 
         schoolabbr = school_abbr, 
         enroll_status, 
         same_schoolid_course, 
         grade_ela:`grade_mathematics centers`, 
         percent_ela:`percent_mathematics centers`,
         gpa_Q1,
         gpa_Q2 = current_quarter_gpa, 
         n_courses_Q1,
         n_courses_Q2,
         letter_grades_Q1,
         letter_grades_Q2,
         points_used_Q1,
         points_used_Q2,
         gpa = cumulative_gpa
         )