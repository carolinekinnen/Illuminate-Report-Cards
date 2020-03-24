
library('ProjectTemplate')
load.project()

# --------------------------------------- ### Final Grades ### --------------------------------

final_percent_grades <- final_percents %>%
  left_join(final_grades, by = c("student_id", "subject", "site_id")) %>%
  pivot_wider(names_from = subject, values_from = c(percent, grade)) %>%
  janitor::clean_names() 

# --------------------------------------- ### Quarter GPAs ### --------------------------------

quarter_gpa <- quarter_grades %>%
  filter(site_id == ps_schoolid) %>%
  select(site_id, student_id, grade, subject) %>%
  left_join(grade_scale, by = "grade") %>%
  group_by(site_id, student_id) %>%
  summarize(quarter_gpa = round2(mean(points, na.rm = TRUE), digits = 2))


# ---------------------------------- ### Cumulative GPAs ### ------------------------------------

cumulative_gpa <- final_grades %>% 
  left_join(grade_scale, by = "grade") %>%
  group_by(site_id, student_id) %>%
  summarize(cumulative_gpa = round2(mean(points, na.rm = TRUE), digits = 2)) 

# --------------------------------- ### Combine into One File ### -------------------------------

final_grades_gpa_illuminate_upload <- final_percent_grades %>%
  left_join(quarter_gpa, by = "student_number") %>%
  left_join(cumulative_gpa, by = "student_number")
