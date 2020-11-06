
# --------------------------------------- ### Final Grades ### --------------------------------

# final_grades <- final_percents %>%
#   mutate(percent = paste0(as.character(round(percent, 0)), "%")) %>%
#   left_join(final_grades, by = c("student_id", "subject", "site_id")) %>%
#   select(site_id, student_id, grade, subject)
# 
# 
# final_percent_grades <- final_percents %>%
#   mutate(percent = paste0(as.character(round(percent, 0)), "%")) %>%
#   left_join(final_grades, by = c("student_id", "subject", "site_id")) %>%
#   pivot_wider(names_from = subject, values_from = c(percent, grade)) %>%
#   janitor::clean_names()

final_grades <- final_percents %>%
  mutate(percent = as.character(round(percent, 1))) %>%
  left_join(grade_percent_scale %>%
    mutate(percent = as.character(percent)),
  by = "percent"
  )

final_percent_grades <- final_grades %>%
  mutate(percent = paste0(as.character(percent), "%")) %>%
  pivot_wider(names_from = subject, values_from = c(percent, grade)) %>%
  janitor::clean_names()

# --------------------------------------- ### Quarter GPAs ### --------------------------------

quarter_gpa <- quarter_grades %>%
  mutate(grade = case_when(
    percent == "90%" ~ "A-",
    percent == "80%" ~ "B-",
    percent == "70%" ~ "C-",
    percent == "60%" ~ "D-",     #
    TRUE ~ grade
  )) %>%
  # filter(site_id == ps_schoolid) %>%
  select(site_id, student_id, grade, subject) %>%
  left_join(grade_scale, by = "grade") %>%
  group_by(site_id, student_id) %>%
  summarize(quarter_gpa = round2(mean(points, na.rm = TRUE), digits = 2))


# ---------------------------------- ### Cumulative GPAs ### ------------------------------------

cumulative_gpa <- final_grades %>% 
  select(-percent) %>%
  left_join(grade_scale, by = "grade") %>%
  group_by(site_id, student_id) %>%
  summarize(cumulative_gpa = round2(mean(points, na.rm = TRUE), digits = 2))

# --------------------------------- ### Combine into One File ### -------------------------------

final_grades_gpa_illuminate_upload <- final_percent_grades %>%
  left_join(quarter_gpa, by = "student_id") %>%
  left_join(cumulative_gpa, by = "student_id") %>%
  rename(
    `ela grade` = grade_ela,
    `ela percent` = percent_math,
    `literacy centers grade` = grade_lit_centers,
    `literacy centers percent` = percent_lit_centers,
    `pe grade` = grade_pe,
    `pe percent` = percent_pe,
    `science grade` = grade_science,
    `science percent` = percent_science,
    `social studies grade` = grade_social_studies,
    `social studies percent` = percent_social_studies,
    `math grade` = grade_math,
    `math percent` = percent_math,
    `prealg grade` = grade_pre_algebra,
    `prealg percent` = percent_pre_algebra,
    `alg grade` = grade_algebra,
    `alg percent` = percent_algebra,
    gpa = cumulative_gpa,
  ) %>%
  mutate(
    across(everything(), ~replace_na(.x, ""))
  )


# Rename Columns ----------------------------------------------------------


# final_grades_gpa_illuminate_upload %>% 
#   ungroup() %>%
#   select(student_id, percent_algebra, grade_algebra) %>%
#   mutate(grade_pct_match = case_when(
#     grade_algebra == "A+" & percent_algebra >= 98 ~ TRUE, 
#     grade_algebra == "A" & between(percent_algebra, 94.0, 97.9) ~ TRUE, 
#     grade_algebra == "A-" & between(percent_algebra, 90.0, 93.9) ~ TRUE, 
#     grade_algebra == "B+" & between(percent_algebra, 87.0, 89.9) ~ TRUE, 
#     grade_algebra == "B" & between(percent_algebra, 83.0, 86.9) ~ TRUE, 
#     grade_algebra == "B-" & between(percent_algebra, 80.0, 82.9) ~ TRUE, 
#     grade_algebra == "C+" & between(percent_algebra, 77.0, 79.9) ~ TRUE, 
#     grade_algebra == "C" & between(percent_algebra, 73.0, 76.9) ~ TRUE, 
#     grade_algebra == "C-" & between(percent_algebra, 70.0, 72.9) ~ TRUE, 
#     grade_algebra == "D+" & between(percent_algebra, 67.0, 69.9) ~ TRUE, 
#     grade_algebra == "D" & between(percent_algebra, 63.0, 66.9) ~ TRUE, 
#     grade_algebra == "D-" & between(percent_algebra, 60.0, 62.9) ~ TRUE, 
#     grade_algebra == "F" & between(percent_algebra, 0, 59.9) ~ TRUE, 
#     TRUE ~ FALSE
#   )) %>% filter(grade_pct_match == FALSE) %>% View()
