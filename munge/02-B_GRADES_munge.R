
# Parameters --------------------------------------------------------------

QUARTER <- "Q1"


# --------------------------------------- ### Final Grades ### --------------------------------

original_letter_grades_dl <- 
  quarter_grades_pivot_wide %>%
  filter(student_id %in% dl_mod_grades_students$student_id) %>%
  select(student_id, 
         grade_ela_mod = grade_ela,
         `grade_lit centers_mod` = `grade_lit centers`,
         grade_math_mod = grade_math,
         grade_science_mod = grade_science,
         grade_social_mod = grade_social,
         grade_algebra_mod = grade_algebra,
         grade_pe_mod = grade_pe,
         grade_pre_algebra_mod = grade_pre_algebra
         )


final_grades <- final_percents %>%
  mutate(percent = as.character(round(percent, 1))) %>%
  left_join(grade_percent_scale %>%
    mutate(percent = as.character(percent)),
  by = "percent"
  )

final_percent_grades <- final_grades %>%
  mutate(percent = paste0(as.character(percent), "%")) %>%
  pivot_wider(names_from = subject, values_from = c(percent, grade)) %>%
  left_join(original_letter_grades_dl,
    by = "student_id"
  ) %>%
  mutate(
    grade_ela = if_else(grade_ela == grade_ela_mod | is.na(grade_ela_mod), grade_ela, grade_ela_mod),
    grade_math = if_else(grade_math == grade_math_mod | is.na(grade_math_mod), grade_math, grade_math_mod),
    `grade_lit centers` = if_else(`grade_lit centers` == `grade_lit centers_mod` | is.na(`grade_lit centers_mod`),
      `grade_lit centers`, `grade_lit centers_mod`
    ),
    grade_science = if_else(grade_science == grade_science_mod | is.na(grade_science_mod), grade_science, grade_science_mod),
    grade_social_studies = if_else(grade_social_studies == grade_social_mod | is.na(grade_social_mod),
      grade_social_studies, grade_social_mod
    ),
    grade_algebra = if_else(grade_algebra == grade_algebra_mod | is.na(grade_algebra_mod), grade_algebra, grade_algebra_mod),
    grade_pe = if_else(grade_pe == grade_pe_mod | is.na(grade_pe_mod), grade_pe, grade_pe_mod),
    grade_pre_algebra = if_else(grade_pre_algebra == grade_pre_algebra_mod | is.na(grade_pre_algebra_mod),
      grade_pre_algebra, grade_pre_algebra_mod
    )
  ) %>%
  select(-c(
    site_id,
    grade_ela_mod,
    `grade_lit centers_mod`,
    grade_math_mod,
    grade_science_mod,
    grade_social_mod,
    grade_algebra_mod,
    grade_pe_mod,
    grade_pre_algebra_mod
  )) %>%
  janitor::clean_names()

# final_percent_grades %>%
#   pivot_longer(cols = percent_ela:grade_pre_algebra, 
#                names_to = "subject", values_to = "percent", 
#                values_drop_na = TRUE) %>% View()
  

# --------------------------------------- ### Quarter GPAs ### --------------------------------

quarter_gpa <- quarter_grades %>%
  mutate(grade = case_when(
    percent == "90%" ~ "A-",
    percent == "80%" ~ "B-",
    percent == "70%" ~ "C-",
    percent == "60%" ~ "D-",
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
    gpa = quarter_gpa,
    across(everything(), ~replace_na(.x, ""))
  ) %>% 
  filter(store_code == QUARTER)



