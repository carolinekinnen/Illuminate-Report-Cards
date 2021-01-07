

# Parameters --------------------------------------------------------------

QUARTER <- "Q2"

# --------------------------------------- ### Final Grades ### --------------------------------

current_quarter_grades_pivot_wide <- current_quarter_grades %>%
  
  # we only output grades for 4th grade and higher. Let's filter out k - 3rd
  filter(grade_level > 3) %>%
  select(
    `Student ID`,
    subject,
    grade,
    percentage
  ) %>%
  group_by(
    subject,
    `Student ID`,
  ) %>%
  # mutate(row = row_number()) %>%
  pivot_wider(
    names_from = subject,
    values_from = c(grade, percentage)
  ) %>%
  unnest(cols = c(
    grade_ela,
    # `grade_lit centers`,
    grade_math,
    grade_science,
    grade_social,
    grade_pe,
    grade_pre_algebra,
    grade_algebra,
    percent_ela,
    `percent_lit centers`,
    percent_math,
    percent_science,
    percent_social,
    percent_pe,
    percent_pre_algebra,
    percent_algebra
  ))


original_letter_grades_dl <- 
  current_quarter_grades_pivot_wide %>%
  filter(student_id %in% dl_mod_grades_students$student_id) %>%
  select(student_id, 
         grade_ela_mod = grade_ela,
         grade_lit_centers_mod = `grade_lit centers`,
         grade_math_mod = grade_math,
         grade_science_mod = grade_science,
         grade_social_mod = grade_social,
         grade_algebra_mod = grade_algebra,
         grade_pe_mod = grade_pe,
         grade_pre_algebra_mod = grade_pre_algebra
         ) %>%
  janitor::clean_names()

final_grades <- final_percents %>%
  mutate(percent = as.character(round(percent, 1))) %>%
  left_join(grade_percent_scale %>%
    mutate(percent = as.character(percent)),
  by = "percent"
  ) %>%
  janitor::clean_names()

final_percent_grades <- final_grades %>%
  mutate(percent = paste0(as.character(percent), "%")) %>%
  pivot_wider(names_from = subject, values_from = c(percent, grade)) %>%
  left_join(original_letter_grades_dl,
    by = "student_id"
  ) %>%
  rename(grade_lit_centers = `grade_lit centers`) %>%
  mutate(
    grade_ela = if_else(grade_ela == grade_ela_mod | is.na(grade_ela_mod), grade_ela, grade_ela_mod),
    grade_math = if_else(grade_math == grade_math_mod | is.na(grade_math_mod), grade_math, grade_math_mod),
    # grade_lit_centers = if_else(grade_lit_centers == grade_lit_centers_mod | is.na(grade_lit_centers_mod),
    #                             grade_lit_centers, grade_lit_centers_mod),
    grade_science = if_else(grade_science == grade_science_mod | is.na(grade_science_mod), grade_science, grade_science_mod),
    grade_social_studies = if_else(grade_social_studies == grade_social_mod | is.na(grade_social_mod),
                                   grade_social_studies, grade_social_mod),
    grade_algebra = if_else(grade_algebra == grade_algebra_mod | is.na(grade_algebra_mod), grade_algebra, grade_algebra_mod),
    grade_pe = if_else(grade_pe == grade_pe_mod | is.na(grade_pe_mod), grade_pe, grade_pe_mod),
    grade_pre_algebra = if_else(grade_pre_algebra == grade_pre_algebra_mod | is.na(grade_pre_algebra_mod),
      grade_pre_algebra, grade_pre_algebra_mod
    )
  ) %>%
  select(-c(
    site_id,
    grade_ela_mod,
    grade_lit_centers_mod,
    grade_math_mod,
    grade_science_mod,
    grade_social_mod,
    grade_algebra_mod,
    grade_pe_mod,
    grade_pre_algebra_mod
  )) %>%
  janitor::clean_names() %>%
  pivot_longer(contains("grade"), 
               names_to = "subject", 
               values_to = "grade") %>%
  pivot_longer(contains("percent"), 
               names_to = "subject2", 
               values_to = "percent") %>%
  drop_na("grade") %>%
  drop_na("percent") %>%
  mutate(subject = gsub("^[^_]*_", "", subject),
         subject2 = gsub("^[^_]*_", "", subject2)) %>%
  filter(subject==subject2) %>%
  select(-subject2) %>%
  filter(store_code == "Q1") %>%

  rename(`lit centers` = lit_centers)
  

# QUARTER GPAs ------------------------------------------------------------

quarter_gpa <- current_quarter_grades %>%
  select(site_id, `Student ID`, Mark, subject) %>%
  left_join(grade_scale, by = c("Mark"  = "grade")) %>%
  group_by(site_id, `Student ID`) %>%
  summarize(quarter_gpa = round2(mean(points, na.rm = TRUE), digits = 2))


# CUMULATIVE GPAs ---------------------------------------------------------

cumulative_gpa <- quarter_grades %>% 
  # select(-percent) %>%
  left_join(grade_scale, by = "grade") %>%
  group_by(course_school, `Student ID`) %>%
  summarize(cumulative_gpa = round2(mean(points, na.rm = TRUE), digits = 2))


# COMBINE INTO ONE FILE ---------------------------------------------------

final_grades_gpa_illuminate_upload <- final_percent_grades %>%
  left_join(quarter_gpa, by = "student_id") %>%
  left_join(cumulative_gpa, by = "student_id") %>%
  # rename(
  #   `ela grade` = grade_ela,
  #   `ela percent` = percent_math,
  #   `literacy centers grade` = grade_lit_centers,
  #   `literacy centers percent` = percent_lit_centers,
  #   `pe grade` = grade_pe,
  #   `pe percent` = percent_pe,
  #   `science grade` = grade_science,
  #   `science percent` = percent_science,
  #   `social studies grade` = grade_social_studies,
  #   `social studies percent` = percent_social_studies,
  #   `math grade` = grade_math,
  #   `math percent` = percent_math,
  #   `prealg grade` = grade_pre_algebra,
  #   `prealg percent` = percent_pre_algebra,
  #   `alg grade` = grade_algebra,
  #   `alg percent` = percent_algebra,
  #   gpa = cumulative_gpa,
  # ) %>%
  mutate(
    gpa = quarter_gpa,
    across(everything(), ~replace_na(.x, ""))
  ) %>% 
  filter(store_code == QUARTER)



