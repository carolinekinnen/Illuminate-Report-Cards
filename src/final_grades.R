 
# library('ProjectTemplate')
# load.project()

# --------------------------------------- ### Final Grades ### --------------------------------

final_grades <- final_percents %>%
  mutate(percent = paste0(as.character(round(percent, 0)), "%")) %>%
  left_join(final_grades, by = c("student_id", "subject", "site_id")) %>% 
  mutate(grade = case_when(
    percent == "90%" ~ "A-",
    percent == "80%" ~ "B-",
    percent == "70%" ~ "C-",
    percent == "60%" ~ "D-",     # bandaid fix, figure out why
    TRUE ~ grade
  )) %>% 
  select(site_id, student_id, grade, subject)
  

final_percent_grades <- final_percents %>%
  mutate(percent = paste0(as.character(round(percent, 0)), "%")) %>%
  left_join(final_grades, by = c("student_id", "subject", "site_id")) %>% 
  mutate(grade = case_when(
    percent == "90%" ~ "A-",
    percent == "80%" ~ "B-",
    percent == "70%" ~ "C-",
    percent == "60%" ~ "D-",     # bandaid fix, figure out why
    TRUE ~ grade
  )) %>%
  pivot_wider(names_from = subject, values_from = c(percent, grade)) %>%
  janitor::clean_names() 

# --------------------------------------- ### Quarter GPAs ### --------------------------------

quarter_gpa <- quarter_grades %>%
  mutate(grade = case_when(
    percent == "90%" ~ "A-",
    percent == "80%" ~ "B-",
    percent == "70%" ~ "C-",
    percent == "60%" ~ "D-",     # bandaid fix, figure out why
    TRUE ~ grade
  )) %>%
 # filter(site_id == ps_schoolid) %>%
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
  left_join(quarter_gpa, by = "student_id") %>%
  left_join(cumulative_gpa, by = "student_id")


# fix 2020

alg_8
prealg_7
alg_or_prealg <- bind_rows(alg_8, prealg_7) 


final_grades_gpa_illuminate_upload <- final_grades_gpa_illuminate_upload %>%
  unnest(cols = c(percent_math, grade_math, percent_algebra, grade_algebra, grade_pre_algebra, percent_pre_algebra)) %>%
  mutate(percent_math = case_when(
    student_id %in% alg_or_prealg$student_number ~ "NA",
    TRUE ~ percent_math),
    grade_math = case_when(
      student_id %in% alg_or_prealg$student_number ~ "NA",
      TRUE ~ grade_math),
    percent_algebra = case_when(
      !student_id %in% alg_or_prealg$student_number ~ "NA",
      TRUE ~ percent_algebra),
    grade_algebra = case_when(
      !student_id %in% alg_or_prealg$student_number ~ "NA",
      TRUE ~ grade_algebra),
    grade_pre_algebra = case_when(
      !student_id %in% alg_or_prealg$student_number ~ "NA",
      TRUE ~ grade_pre_algebra),
    percent_pre_algebra = case_when(
      !student_id %in% alg_or_prealg$student_number ~ "NA",
      TRUE ~ percent_pre_algebra)) %>% 
  mutate_all(funs(str_replace_all(., "NA", " ")))

# last minute edits

final_grades_gpa_illuminate_upload <- final_grades_gpa_illuminate_upload %>%
  mutate(grade_math = case_when(
    student_id == 50410941 ~ "C",
    TRUE ~ grade_math
  )) %>%
  mutate(percent_math = case_when(
    student_id == 50410941 ~ "73%",
    TRUE ~ percent_math
  )) %>%
  mutate(grade_science = case_when(
    student_id == 50372559 ~ "A+",
    TRUE ~ grade_math
  )) %>%
  filter(student_id != 50333465 & student_id != 50280137)



# Identifying which students grades were changed at KAC

kac_fail_change <- all_quarter_percents %>%
  dplyr::group_by(site_id, student_id, subject) %>%
  summarize(percent = mean(percent)) %>% filter(site_id == 400146 & percent < 70) %>% 
  left_join(students, by = c("student_id" = "student_number"))

kac_fail_quarter <- quarter_4_final %>% 
  filter(site_id == 400146) %>%
  left_join(students, by = c("student_id" = "student_number"))

write.csv(kac_fail_change %>%
            as.data.frame(),
          file = "~/Downloads/kac_fail_change_updated.csv",
          row.names = FALSE)


kop_fail_change <- all_quarter_percents %>%
  dplyr::group_by(site_id, student_id, subject) %>%
  summarize(percent = mean(percent)) %>% filter(site_id == 4001802 & percent < 70 & subject %in% c("math", "ela", "science")) %>% 
  left_join(students, by = c("student_id" = "student_number"))

