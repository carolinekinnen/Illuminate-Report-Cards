
# library('ProjectTemplate')
# load.project()


# ---------------- ### KOA Report Card Comments - Gradebook Categories ### -----------------------

category_student_gradebook_avg <- assignments %>%
  left_join(scores, by = c("assignment_id", "gradebook_id")) %>%
  left_join(categories, by = c("category_id", "gradebook_id")) %>%
  left_join(illuminate_students, by = "student_id") %>%
  filter(!is.na(category_id)) %>%
  group_by(student_number, first_name, last_name, gradebook_id, category_id, category_name) %>% 
  summarize(all_possible_points = sum(possible_points, na.rm = TRUE),
            all_student_points = sum(value, na.rm = TRUE)) %>%
  mutate(all_points_percent_total = round((all_student_points/all_possible_points) * 100)) %>% 
  left_join(gradebooks, by = "gradebook_id") %>% 
  filter(active, 
         !is_deleted,
         str_detect(gradebook_name, "KOA")) %>%
  mutate(category_name = str_replace(category_name, 
         "\\.,", 
         "")) %>%
  mutate(subject = str_extract(tolower(gradebook_name), 
                               "math|history|science|centers|ela|social|esl")) %>%
  mutate(subject = case_when(
    subject == "centers" ~ "lit_centers",
    subject == "esl" ~ "ela",
    subject == "history" ~ "social",
    TRUE ~ subject
  ))

category_comments <- category_student_gradebook_avg %>%
  mutate(subject_cat_percent = str_c(category_name, " - ", all_points_percent_total, "%", "  ", sep = "")) %>%
  mutate(subject_field_name = str_c(subject, "_cat_comment", sep = "")) %>%
  ungroup() %>%
 group_by(gradebook_id) %>% 
  select(student_number, first_name, last_name, gradebook_id, subject_cat_percent, subject_field_name) %>%
  mutate(cat_comment = paste(subject_cat_percent, collapse = " ")) %>%
  select(-c(subject_cat_percent)) %>%
  unique() %>%
  ungroup() %>%
  select(-c(gradebook_id)) %>%
  group_by(student_number, first_name, last_name, subject_field_name) %>%
  mutate(row = row_number()) %>% 
  pivot_wider(names_from = "subject_field_name", values_from = "cat_comment") %>%
  select(-row)


