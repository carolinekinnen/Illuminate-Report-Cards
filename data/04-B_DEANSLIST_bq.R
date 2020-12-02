

# PARAMETERS --------------------------------------------------------------

ps_termid <- calc_ps_termid(current_first_year)


# BIG QUERY TABLES --------------------------------------------------------

course_section <- cc %>%
  filter(termid == ps_termid) %>%
  left_join(courses,
    by = "course_number"
  ) %>%
  left_join(students %>%
    dplyr::rename(studentid = id) %>% select(-schoolid),
  by = "studentid"
  ) %>%
  select(
    student_number,
    course_long_name,
    sectionid,
  ) %>%
  mutate(course_long_name = case_when(
    str_detect(course_long_name, "ELA") ~ gsub("ELA", "English Language Arts", course_long_name),
    course_long_name == "4th Math" ~ "4th Mathematics",
    course_long_name == "5th Math" ~ "5th Mathematics",
    course_long_name == "5th Literacy Center" ~ "5th Literacy Centers",
    course_long_name == "6th Math" ~ "6th Mathematics",
    course_long_name == "7th Math" ~ "7th Mathematics",
    course_long_name == "8th Math" ~ "8th Mathematics",
    TRUE ~ course_long_name
  ))
