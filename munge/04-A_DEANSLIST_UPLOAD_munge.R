
#-------------------------- ### Grades for Deans List Progress Reports ###----------------------------------

dl_gradebook_w_subject <-
  dl_gradebook %>%
  mutate(
    subject = word(GradeBook, start = -1, sep = "_")
  ) %>%
  mutate(
    subject = str_to_upper(subject)
  ) %>%
  filter(!subject == "EX") %>%
  filter(!subject == "SUBJECT") %>%
  filter(!subject == "EXPLORATIONS") %>%
  mutate(
    subject =
      case_when(
        subject == "ELA" ~ "ENGLISH LANGUAGE ARTS",
        subject == "MATH" ~ "MATHEMATICS",
        subject == "SS" ~ "SOCIAL STUDIES",
        subject == "READING" ~ "ENGLISH LANGUAGE ARTS",
        subject == "LITERACY" ~ "LITERACY CENTERS",
        subject == "CENTERS" ~ "LITERACY CENTERS",
        subject == "EEL ELA" ~ "ENGLISH LANGUAGE ARTS",
        subject == "GENED" ~ "MATHEMATICS",
        subject == "HONORSMATH" ~ "MATHEMATICS",
        subject == "ELA1" ~ "ENGLISH LANGUAGE ARTS",
        subject == "MATH1" ~ "MATHEMATICS",
        subject == "DL ELA" ~ "ENGLISH LANGUAGE ARTS",
        subject == "DL MATH" ~ "MATHEMATICS",
        subject == "PE" ~ "PHYSICAL EDUCATION",
        TRUE ~ subject
      )
  ) %>%
  select(
    `Student ID`, 
    Teacher, 
    subject, 
    GradeBook
  )

dl_upload <- quarter_grades %>%
  left_join(course_names_teachers %>% select(
    student_id = student_number,
    course_long_name
  ),
  by = c("student_id", "course_long_name")
  ) %>%
  mutate(teacher_last_name = word(teacher_full_name, -1)) %>%
  left_join(course_section %>%
    dplyr::rename(student_id = student_number), by = c("student_id", "course_long_name")) %>%
  left_join(dl_rosters,
    by = c("sectionid" = "sec_id")
  ) %>%
  left_join(student_enroll %>%
    select(
      student_number,
      schoolid
    ),
  by = c("student_id" = "student_number")
  ) %>%
  filter(
    site_id == schoolid,
    !is.na(course_long_name),
    !is.na(grade)
  ) %>%
  ungroup() %>%
  mutate(
    score_last_updated = as.Date(rc_quarter_last_day),
    grading_period = str_c(as.Date(rc_quarter_first_day), as.Date(rc_quarter_last_day), sep = " - ")
  ) %>%
  select(site_id,
    `Student ID` = student_id,
    last_name,
    first_name,
    teacher_full_name,
    course_long_name,
    incomplete_GradeBook = gb_name,
    score_last_updated,
    Percentage = percent,
    Mark = grade,
    grading_period
  ) %>%
  mutate(
    Teacher = word(teacher_full_name, start = 2, sep = " "), 
    course_long_name = str_to_upper(course_long_name),
    subject = word(course_long_name, start = 2, end = -1, sep = " ")
  ) %>%
  left_join(dl_gradebook_w_subject, 
            by = c("Student ID", "Teacher", "subject")) %>%
  mutate(
    GradeBook = if_else(is.na(GradeBook), incomplete_GradeBook, GradeBook)
  ) %>%
  ungroup() %>%
  select(-c(incomplete_GradeBook, 
            subject)) %>%
  
  # only retain columns required for upload (anything else and the upload will fail)
  select(`Student ID`, Percentage, Mark, GradeBook, site_id)

dl_upload_kac <- dl_upload %>%
  filter(site_id == 400146) %>%
  select(-site_id)

dl_upload_kap <- dl_upload %>%
  filter(site_id == 78102) %>%
  select(-site_id)

dl_upload_kams <- dl_upload %>%
  filter(site_id == 7810) %>%
  select(-site_id)

dl_upload_kbcp <- dl_upload %>%
  filter(site_id == 400163) %>%
  select(-site_id)

dl_upload_koa <- dl_upload %>%
  filter(site_id == 400180) %>%
  select(-site_id)
