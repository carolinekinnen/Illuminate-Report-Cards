# Grades Munge
# cleaning and transforming files from Illuminate for Illuminate, Deans List, and Powerschool

# Course Names for Grades 4-8 ---------------------------------------------

course_names_teachers <- users_names %>%
  left_join(teachers,
    by = "users_dcid"

    # joins users and teachers
  ) %>%
  left_join(cc,
    by = "teacherid"
  ) %>%
  # filter(termid == ps_termid) %>% # joins users/teachers and cc
  left_join(courses,
    by = "course_number"
  ) %>% # joins users/teachers/cc and courses
  select(c(
    schoolid,
    studentid,
    course_long_name,
    first_name,
    last_name,
    teacher_full_name,
    course_number,
    section_number
  )) %>%
  left_join(students %>% # joins users/teachers/cc/courses and students
    select(student_number,
      studentid = id,
      grade_level,
      home_room,
      student_first = first_name,
      student_last = last_name
    ),
  by = "studentid"
  ) %>%
  filter(
    !grepl("Attendance|ELL", course_long_name),
    grade_level > 2
  ) %>%
  mutate(
    course_long_name = gsub("ELA", "English Language Arts", course_long_name),
    course_long_name = if_else(grepl("(\\d)th Math", course_long_name) &
      !grepl("Mathematics", course_long_name),
    gsub("Math", "Mathematics", course_long_name), course_long_name
    ),
    course_long_name = if_else(grepl("(\\d)th Literacy Center", course_long_name) &
      !grepl("Literacy Centers", course_long_name),
    gsub("Literacy Center", "Literacy Centers", course_long_name), course_long_name
    ),
    subject = gsub("(\\d)th ", "", course_long_name)
  ) %>%
  select(-c(section_number, first_name, last_name)) %>%
  left_join(schools, by = "schoolid") %>%
  mutate(subject = tolower(subject)) %>%
  mutate(subject = case_when(
    subject == "english language arts" ~ "ela",
    subject == "literacy centers" ~ "lit centers",
    subject == "mathematics" ~ "math",
    subject == "physical education" ~ "pe",
    subject == "pre-algebra" ~ "pre_algebra",
    subject == "social studies" ~ "social",
    TRUE ~ subject
  )) %>%
  mutate(course_number = str_to_upper(course_number))

# Connect students table from Powerschool with manual schools table
student_schools <- students_powerschool_transcripts %>%
  select(
    ps_schoolid,
    student_id
  ) %>%
  left_join(schools,
            by = c("ps_schoolid" = "schoolid")
  )

course_names <- course_names_teachers %>%
  select(
    site_id = schoolid,
    student_id = student_number,
    course_long_name,
    course_number
  ) %>%
  left_join(student_schools, by = "student_id") %>%
  mutate(store_code = RC_QUARTER) %>%
  dplyr::rename(course_school = schoolabbreviation) %>%
  mutate(subject = tolower(str_sub(course_long_name, start = 5))) %>%
  # filter(!grepl("behavior|choice reading|explorations|homework|musical theater|visual arts", subject)) %>%
  mutate(subject = case_when(
    subject == "english language arts" ~ "ela",
    subject == "literacy centers" ~ "lit centers",
    subject == "mathematics" ~ "math",
    subject == "pre-algebra" ~ "pre_algebra",
    subject == "physical education" ~ "pe",
    subject == "social studies" ~ "social",
    subject == "choice reading" ~ "choice_reading",
    TRUE ~ subject
  ))

# Current Quarter Report Card Data for Powerschool and Deans List  --------

rc_letter_grades <-
  kc_grades_full %>%
  drop_na("Mark") %>%
  mutate(
    `Course ID` =
      case_when(
        GradeBook == "2021_KOP_4_UniversityofCal_ELA" ~ "KOP4ELA",
        TRUE ~ `Course ID`
      ),
    `Course Name` = case_when(
      GradeBook == "2021_KOP_4_UniversityofCal_ELA" ~ "4th Math",
      TRUE ~ `Course Name`
    )
  ) %>%

  # Teacher column makes data duplicate grades. Erase teacher column
  # and call distinct

  select(-c(Teacher)) %>%
  distinct() %>%
  left_join(student_schools,
    by = c("Student ID" = "student_id")
  ) %>%
  select(-c(ps_schoolid, schoolname, schoolabbreviation))

# rc_letter_grades <- grade_df_df[[2]]%>% # grade_df_list %>%
#   map_df(
#     .f = get_q_grades_pct,
#     grade_type = "grade",
#     rc_quarter_input = RC_QUARTER
#   ) %>%
#   left_join(student_schools,
#     by = "student_id"
#   ) %>%
#   # filter(course_school == schoolabbreviation) %>%
#   select(-c(ps_schoolid, schoolname, schoolabbreviation))
# 
# rc_percent <- grade_df_df[[2]] %>% # grade_df_list %>%
#   map_df(
#     .f = get_q_grades_pct,
#     grade_type = "percent",
#     rc_quarter_input = RC_QUARTER
#   ) %>%
#   left_join(student_schools,
#     by = "student_id"
#   ) %>%
#   filter(course_school == schoolabbreviation) %>%
#   select(-c(ps_schoolid, schoolname, schoolabbreviation))

quarter_grades <- rc_letter_grades %>%
  left_join(course_names_teachers %>% rename(
    student_id = student_number,
    course_school = schoolabbreviation,
    site_id = schoolid
  ) %>% filter(!str_detect(subject, "3rd")) %>%
    select(-grade_level),
  by = c(
    "Student ID" =  "student_id",
    "Course ID" = "course_number"
  )
  ) %>%
  # not working with courses, can't remember where this was used
  mutate(Percentage = as.double(gsub("%", "", Percentage))) %>%
  left_join(students %>%
    select(student_id = student_number, first_name, last_name, grade_level),
  by = c("Student ID" = "student_id")
  ) %>%
  filter(grade_level > 3) %>%
  filter(!str_detect(`Course Name`, "Attendance")) %>%
  select(-c(`Section ID`, `Grading Period`)) %>%
  mutate(`Score Last Updated` = lubridate::mdy(`Score Last Updated`)) %>%
  distinct() %>% 
  arrange(`Student ID`) %>%
  drop_na("subject") %>%
  select(
    "Student ID",  
    "Last Name",       
    "First Name",        
    "GradeBook",       
    "Percentage",        
    "Mark",       
    "Course ID",         
    "Course Name",       
    "site_id",        
    "course_long_name",  
    "teacher_full_name", 
    "subject",           
    "course_school",
    "grade_level" 
  )


quarter_grades_pivot_wide <- quarter_grades %>%
  distinct() %>%
  select(
    student_id = "Student ID",
    subject,
    grade = Mark,
    percent = Percentage
  ) %>%
  group_by(
    subject,
    student_id,
  ) %>%
  # mutate(row = row_number()) %>%
  pivot_wider(
    names_from = subject,
    values_from = c(grade, percent)
  ) %>%
  unnest(cols = c(
    contains("grade"), 
    contains("percent")
  ))

# 
# 
# # Year Average Percentage for Powerschool and Illuminate ------------------
# 
# # Run all quarters seperately then bind rows because setting rc_quarter_input = c("Q1", "Q2", "Q3", "Q4") was dropping subjects
# 
# quarter_1_final <- grade_df_df[[2]] %>% # grade_df_list %>%
#   map_df(
#     .f = get_q_grades_pct,
#     grade_type = "percent",
#     rc_quarter_input = c("Q1")
#   )
# 
# quarter_2_final <- grade_df_df[[2]] %>% # grade_df_list %>%
#   map_df(
#     .f = get_q_grades_pct,
#     grade_type = "percent",
#     rc_quarter_input = c("Q2")
#   )
# 
# # commented out because school closure, quarter 4 has both Q3 and Q3
# 
# # quarter_3_final <- grade_df_df[[2]] %>% #grade_df_list %>%
# # map_df(.f = get_q_grades_pct,
# #        grade_type = "percent",
# #        rc_quarter_input = c("Q3"))
# 
# quarter_4_final <- grade_df_df[[2]] %>% # grade_df_list %>%
#   map_df(
#     .f = get_q_grades_pct,
#     grade_type = "percent",
#     rc_quarter_input = c("Q4")
#   )
# 
# all_quarter_percents <- quarter_1_final %>%
#   bind_rows(
#     quarter_2_final, # quarter_3_final,
#     quarter_4_final
#   ) %>%
#   mutate(percent = as.double(str_extract(percent, "[[:digit:]]+")))
# 
# final_percents <- all_quarter_percents %>%
#   dplyr::group_by(site_id, student_id, subject, store_code) %>%
#   summarize(percent = mean(percent)) %>%
#   mutate(percent = round(percent, 0)) %>%
#   mutate(subject = case_when(
#     subject == "social" ~ "social_studies",
#     TRUE ~ subject
#   ))
