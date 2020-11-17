

# --------------------- ### Subject and Teachers - Middle ###--------------------------

# Joins Powerschool tables (users, teachers, courses, cc, and students) to create one data frame that
# has one row for each class a student is rostered to

course_names_teachers <- users_names %>%
  left_join(teachers,
    by = "users_dcid"
    
  # joins users and teachers
  ) %>% 
  left_join(cc,
            by = "teacherid") %>% 
 # filter(termid == ps_termid) %>% # joins users/teachers and cc
  left_join(courses,
            by = "course_number") %>% # joins users/teachers/cc and courses 
  select(c(schoolid,
           studentid,
           course_long_name, 
           first_name,
           last_name, 
           teacher_full_name,
           course_number,
           section_number))  %>% 
  left_join(students %>% # joins users/teachers/cc/courses and students
              select(student_number,
                     studentid = id,
                     grade_level,
                     home_room,
                     student_first = first_name,
                     student_last = last_name), 
            by = "studentid") %>%
  filter(!grepl("Attendance|ELL", course_long_name), 
         grade_level > 2) %>% 
  mutate(course_long_name = gsub("ELA", "English Language Arts", course_long_name),
         course_long_name = if_else(grepl("(\\d)th Math", course_long_name) &
                                      !grepl("Mathematics", course_long_name), 
                                    gsub("Math", "Mathematics", course_long_name), course_long_name),
         course_long_name = if_else(grepl("(\\d)th Literacy Center", course_long_name) &
                                      !grepl("Literacy Centers", course_long_name), 
                                    gsub("Literacy Center", "Literacy Centers", course_long_name), course_long_name), 
         subject = gsub("(\\d)th ", "", course_long_name)) %>%
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
  ))


