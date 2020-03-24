

# --------------------- ### Subject and Teachers - Middle ###--------------------------

# Joins Powerschool tables (users, teachers, courses, cc, and students) to create one data frame that 
# has one row for each class a student is rostered to

course_names_teachers <- users_names %>% 
  left_join(teachers, 
            by = "users_dcid") %>% # joins users and teachers
  left_join(cc,
            by = "teacherid") %>% 
  filter(termid == ps_termid) %>% # joins users/teachers and cc
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
         grade_level > 3) %>% 
  mutate(course_long_name = gsub("ELA", "English Language Arts", course_long_name),
         course_long_name = if_else(grepl("(\\d)th Math", course_long_name) &
                                      !grepl("Mathematics", course_long_name), 
                                    gsub("Math", "Mathematics", course_long_name), course_long_name),
         course_long_name = if_else(grepl("(\\d)th Literacy Center", course_long_name) &
                                      !grepl("Literacy Centers", course_long_name), 
                                    gsub("Literacy Center", "Literacy Centers", course_long_name), course_long_name), 
         subject = gsub("(\\d)th ", "", course_long_name)) %>%
  select(-c(section_number, first_name, last_name))

# Connect students table from Powerschool with manual schools table
student_schools <- students_powerschool_transcripts %>% 
  select(ps_schoolid,
         student_id) %>% 
  left_join(schools,
            by = c("ps_schoolid" = "schoolid"))
