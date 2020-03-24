
library('ProjectTemplate')
load.project()



# --------------------- ### Courses and Teachers - Middle ###--------------------------

course_subject <- course_names_teachers %>%
  mutate(subject_course = sprintf("%s_course", subject)) %>%
  select(-c(teacher_full_name, #teacher_first_last, 
            subject)) %>%
  spread(subject_course, course_long_name)

teacher_subject <- course_names_teachers %>%
  mutate(subject_teacher = sprintf("%s_teacher", subject)) %>%
  select(-c(course_long_name, 
            subject)) %>%
  spread(subject_teacher, teacher_full_name) #teacher_first_last) 

course_names_teachers_final <- course_subject %>%
  left_join(teacher_subject,
            by = c("schoolid",
                   "studentid",
                   "student_number",
                   "grade_level",
                   "student_first",
                   "student_last")) %>%
  select(-home_room.y) %>%
  dplyr::rename(home_room = home_room.x) %>%
  mutate(prealgebra_spanish = if_else(is.na(`Pre-Algebra_course`),`Pre-Algebra_course`, 'Pre-álgebra'),
         math_spanish = if_else(is.na(`Mathematics_course`), `Mathematics_course`, 'Matemáticas'),
         algebra_spanish = if_else(is.na(`Algebra_course`), `Algebra_course`, 'Álgebra')) %>%    # spanish pre-algebra courses for KOA and KBCP because different within grade level cannot be printed on template
  mutate(kap4_hr_teacher = case_when(
    home_room == "4th University of Wisconsin" ~ "Kate Ray",           
    home_room == "4th Berkeley" ~ "Ashley McNulty",
    home_room == "4th Central State University" ~ "Faith Anderson",
    home_room == "4th Philander Smith College" ~ "Jacques Edwards")) %>% 
  dplyr::rename(ELA = `English Language Arts_course`,
         `ELA Teacher` = `English Language Arts_teacher`,
         Math = `Mathematics_course`, 
         `Math Teacher` = `Mathematics_teacher`,
         Science = `Science_course`,
         # Science_teacher = `Science Teacher`, 
         Art = Art_course,
         `Art Teacher` = Art_teacher,
         PE = `Physical Education_course`, 
         `PE Teacher` = `Physical Education_teacher`,
         `Social Studies` = `Social Studies_course`,
         `Social Studies Teacher` = `Social Studies_teacher`,
         Dance = `Dance_course`,
         `Dance Teacher` = `Dance_teacher`,
         `Literacy Centers` = `Literacy Centers_course`,
         `Literacy Centers Teacher` = `Literacy Centers_teacher`,
         `Algebra Spanish` = algebra_spanish,
         `Math Spanish` = math_spanish,
         `Pre Algebra Spanish` = prealgebra_spanish) %>%
   mutate(
    ELA = case_when(
      `ELA Teacher` == "Kayla Nuguid" ~ "ESL/English Language Arts",    # renaming a course per KOA request
      TRUE ~ ELA)
  )


#Reviewing work - finding cases of duplicate IDs

# flags duplicate ids, should be 0
dup_ids <- which(duplicated(course_names_teachers_final$studentid))

# If applicable, make duplicates their own dataframe to tell where mistakes are
duplicates <- course_names_teachers_final[dup_ids,]

dups <- course_names_teachers_final[dup_ids,] %>% select(studentid) %>% unique()


# --------------------- ### Combining Tables - Primary ###--------------------------

primary_course_teacher <- users_names %>% 
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
                     student_first = first_name,
                     student_last = last_name), 
            by = "studentid") %>%
  filter(grade_level < 4) %>%
  filter(!grepl("Science", course_long_name))

