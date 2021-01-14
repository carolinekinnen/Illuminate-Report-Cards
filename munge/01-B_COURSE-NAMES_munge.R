
sy <- calc_academic_year(today(), format = "firstyear")

current_first_year <- calc_academic_year(lubridate::today(),
                                         format = "first_year"
)

current_last_year <- calc_academic_year(today(),
                                        format = "second_year"
)

PS_TERMID <- calc_ps_termid(current_first_year)

# --------------------- ### Courses and Teachers - Middle ###--------------------------

teacher_subject <- course_names_teachers %>%
  mutate(subject_teacher = sprintf("%s_teacher", subject)) %>%
  select(-c(course_number, subject, course_long_name)) %>%
  group_by(subject_teacher, schoolid, studentid, student_number, grade_level, home_room) %>%
  dplyr::mutate(row = row_number()) %>%
  # group_by(schoolid, studentid, student_number, home_room, grade_level, student_first, student_last) %>%
  pivot_wider(
    id_cols = c(row, schoolid, studentid, student_number, grade_level, home_room),
    names_from = subject_teacher, values_from = teacher_full_name
  ) %>%
  select(-row)

course_subject <- course_names_teachers %>%
  mutate(subject_course = sprintf("%s_course", subject)) %>%
  select(-c(course_number, subject, teacher_full_name)) %>%
  group_by(subject_course, schoolid, studentid, student_number, grade_level, home_room) %>%
  mutate(row = row_number()) %>%
  # group_by(schoolid, studentid, student_number, home_room, grade_level, student_first, student_last) %>%
  pivot_wider(
    id_cols = c(row, schoolid, studentid, student_number, grade_level, home_room),
    names_from = subject_course, values_from = course_long_name
  ) %>%
  select(-row) %>%
  rename(`English Language Arts_course` = ela_course,
         `Literacy Centers_course` = `lit centers_course`,
         Science_course = science_course,
         `Social Studies_course` = social_course,
         `Physical Education_course` = pe_course,
         Explorations_course = explorations_course
         )

course_names_teachers_final <- course_subject %>%
  left_join(teacher_subject,
    by = c(
      "schoolid",
      "studentid",
      "student_number",
      "grade_level"
    )
  ) %>%
  select(-home_room.y) %>%
  dplyr::rename(home_room = home_room.x) %>%
  mutate(
    prealgebra_spanish = if_else(is.na(`pre_algebra_course`), `pre_algebra_course`, "Pre-álgebra"),
    math_spanish = if_else(is.na(`math_course`), `math_course`, "Matemáticas"),
    algebra_spanish = if_else(is.na(`algebra_course`), `algebra_course`, "Álgebra"),
    ela_spanish = if_else(is.na(`English Language Arts_course`), `English Language Arts_course`, "Literatura"),
    lit_center_spanish = if_else(is.na(`Literacy Centers_course`), `Literacy Centers_course`, "Centros de Alfabetización"),
    science_spanish = if_else(is.na(Science_course), Science_course, "Ciencias"),
    ss_spanish = if_else(is.na(`Social Studies_course`), `Social Studies_course`, "Ciencias sociales"),
    pe_spanish = if_else(is.na(`Physical Education_course`), `Physical Education_course`, "Educación Física"),
    explorations_spanish = if_else(is.na(Explorations_course), Explorations_course, "Exploraciones")
  ) %>%
  mutate(kap4_hr_teacher = case_when(
    home_room == "4th University of Wisconsin" ~ "Kate Ray",
    home_room == "4th Berkeley" ~ "Ashley McNulty",
    home_room == "4th Central State University" ~ "Faith Anderson",
    home_room == "4th Philander Smith College" ~ "Jacques Edwards"
  )) %>%
  dplyr::rename(
    `ELA` = `English Language Arts_course`,
    `ELA Teacher` = `ela_teacher`,
    `ELA Spanish` = ela_spanish, 
    
    `Math` = math_course, 
    `Math Teacher` = math_teacher, 
    `Math Spanish` = math_spanish, 
    
    `Social Studies` = `Social Studies_course`,
    `Social Studies Teacher` = social_teacher,
    `Social Studies Spanish` = ss_spanish, 
    
    `Literacy Centers` = `Literacy Centers_course`,
    `Literacy Centers Teacher` = `lit centers_teacher`,
    `Literacy Centers Spanish` = `lit_center_spanish`,                
    
    `Science` = `Science_course`,    
    `Science Teacher` = `science_teacher`,  
    `Science Spanish` = `science_spanish`,                     
    
    `Mathmatics` = `mathematics centers_course`,  
    `Mathmatics Teacher` = `mathematics centers_teacher`, 
    
    `Algebra` = `algebra_course`,    
    `Algebra Teacher` = `algebra_teacher`, 
    `Algebra Spanish` = `algebra_spanish`,                     
    
    `Pre Algebra` = `pre_algebra_course`, 
    `Pre Algebra Teacher` = `pre_algebra_teacher`,  
    `Pre Algebra Spanish` = `prealgebra_spanish`, 
    
    `PE` = `Physical Education_course`, 
    `pe_teacher` = `pe_teacher`, 
    `pe_spanish` = `pe_spanish` 
  ) %>%
  mutate(
    `ELA` = case_when(
      `ELA Teacher` == "Kayla Nuguid" ~ "ESL/English Language Arts",
      TRUE ~ `ELA`
    )
  )

# Reviewing work - finding cases of duplicate IDs

# flags duplicate ids, should be 0
dup_ids <- which(duplicated(course_names_teachers_final$studentid))

# If applicable, make duplicates their own dataframe to tell where mistakes are
duplicates <- course_names_teachers_final[dup_ids, ]

dups <- course_names_teachers_final[dup_ids, ] %>%
  select(studentid) %>%
  unique()


# --------------------- ### Combining Tables - Primary ###--------------------------

primary_course_teacher <- users_names %>%
  left_join(teachers,
    by = "users_dcid"
  ) %>% # joins users and teachers
  left_join(cc,
    by = "teacherid"
  ) %>%
  filter(termid == PS_TERMID) %>% 
  
  # joins users/teachers and cc
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
  
  # joins users/teachers/cc/courses and students
  left_join(students %>% 
    select(student_number,
      studentid = id,
      grade_level,
      student_first = first_name,
      student_last = last_name
    ),
  by = "studentid"
  ) %>%
  filter(grade_level < 4) %>%
  filter(!grepl("Science", course_long_name))


# Adding in subjects to  KOP and KAP 3rd grade
# special circumstance due to distance learning report cards

course_3_teacher <- primary_course_teacher %>%
  filter(
    grade_level == 3,
    !str_detect(course_long_name, "ELL"),
    !str_detect(course_long_name, "IXL")
  ) %>%
  mutate(
    Science = "3rd Science",
    Science_teacher = teacher_full_name,
    Math = "3rd Math",
    `Math Teacher` = teacher_full_name,
    `Math Spanish` = "Matemáticas",
    ELA = "3rd Reading",
    `ELA Teacher` = teacher_full_name
  )
