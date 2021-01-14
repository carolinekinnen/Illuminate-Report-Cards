# Grades Munge
# cleaning and transforming files from Illuminate for Illuminate, Deans List, and Powerschool


# CONSTANTS ---------------------------------------------------------------

QUARTER_1 = "Q1"
QUARTER_2 = "Q2"
QUARTER_3 = "Q3"
QUARTER_4 = "Q4"

CURRENT_QUARTER_NUMBER <- QUARTER_2

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


# Current Quarter Report Card Data ----------------------------------------

kc_grades_distinct <- 
  kc_grades_full %>%
  
  select(-c(Teacher, `Section ID`)) %>% 
  distinct() %>%
  filter(!is.na(Percentage))

current_quarter_grades <-
  kc_grades_distinct %>%
  
  # START
  #  ___ _ __ __ _ ___  ___
  # / _ \ '__/ _` / __|/ _ \
  # |  __/ | | (_| \__ \  __/
  #  \___|_|  \__,_|___/\___|
 
  # Filter out attendance. We do not put a grade in for attendance
  filter(!grepl("Attendance", `Course Name`)) %>%
  
  # END
  #  ___ _ __ __ _ ___  ___
  # / _ \ '__/ _` / __|/ _ \
  # |  __/ | | (_| \__ \  __/
  #  \___|_|  \__,_|___/\___|
  drop_na("Mark") %>%
  mutate(
    Percentage = as.character(Percentage),
    Percentage = trimws(Percentage, which = "both")
  ) %>%
  
  # START
  #  ___ _ __ __ _ ___  ___
  # / _ \ '__/ _` / __|/ _ \
  # |  __/ | | (_| \__ \  __/
  #  \___|_|  \__,_|___/\___|
  
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
  
  # END
  #  ___ _ __ __ _ ___  ___
  # / _ \ '__/ _` / __|/ _ \
  # |  __/ | | (_| \__ \  __/
  #  \___|_|  \__,_|___/\___|
  
  left_join(student_schools,
    by = c("Student ID" = "student_id")
  ) %>%
  select(-c(ps_schoolid, schoolname, schoolabbreviation)) %>%
  mutate(Mark = as.character(Mark)) %>%
  
  # Add grades w. pluses and minus
  left_join(grade_percent_scale,
    by = c("Percentage" = "percent")
  ) %>%

  # Remove Mark (which has no + or -)
  select(-Mark) %>%
  mutate(Percentage = as.numeric(Percentage)) %>%
  
  left_join(course_names_teachers %>% rename(
    student_id = student_number,
    course_school = schoolabbreviation,
    site_id = schoolid
  ) %>%
    select(-grade_level),
  by = c(
    "Student ID" = "student_id",
    "Course ID" = "course_number"
  )
  ) %>%
  left_join(students %>%
    select(`Student ID` = student_number, first_name, last_name, grade_level),
  by = "Student ID"
  ) %>%
  mutate(quarter = CURRENT_QUARTER_NUMBER) %>%

  # We only calculate grades for 4th grade and up
  filter(grade_level > 3) %>%
  select(
    site_id,
    school_abbr = course_school,
    `Student ID`,
    last_name,
    first_name,
    grade_level,
    studentid,
    course_long_name,
    subject,
    `Course ID`,
    grade,
    percentage = Percentage,
    home_room,
    teacher_full_name,
    GradeBook,
    `Grading Period`,
    quarter,
    `Score Last Updated`
  ) %>%
  mutate(subject = if_else(subject == "social", "social_studies", subject)) %>%
  
  # set any percent above 100 equal to A+
  mutate(grade = if_else(percentage > 100, "A+", grade)) %>%
  distinct() %>%
  arrange(`Student ID`) %>%
  mutate(`Score Last Updated` = lubridate::mdy(`Score Last Updated`)) %>%
  group_by(`Student ID`, subject) %>%
  slice(which.max(`Score Last Updated`)) %>%
  ungroup(subject) %>%
  distinct()
  
