
# Identification ----------------------------------------------------------

bq_project <- Sys.getenv("BQ_PROJECT")
bq_auth_file <- Sys.getenv("BQ_AUTH_FILE")

bigrquery::bq_auth(path = bq_auth_file)
Sys.getenv("BQ_AUTH_FILE")

googleAuthR::gar_auth_service("/usr/local/etc/gcs/kipp-chicago-silo-2-3789ce3e3415.json")
gcs_global_bucket("idea_remote_attendance_dashboard")

#-------------------------- ### Parameters ### ----------------------------------------

# find School Year and Term ID to filter large attendance tables
sy <- calc_academic_year(today(), format = "firstyear")

current_first_year <- calc_academic_year(lubridate::today(),
  format = "first_year"
)

current_last_year <- calc_academic_year(today(),
  format = "second_year"
)

ps_termid <- calc_ps_termid(current_first_year)

terms <- get_powerschool("terms") %>%
  filter(id >= ps_termid) %>%
  select(
    id,
    abbreviation,
    firstday,
    lastday
  ) %>%
  collect() %>%
  unique() %>%
  arrange(id)

sy_abbreviation <- terms$abbreviation[1]

terms_last_year <- get_powerschool("terms") %>%
  filter(id == ps_termid - 100) %>%
  select(
    id,
    abbreviation,
    firstday,
    lastday
  ) %>%
  collect() %>%
  unique() %>%
  arrange(id)

sy_abbreviation_last_year <- terms_last_year$abbreviation[1]

# Last day of quarter in which report cards are being generated
rc_quarter_table <- terms %>%
  filter(abbreviation == identify_quarter(today() - 15)) %>%
  select(lastday, firstday) %>%
  unique()

rc_quarter_first_day <- rc_quarter_table$firstday[1]

rc_quarter_last_day <- rc_quarter_table$lastday[1]

rc_quarter <- identify_quarter(today() - 15)

rc_quarter_number <- as.double(str_extract(rc_quarter, "[1-4]"))

year_table <- terms %>%
  filter(id == ps_termid)

year_term_id <- year_table$id

year_first_day <- year_table$firstday

calculated_type <- case_when(
  today() - 5 <= ymd(rc_quarter_last_day) ~ "first_upload",
  # if it's less than 5 days after the quarter ended:
  # designation is first_upload
  # will eventually use to decide whether or not to include DL students in normal GPA and final grade calculation
  TRUE ~ "not_first_upload" # if not designation is not_first_upload,
  # will eventually use to decide whether or not to run seperate script for DL vs gen ed
)

# MAP term names - Current Year
termname_map_winter <- paste0("Winter ", current_first_year, "-", current_last_year)
termname_map_spring <- paste0("Spring ", current_first_year, "-", current_last_year)

# MAP term names - Last Year
# used in KTC_data

termname_map_last_spring <- paste0("Spring ", current_first_year - 1, "-", current_last_year - 1)


termname_map_last_spring <- paste0("Spring ", current_first_year - 1, "-", current_last_year - 1)

#-------------------------- ### Attendance Tables ###------------------------------------

# School table
schools <- tibble(
  schoolid = c(
    78102, 7810, 400146, 4001462, 400163,
    4001802, 400180, 4001632
  ),
  schoolname = c(
    "Ascend Primary", "Ascend Middle", "Academy Chicago",
    "Academy Chicago Primary", "Bloom", "One Primary",
    "One Academy", "Bloom Primary"
  ),
  schoolabbreviation = c("KAP", "KAMS", "KAC", "KACP", "KBCP", "KOP", "KOA", "KBP")
)


# Get students

## Powerschool ##

students <- get_powerschool("students") %>%
  select(
    id,
    schoolid,
    student_number,
    first_name,
    last_name,
    home_room,
    enroll_status,
    grade_level
  ) %>%
  collect()

students_remote <- get_powerschool("students") %>%
  select(student_number,
    studentid = id,
    enroll_status,
    grade_level,
    home_room,
    schoolid,
    first_name,
    last_name,
  ) %>%

  # filter for currently enrolled students
  filter(enroll_status == 0) %>%
  collect(page_size = 2000) %>%
  mutate(home_room = trimws(home_room, which = "both"))



attendance_remote <- get_deanslist("daily_attendance") %>%
  select(
    modified_attendance = `_modified`,
    student_number = student_school_id,
    student_first_name,
    student_last_name,
    behavior_date,
    behavior
  ) %>%
  collect(page_size = 2000) %>%
  mutate(student_number = as.character(student_number))

attendance_code_remote <-
  get_powerschool("attendance_code") %>%
  mutate(att_code = if_else(att_code == "true", "T", att_code)) %>%
  select(
    att_code,
    description
  ) %>%
  collect() %>%
  mutate(description = trimws(description, which = "both"))


membership_remote <-
  get_powerschool("ps_membership_reg") %>%
  select(studentid,
    schoolid,
    date = calendardate,
    membership = studentmembership,
    grade_level,
    attendance = ATT_CalcCntPresentAbsent,
    yearid
  ) %>%
  collect() %>%
  select(-c(
    grade_level,
    attendance,
    schoolid
  ))



students_ktc <- get_powerschool("students") %>%
  select(schoolid,
    student_id = student_number,
    dob,
    gender,
    guardianemail,
    home_phone,
    street,
    city,
    state,
    zip
  ) %>%
  collect() %>%
  mutate(address = paste(street, city, state, zip, sep = ", ")) %>%
  select(-c(street, city, zip, state))

# Get attendance
attendance <- get_powerschool("attendance") %>%
  filter(att_date >= lubridate::ymd(year_first_day)) %>%
  filter(att_date <= lubridate::ymd(rc_quarter_last_day)) %>%
  filter(att_mode_code == "ATT_ModeDaily") %>%
  collect()

# Get attendance code table
attendance_code <- get_powerschool("attendance_code") %>%
  # mutate(att_code = if_else(att_code == "true", "T", att_code)) %>% #
  collect()

# Get membership table
membership <- silounloadr::get_powerschool("ps_membership_reg") %>%
  filter(yearid >= ps_termid / 100) %>%
  select(studentid,
    schoolid,
    date = calendardate,
    enrolled = studentmembership,
    grade_level,
    attendance = ATT_CalcCntPresentAbsent
  ) %>%
  collect()

#-------------------------- ### Enrollment ###----------------------------------------

enrollment <- get_powerschool("ps_enrollment_all") %>%
  filter(exitdate >= rc_quarter_last_day) %>%
  collect()

student_enroll <- enrollment %>%
  left_join(students %>%
    rename(studentid = id) %>% select(-schoolid),
  by = "studentid"
  ) %>%
  group_by(studentid) %>%
  filter(entrydate == max(entrydate))

#-------------------------- ### Course/Teacher Tables ###----------------------------------------

# Get users table
users <- get_powerschool("users") %>%
  select(first_name,
    last_name,
    homeschoolid,
    users_dcid = dcid
  ) %>%
  collect()

# Get teachers table
teachers <- get_powerschool("schoolstaff") %>%
  select(
    teacherid = id,
    users_dcid
  ) %>%
  collect()


# Get courses table
courses <- get_powerschool("courses") %>%
  select(
    course_long_name = course_name,
    course_number
  ) %>%
  collect()

#-------------------------- ### Powerschool Transcript Tables ###----------------------------------

# needed for transcripts and 8th grade promotion data

students_powerschool_transcripts <- get_powerschool("students") %>%
  select(
    student_id = student_number,
    ps_id = id,
    grade_level,
    ps_schoolid = schoolid
  ) %>%
  collect()

storedgrades <- get_powerschool("storedgrades",
  collect = TRUE
)

pgfinalgrades <- get_powerschool("pgfinalgrades",
  collect = TRUE
)


# ----------------------- ### Illuminate Primary Gradebook tables for Retention data ### ----------------
# Need to pull from Illuminate gradebooks instead of report cards because report cards don't have grades in A/B/C/D scale

# Quarter grades for K-2 students
q_grades_primary <- get_illuminate("overall_score_cache", schema = "gradebook") %>%
  filter(
    timeframe_start_date >= rc_quarter_first_day,
    timeframe_end_date <= rc_quarter_last_day
  ) %>%
  select(
    gradebook_id,
    calculated_at,
    mark,
    percentage,
    student_id,
    timeframe_end_date,
    timeframe_start_date
  ) %>%
  collect(n = Inf)


# ------------------------ ### Unique Schools/Grade Tables for Reading in Files ### ----------------------

# Unique list of schools and grade levels of currently enrolled students in students table
schools_grades <- students %>%
  filter(enroll_status == 0) %>%
  left_join(schools, by = "schoolid") %>%
  distinct(schoolabbreviation, grade_level) %>%
  filter(grade_level > 2)

# Schools_grades into lists for map2 fucntion
schools_map <- schools_grades %>%
  pull(schoolabbreviation) %>%
  as.character()

grades_map <- schools_grades %>% pull(grade_level)


# ------------------------ ### Tables to find Quarter Grades for Primary Students #### --------------
# need for retention file

students_primary <- get_powerschool("students") %>%
  select(
    studentid = id,
    student_number,
    first_name,
    last_name,
    grade_level,
    dob,
    home_room,
    enroll_status,
    schoolid
  ) %>%
  collect()

# get gradebooks
gradebooks <- get_illuminate("gradebooks", schema = "gradebook") %>%
  filter(academic_year == current_last_year) %>%
  select(
    gradebook_id,
    created_by,
    gradebook_name,
    active,
    is_deleted,
    academic_year
  ) %>%
  filter(
    active,
    !is_deleted
  ) %>%
  collect()

# get illuminate students
ill_students_primary <- get_illuminate("students", "public") %>%
  select(
    student_id,
    local_student_id
  ) %>%
  collect()

# get sections corresponding to grade books
gradebook_sections <- get_illuminate("gradebook_section_course_aff", schema = "gradebook") %>%
  select(gradebook_id,
    ill_sec_id = section_id,
    user_id
  ) %>%
  collect()

# Illuminate sections
illuminate_sec <- get_illuminate(
  "sections",
  "public"
) %>%
  select(
    ill_sec_id = section_id,
    ps_sec_id = local_section_id
  ) %>%
  collect()

illuminate_teacher_sec <- get_illuminate("section_teacher_aff",
  schema = "public"
) %>%
  select(
    ill_sec_id = section_id,
    sec_user = user_id,
    primary_teacher
  ) %>%
  collect()
