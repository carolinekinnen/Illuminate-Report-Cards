# Pull tables from BigQuery database

# Identification ----------------------------------------------------------

bq_project <- Sys.getenv("BQ_PROJECT")
bq_auth_file <- Sys.getenv("BQ_AUTH_FILE")

bigrquery::bq_auth(path = bq_auth_file)
Sys.getenv("BQ_AUTH_FILE")

googleAuthR::gar_auth_service("/usr/local/etc/gcs/kipp-chicago-silo-2-3789ce3e3415.json")
gcs_global_bucket("idea_remote_attendance_dashboard")


# BQ TABLES ---------------------------------------------------------------

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