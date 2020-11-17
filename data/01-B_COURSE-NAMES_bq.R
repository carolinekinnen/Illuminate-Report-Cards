
# Pull tables from BigQuery database

# Identification ----------------------------------------------------------

bq_project <- Sys.getenv("BQ_PROJECT")
bq_auth_file <- Sys.getenv("BQ_AUTH_FILE")

bigrquery::bq_auth(path = bq_auth_file)
Sys.getenv("BQ_AUTH_FILE")

googleAuthR::gar_auth_service("/usr/local/etc/gcs/kipp-chicago-silo-2-3789ce3e3415.json")
gcs_global_bucket("idea_remote_attendance_dashboard")

# PARAMETERS --------------------------------------------------------------

# Get teachers table
teachers <- get_powerschool("schoolstaff") %>%
  select(
    teacherid = id,
    users_dcid
  ) %>%
  collect()

# Get CC table
cc <- get_powerschool("cc") %>%
  select(
    course_number,
    section_number,
    teacherid,
    termid,
    schoolid,
    studentid,
    dateleft,
    sectionid
  ) %>%
  collect()

# Get courses table
courses <- get_powerschool("courses") %>%
  select(
    course_long_name = course_name,
    course_number
  ) %>%
  collect()

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

# Get users table
users <- get_powerschool("users") %>%
  select(first_name,
         last_name,
         homeschoolid,
         users_dcid = dcid
  ) %>%
  collect()


