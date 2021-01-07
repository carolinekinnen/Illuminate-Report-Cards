
# Pull tables from BigQuery database

# Pull tables from BigQuery database
#-------------------------- ### Parameters ### ----------------------------------------

# find School Year and Term ID to filter large attendance tables
rc_quarter <- "Q2"

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

# Last day of quarter in which report cards are being generated
rc_quarter_table <- terms %>%
  filter(abbreviation == rc_quarter) %>%
  select(lastday, firstday) %>%
  unique()

rc_quarter_first_day <- rc_quarter_table$firstday[1]

rc_quarter_last_day <- rc_quarter_table$lastday[1]

rc_quarter_number <- as.double(str_extract(rc_quarter, "[1-4]"))

year_table <- terms %>%
  filter(id == ps_termid)

year_term_id <- year_table$id

year_first_day <- year_table$firstday

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
# cc <- get_powerschool("cc") %>%
#   select(
#     course_number,
#     section_number,
#     teacherid,
#     termid,
#     schoolid,
#     studentid,
#     dateleft,
#     sectionid
#   ) %>%
#   collect()

cc <- read_csv(here::here("data", "flatfiles", "cc.csv"))

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



