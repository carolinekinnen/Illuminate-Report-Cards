
# Parameters --------------------------------------------------------------

Q1 <- "Q1"
Q2 <- "Q2"
Q3 <- "Q3"
Q4 <- "Q4"

CURRENT_QUARTER <- Q2

SY_2021 <- silounloadr::calc_academic_year(ymd("2020-08-24"), format = "firstyear")

# Output: 2020
CURRENT_FIRST_YEAR <- calc_academic_year(lubridate::today(),
  format = "first_year"
)

# Output: 3000
PS_TERMID_2021 <- calc_ps_termid(CURRENT_FIRST_YEAR)

PS_YEARID_2021 <-
  silounloadr::calc_ps_termid(SY_2021) %>%
  str_extract("\\d{2}") %>%
  as.integer()

current_first_year <- calc_academic_year(lubridate::today(),
  format = "first_year"
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

current_quarter_table_dates <- terms %>%
  filter(abbreviation == CURRENT_QUARTER) %>%
  select(lastday, firstday) %>%
  unique()

q1_quarter_table_dates <- terms %>%
  filter(abbreviation == Q1) %>%
  select(lastday, firstday) %>%
  unique()

q2_quarter_table_dates <- terms %>%
  filter(abbreviation == Q2) %>%
  select(lastday, firstday) %>%
  unique()

q3_quarter_table_dates <- terms %>%
  filter(abbreviation == Q3) %>%
  select(lastday, firstday) %>%
  unique()

q4_quarter_table_dates <- terms %>%
  filter(abbreviation == Q4) %>%
  select(lastday, firstday) %>%
  unique()

CURRENT_QUARTER_FIRST_DAY <- current_quarter_table_dates$firstday[1]
CURRENT_QUARTER_LAST_DAY <- current_quarter_table_dates$lastday[1]

Q1_FIRST_DAY <- q1_quarter_table_dates$firstday[1]
Q1_LAST_DAY <- q1_quarter_table_dates$lastday[1]

Q2_FIRST_DAY <- q2_quarter_table_dates$firstday[1]
Q2_LAST_DAY <- q2_quarter_table_dates$lastday[1]

Q3_FIRST_DAY <- q3_quarter_table_dates$firstday[1]
Q3_LAST_DAY <- q3_quarter_table_dates$lastday[1]

Q4_FIRST_DAY <- q4_quarter_table_dates$firstday[1]
Q4_LAST_DAY <- q4_quarter_table_dates$lastday[1]



# Student Daily Attendance ------------------------------------------------

student_daily_attendance <-

  # this table includes only days where students where not first marked as present
  attendance_remote %>%

  # filter to current school year
  filter(behavior_date >= Q1_FIRST_DAY) %>%
  mutate(behavior = trimws(behavior, which = "both")) %>%
  left_join(students_remote %>%
    # set column types
    mutate(
      student_number = as.character(student_number),
      schoolid = as.character(schoolid)
    ),
  by = "student_number"
  ) %>%

  # this table ensures that each student has a row for each day they were enrolled in school.
  # because attendance table only records "special circumstances", membership table is needed
  # to figure out all days students were present.
  left_join(membership_remote %>% filter(date >= Q1_FIRST_DAY),
    by = c(
      "studentid" = "studentid",
      "behavior_date" = "date"
    )
  ) %>%
  left_join(attendance_code_full,
    by = "behavior"
  ) %>%
  mutate(attendance = as.factor(attendance)) %>%

  # filter to current school year
  filter(yearid == 30) %>%

  # filter out students who were DNA (students who signed up but never came to school)

  as.data.table() %>%
  one_hot() %>%
  # filter(`attendance_Did Not Arrive` != 1) %>%
  left_join(school_id_table,
    by = "schoolid"
  ) %>%
  rename(school_abbr = abbr) %>%
  mutate(
    yearid = as.character(yearid),
    student_number = as.character(student_number),
    studentid = as.character(studentid),
    schoolid = as.character(schoolid),
    grade_level = as.numeric(grade_level),
    behavior_date = ymd(behavior_date),
    membership = as.numeric(membership)
  ) %>%

  # reorder columns
  select(
    student_number,
    studentid,
    student_last_name,
    student_first_name,
    grade_level,
    school_abbr,
    schoolid,
    behavior_date,
    att_code,
    membership,
    `attendance_Present (Remote)`,
    `attendance_Academic Contact`,
    `attendance_Tardy (Remote)`,
    `attendance_Present`,
    `attendance_Absent (Remote)`,
    `attendance_Absent (COVID related)`,
    `attendance_Absent`,
    `attendance_Did Not Arrive`,
    `attendance_Hospital`
  ) %>%
  distinct() %>%
  mutate(membership = if_else(att_code == "D", 0, membership), 
         quarter = case_when(behavior_date <= Q1_LAST_DAY ~ "Q1", 
                             behavior_date >= Q2_FIRST_DAY & behavior_date <= Q2_LAST_DAY ~ "Q2", 
                             behavior_date >= Q3_FIRST_DAY & behavior_date <= Q3_LAST_DAY ~ "Q3", 
                             behavior_date >= Q4_FIRST_DAY & behavior_date <= Q3_LAST_DAY ~ "Q4")
         )



#------------------------ ### Summarize Table ###-------------------------------------

attend_school_grade_student_totals <- student_daily_attendance %>%
  filter(behavior_date <= CURRENT_QUARTER_LAST_DAY) %>%
  group_by(
    student_number,
    student_last_name,
    student_first_name,
    grade_level,
    school_abbr, 
    quarter
  ) %>%
  summarize(
    `Absences (Remote)` = sum(`attendance_Absent (Remote)`, na.rm = TRUE),
    `Absences (Covid Related)` = sum(`attendance_Absent (COVID related)`, na.rm = TRUE),
    `Absences` = sum(`attendance_Absent`, na.rm = TRUE),
    `Present (Remote)` = sum(`attendance_Present (Remote)`, na.rm = TRUE),
    `Academic Contact` = sum(`attendance_Academic Contact`, na.rm = TRUE),
    `Present` = sum(`attendance_Present`, na.rm = TRUE),
    total_tardy = sum(`attendance_Tardy (Remote)`, na.rm = TRUE),
    total_present = (`Present (Remote)` + `Academic Contact` + total_tardy + `Present`),
    total_absent = (`Absences (Remote)` + `Absences (Covid Related)` + `Absences`),
    total_enrolled = sum(membership, na.rm = TRUE),
    rate = paste0(round((total_present / total_enrolled) * 100, 1), "%"),
  ) %>%
  filter(total_enrolled != 0) %>%
  select(
    student_number, school_abbr, grade_level, student_last_name, student_first_name, 
    quarter, total_tardy, total_absent, total_present, total_enrolled, rate
  ) %>% 
  pivot_wider(names_from = quarter, values_from = c(total_enrolled, total_present, total_tardy, total_absent, rate)) %>%
  mutate(total_tardy_Q1 = NA) %>%
  arrange(school_abbr, grade_level)
