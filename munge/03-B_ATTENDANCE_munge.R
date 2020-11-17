
# Parameters --------------------------------------------------------------

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

rc_quarter_table <- terms %>%
  filter(abbreviation == identify_quarter(today() - 15)) %>%
  select(lastday, firstday) %>%
  unique()

rc_quarter <- identify_quarter(today() - 15)

rc_quarter_first_day <- rc_quarter_table$firstday[1]

rc_quarter_last_day <- rc_quarter_table$lastday[1]

# Daily Attendance - Data Table Munging -----------------------------------

student_daily_attendance <-

  # this table includes only days where students where not first marked as present
  attendance_remote %>%

  # filter to current school year
  filter(behavior_date >= ymd("2020-08-24")) %>%
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
  left_join(membership_remote %>% filter(date >= ymd("2020-08-24")),
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
    `attendance_Absent (Remote)`,
    `attendance_Absent (COVID related)`,
    `attendance_Absent`,
    `attendance_Did Not Arrive`,
    attendance_Hospital
  ) %>%
  distinct() %>%
  mutate(membership = if_else(att_code == "D", 0, membership))



#------------------------ ### Summarize Table ###-------------------------------------


attend_school_grade_student <- student_daily_attendance %>%
  filter(behavior_date <= rc_quarter_last_day) %>%
  group_by(
    student_number,
    student_last_name,
    student_first_name,
    grade_level,
    school_abbr
  ) %>%
  summarize(
    `Days Enrolled (From First Day Present)` = sum(membership),
    `Absences (Remote)` = sum(`attendance_Absent (Remote)`),
    `Absences (Covid Related)` = sum(`attendance_Absent (COVID related)`),
    `Absences (Total)` = (`Absences (Remote)` + `Absences (Covid Related)`),
    `Present (Remote)` = sum(`attendance_Present (Remote)`),
    `Academic Contact` = sum(`attendance_Academic Contact`),
    `Present (Total)` = (`Present (Remote)` + `Academic Contact`),
    ADA = round((`Present (Total)` / `Days Enrolled (From First Day Present)`) * 100, 2)
  ) %>%
  rename(
    enrolled = `Days Enrolled (From First Day Present)`,
    present = `Present (Total)`,
    absent = `Absences (Total)`
  ) %>%
  mutate(quarter = "Q1") %>%
  select(
    school_abbr, grade_level, student_number, student_first_name,
    student_last_name, quarter, enrolled, present,
    absent
  )


enrolled_quarter <- str_c("enrolled_", rc_quarter)

# Pivot table so each attendance category and quarter is seperate row , calculate totals

attend_school_grade_student_totals <- attend_school_grade_student %>%
  pivot_wider(names_from = quarter, values_from = c(enrolled, present, absent)) %>%
  ungroup() %>%
  mutate(
    total_absent = rowSums(.[grep("absent", names(.))], na.rm = TRUE),
    total_present = rowSums(.[grep("present", names(.))], na.rm = TRUE),
    total_enrolled = rowSums(.[grep("enrolled", names(.))], na.rm = TRUE),
    tardy_Q1 = NA,
    total_tardy = NA,
    rate = paste0(round(total_present / total_enrolled * 100, 1), "%")
  )
