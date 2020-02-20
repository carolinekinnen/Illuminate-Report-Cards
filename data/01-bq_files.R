
# Pull tables from BigQuery database


#-------------------------- ### Parameters ### ----------------------------------------

# find School Year and Term ID to filter large attendance tables
sy <- calc_academic_year(today(), format = 'firstyear')

ps_termid <- calc_ps_termid(current_first_year)

#-------------------------- ### Tables ###----------------------------------------

# Get students
students <- get_powerschool("students") %>%
  select(id,
         schoolid,
         student_number,
         first_name,
         last_name, 
         home_room,
         enroll_status,
         grade_level) %>%
  collect()

# Get attendance
attendance <- get_powerschool("attendance") %>%
  filter(att_date >= lubridate::ymd("2019-08-19")) %>%
  filter(att_date <= lubridate::ymd("2020-1-25")) %>% # CHANGE DATE TO Q3
  filter(att_mode_code == "ATT_ModeDaily") %>%
  collect()

# Get attendance code table  
attendance_code <- get_powerschool("attendance_code") %>%
  mutate(att_code = if_else(att_code == "true", "T", att_code)) %>% #
  collect()

# Get membership table
membership <- silounloadr::get_powerschool("ps_membership_reg") %>% 
  filter(yearid >= ps_termid/100) %>% 
  select(studentid,
         schoolid,
         date = calendardate,
         enrolled = studentmembership,
         grade_level,
         attendance = ATT_CalcCntPresentAbsent) %>%
  collect() 
