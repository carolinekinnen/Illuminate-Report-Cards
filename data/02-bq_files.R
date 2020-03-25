
# Pull tables from BigQuery database


#-------------------------- ### Parameters ### ----------------------------------------

# find School Year and Term ID to filter large attendance tables
sy <- calc_academic_year(today(), format = 'firstyear')

current_first_year <- calc_academic_year(lubridate::today(),
                                         format = "first_year")

ps_termid <- calc_ps_termid(current_first_year)

terms <- get_powerschool("terms") %>%
  filter(id >= ps_termid) %>%
  select(id,
         abbreviation,
         firstday,
         lastday) %>%
  collect()  %>%
  unique()

sy_abbreviation <- terms$abbreviation[1]

# Last day of quarter in which report cards are being generated 
rc_quarter_table <- terms %>% 
  filter(abbreviation == identify_quarter(today() - 15)) %>%
  select(lastday, firstday)

rc_quarter_first_day <- rc_quarter_table$firstday

rc_quarter_last_day <- rc_quarter_table$lastday

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
  TRUE ~ "not_first_upload"    # if not designation is not_first_upload, 
                              # will eventually use to decide whether or not to run seperate script for DL vs gen ed
)

# MAP term names 
termname_map <- "Winter 2019-2020"

#calculate first year of academic year using silounloadr
current_first_year <- calc_academic_year(today(), format = "first_year") 

# calculate last year of academic year
current_last_year <- calc_academic_year(today(), format = "second_year")

#-------------------------- ### Attendance Tables ###------------------------------------

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
  filter(yearid >= ps_termid/100) %>% 
  select(studentid,
         schoolid,
         date = calendardate,
         enrolled = studentmembership,
         grade_level,
         attendance = ATT_CalcCntPresentAbsent) %>%
  collect() 

#-------------------------- ### Enrollment ###----------------------------------------

enrollment <- get_powerschool("ps_enrollment_all") %>% 
  filter(exitdate >= rc_quarter_last_day) %>%
  collect()

student_enroll <- enrollment %>% 
  left_join(students %>% 
            dplyr::rename(studentid = id) %>% select(-schoolid),
            by = "studentid") %>% 
  group_by(studentid) %>% 
  filter(entrydate == max(entrydate))

#-------------------------- ### Course/Teacher Tables ###----------------------------------------

# Get users table
users <- get_powerschool("users") %>%
  select(first_name,
         last_name,
         homeschoolid,
         users_dcid = dcid) %>%
  collect()

# Get teachers table
teachers <- get_powerschool("schoolstaff") %>%
  select(teacherid = id,
         users_dcid) %>%
  collect()

# Get CC table 
cc <- get_powerschool("cc") %>%
  select(course_number,
         section_number,
         teacherid,
         termid,
         schoolid,
         studentid,
         dateleft,
         sectionid) %>%
  collect()

cc_powerschool_transcript <- get_powerschool("cc") %>%
  collect()

# Get courses table
courses <- get_powerschool("courses") %>%
  select(course_long_name = course_name,
         course_number) %>%
  collect()

course_section <-  cc %>% 
  filter(termid == ps_termid) %>%
  left_join(courses,
            by = "course_number") %>% 
  left_join(students %>% 
            dplyr::rename(studentid = id) %>% select(-schoolid),
            by = "studentid") %>% 
  select(student_number, 
        course_long_name, 
         sectionid,) %>%
  mutate(course_long_name = case_when(
    str_detect(course_long_name, "ELA") ~ gsub("ELA", "English Language Arts", course_long_name),
    course_long_name == "4th Math" ~ "4th Mathematics", 
    course_long_name == "5th Math" ~ "5th Mathematics", 
    course_long_name == "5th Literacy Center" ~ "5th Literacy Centers",
    course_long_name == "6th Math" ~ "6th Mathematics",
    course_long_name == "7th Math" ~ "7th Mathematics",
    course_long_name == "8th Math" ~ "8th Mathematics",
    TRUE ~ course_long_name
  ))

#-------------------------- ### Powerschool Transcript Tables ###----------------------------------

students_powerschool_transcripts <- get_powerschool("students") %>%
  select(student_id = student_number,
         ps_id = id,
         grade_level, 
         ps_schoolid = schoolid) %>%
  collect()

# ------------------------- ### MAP Data for Retention and KTC ### ---------------------------

# All Winter MAP scores
map_winter <- get_nwea_map("cdf_combined_kipp_cps") %>%
  select(student_id,
         term_name,
         test_ritscore,
         test_percentile,
         measurement_scale) %>%
  filter(term_name == termname_map) %>%
  collect()

map_winter_pivot <- map_winter %>%
  pivot_wider(names_from = measurement_scale, values_from = c(test_ritscore, test_percentile))

map_winter_pivot_students <- map_winter_pivot %>%
  left_join(students %>%
            dplyr::rename(student_id = student_number), by = "student_id") %>%
  left_join(schools %>% select(-schoolname), by = "schoolid")

# ------------------------ ### Unique Schools/Grade Tables for Reading Files ### ----------------------

# Unique list of schools and grade levels of currently enrolled students in students table
schools_grades <- students %>%
  filter(enroll_status == 0) %>% 
  left_join(schools, by = "schoolid") %>%
  distinct(schoolabbreviation, grade_level) %>%
  filter(grade_level > 2)

# Schools_grades into lists for map2 fucntion
schools_map <- schools_grades %>% pull(schoolabbreviation) %>% as.character()
grades_map <- schools_grades %>% pull(grade_level)


# ------------------------ ### Tables to find Quarter Grades for Primary Students #### --------------
# need for retention file

students_primary <- get_powerschool("students") %>%
  select(studentid = id,
         student_number,
         first_name, 
         last_name,
         grade_level,
         dob,
         home_room,
         enroll_status,
         schoolid) %>%
  collect()

# unique cc table
cc_unique <- cc %>%
  filter(termid %in% c(ps_termid, (-1*ps_termid))) %>% # why does this work
  select(course_number,
         section_number,
         sectionid) %>%
  unique()

# get gradebooks
gradebooks <- get_illuminate("gradebooks", schema = "gradebook") %>% 
  filter(academic_year == current_last_year) %>% 
  select(gradebook_id,
         created_by,
         gradebook_name, 
         active,
         is_deleted,
         academic_year) %>%
  filter(active,
         !is_deleted)
  collect() 

# get illuminate students
ill_students_primary <- get_illuminate("students", "public") %>%
  select(student_id,
         local_student_id) %>%
  collect()

#get sections corresponding to grade books
gradebook_sections <- get_illuminate("gradebook_section_course_aff", schema = "gradebook") %>%
  select(gradebook_id,
         ill_sec_id = section_id,
         user_id) %>%
  collect()

#Illuminate sections
illuminate_sec <- get_illuminate("sections",
                                 "public") %>%
  select(ill_sec_id = section_id,
         ps_sec_id = local_section_id) %>%
  collect()

illuminate_teacher_sec <- get_illuminate("section_teacher_aff",
                                         schema = "public") %>%
  select(ill_sec_id = section_id,
         sec_user = user_id,
         primary_teacher) %>%
  collect()

# ---------------- ### KOA Report Card Comments - Gradebook Categories ### -----------------------

categories <- get_illuminate("categories", schema = "gradebook") %>%
  select(category_id,
         category_name,
         gradebook_id,
         weight) %>%
  collect()

category_score_cache <- get_illuminate("category_score_cache", schema = "gradebook") %>%
  select(calculated_at,
         timeframe_start_date,
         timeframe_end_date,
         category_id,
         category_name,
         gradebook_id,
         mark,
         percentage,
         student_id) %>%
  filter(timeframe_start_date == rc_quarter_first_day) %>%
  collect()

illuminate_students <- get_illuminate("students", schema = "public") %>%
  select(student_id,
         first_name,
         last_name,
         student_number = local_student_id) %>%
  collect()

assignments <- get_illuminate("assignments", schema = "gradebook") %>%
  select(assignment_id,
         category_id,
         gradebook_id,
         long_name,
         short_name,
         due_date,
         possible_points,
         possible_score) %>% 
  filter(due_date >= rc_quarter_first_day) %>%
  collect()

scores <- get_illuminate("scores", schema = "gradebook") %>%
  select(student_id,
         score_id,
         value,
         assignment_id,
         gradebook_id,
         is_excused,
         created) %>%
  filter(created >= year_first_day) %>%
  collect()



