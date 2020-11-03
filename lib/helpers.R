
# ----------------------- ### Identify Quarter Function ### -----------------

identify_quarter <- function(date) {
  case_when(
    date >= terms$firstday[1] & date < terms$lastday[4] ~ "Q1",
    date >= terms$firstday[3] & date < terms$lastday[5] ~ "Q2",
    date >= terms$firstday[4] & date < terms$lastday[6] ~ "Q3",
    date >= terms$firstday[5] & date <= terms$lastday[7] ~ "Q4",
    TRUE ~ "not in SY")
}


# -----------------------### Read in Report Card Files from GCS Bucket Function ### -----------------

get_objects_report_card <- function(school, grade) {
  grade <- as.character(grade)
  
  file_name <- paste0("Illuminate-Report-Cards/Illuminate-Grades/", 
                      sy_abbreviation, "/", school, "_", grade, ".csv")
  file_save_to <- paste0("data/flatfiles/rc_export/", 
                         school, "_", grade, ".csv") 
  
  gcs_get_object(file_name,
                 saveToDisk = file_save_to,
                 overwrite = TRUE)
}

get_objects_roster_links <- function(school) {
  file_name <- paste0("Illuminate-Report-Cards/DL_roster_links/",
                      sy_abbreviation, "_roster_links_", school, ".csv")
  file_save_to <- paste0("data/flatfiles/DL_roster_links/",
                         sy_abbreviation, "_roster_links_", school, ".csv")
  
  gcs_get_object(file_name,
                 saveToDisk = file_save_to,
                 overwrite = TRUE)
}

# ----------------------- ### Read in DL Students from Google Sheets Function ### --------------------

ws <- function(df_title, grade_level){
  read_sheet(df_title$id,
             sheet = grade_level) %>%
    select(2) %>% 
    janitor::clean_names()
}

# if ws isn't working check that id_number column in Google Sheet only contains integer values, not characters

# ---------------- ### Fixing Incorrect Column Names from Illuminate before Grades Functions ### ----------------
# Fixing Homework and Behavior Grade/Percent Problem
# Fields were named incorrectly - have to contain either "Grade" or "Percent", but these contain both
# removing so grade and percent function will work

behavior_homework_name_fixer <- function(df, grade, school){
  grade <- toOrdinal::toOrdinal(as.integer(grade))
  
  df_fixed <- df %>%
    dplyr::rename("sy19_20_rc_{ school }_{ grade }_q1_3_4_behavior_percent" := glue::glue("sy19_20_rc_{ school }_{ grade }_q1_3_4_behavior_grade_behavior_percent"),
           "sy19_20_rc_{ school }_{ grade }_q1_3_4_homework_homework_percent" := glue::glue("sy19_20_rc_{ school }_{ grade }_q1_3_4_homework_grade_homework_percent"),
           "sy19_20_rc_{ school }_{ grade }_q2_3_4_behavior_percent" := glue::glue("sy19_20_rc_{ school }_{ grade }_q2_3_4_behavior_grade_behavior_percent"),
           "sy19_20_rc_{ school }_{ grade }_q2_3_4_homework_homework_percent" := glue::glue("sy19_20_rc_{ school }_{ grade }_q2_3_4_homework_grade_homework_percent"),
           "sy19_20_rc_{ school }_{ grade }_q3_3_4_behavior_percent" := glue::glue("sy19_20_rc_{ school }_{ grade }_q3_3_4_behavior_grade_behavior_percent"),
           "sy19_20_rc_{ school }_{ grade }_q3_3_4_homework_homework_percent" := glue::glue("sy19_20_rc_{ school }_{ grade }_q3_3_4_homework_grade_homework_percent"),
           "sy19_20_rc_{ school }_{ grade }_q4_3_4_behavior_percent" := glue::glue("sy19_20_rc_{ school }_{ grade }_q4_3_4_behavior_grade_behavior_percent"),
          "sy19_20_rc_{ school }_{ grade }_q4_3_4_homework_homework_percent" := glue::glue("sy19_20_rc_{ school }_{ grade }_q4_3_4_homework_grade_homework_percent")
    ) 
  
  df_fixed
}

behavior_homework_name_fixer_by_row <- function(input_dataframe, iteration){
  #grade <- input_dataframe %>% pull(grade)
  grade <- input_dataframe$grade[[iteration]]
  #school <- input_dataframe %>% pull(school)
  school <- input_dataframe$school[[iteration]]
  data <- input_dataframe$df[[iteration]]
  behavior_homework_name_fixer(data, grade, school)
}


# ----------------------- ### Transform File into Grades Functions ### --------------------

get_q_grades_pct <- function(data, grade_type = "grade", rc_quarter_input){
  # data = the data we pass in
  #grade_type should either be "grade" or "percent"
  grade_type_eval <- sym(grade_type)
  
  data %>% 
    select(site_id,
           student_id,
           contains(grade_type),
           -contains("y1_avg")) %>% 
    tidyr::gather(rc_field, !!grade_type_eval, -site_id:-student_id) %>%
    mutate(
      # subject0 = gsub("(^.+th_)(\\w.+)(_.+)", "\\2", rc_field),
      #      subject1 = gsub("q\\d_", "", subject0),
      #      subject1 = gsub("_", " ", subject1),
      #      subject3 = gsub("^(\\S*\\s+\\S+).*", "\\1", subject1),
           subject = str_extract(rc_field, "choice_reading|guided_reading|behavior|math|reading|step|homework|science|musical_theater|writing|art|ela|pe|lit_centers|social_studies|explorations|dance|pre_algebra|algebra|physical_education"),
           course_school = toupper(stringr::str_extract(rc_field, "kap|kop|kacp|kac|kbcp|kams|koa"))) %>%
  #  tidyr::separate("subject3", c("sub1", "sub2"), sep = " ") %>%
    mutate(subject = case_when(
        subject == "lit_centers" ~ "lit centers",
        subject == "social_studies" ~ "social",
        subject == "physical_education" ~ "pe",
        TRUE ~ subject),
      store_code = toupper(str_extract(rc_field, "q\\d"))) %>%
    left_join(schools %>% dplyr::rename(site_id = schoolid), by = "site_id") %>%
    filter(schoolabbreviation == course_school) %>%
     select(-c(schoolname, schoolabbreviation, rc_field)) %>%  
    #           subject0:sub2,
    #           one_subj:subject8,
    #           rc_field)) %>%
    filter(!is.na(!!grade_type_eval)) %>%
    filter(store_code == rc_quarter_input)
}

# Makes the final grade whatever is in the Quarter-Y1Avg Field in Illuminate export
# necessary after final grades are calculated and uploaded once so GPA will reflect grade modifications

get_yavg_grades <- function(data){
  
  data %>% 
    select(site_id,
           student_id,
           contains(tolower(rc_quarter)),
           -contains("percent")) %>%
    select(site_id, student_id, contains("y1_avg")) %>% 
    tidyr::gather(rc_field, grade, -site_id:-student_id) %>% 
    mutate(subject = str_extract(rc_field, "choice_reading|guided_reading|behavior|math|reading|step|homework|science|musical_theater|writing|art|ela|pe|lit_centers|social_studies|explorations|dance|pre_algebra|algebra|physical_education"),    # have to add new subjects
           course_school = toupper(stringr::str_extract(rc_field, "kop|kap|kacp|kac|kbcp|kams|koa"))) %>% 
    select(-c(rc_field)) %>%
    left_join(schools %>% dplyr::rename(site_id = schoolid), by = "site_id") %>% 
    filter(course_school == schoolabbreviation) %>% 
    filter(course_school %in% c("KOP", "KAP", "KACP", "KAC", "KBCP", "KAMS", "KOA")) %>%
    filter(!is.na(grade)) %>%
    mutate(subject = case_when(
      subject == "social" ~ "social studies",
      subject == "lit_centers" ~ "lit centers",
      TRUE ~ subject
    ))
}


# ----------------------- ### Transform File into GPAs Functions ### --------------------

# Function to always round 0.5 up
round2 <- function(x, digits = 0) {  
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^digits
}
