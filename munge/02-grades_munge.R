# Grades Munge
# cleaning and transforming files from Illuminate for Illuminate, Deans List, and Powerschool

#-------------------------- ### Illuminate Report Card Data for Powerschool and Deans List ###----------------------------------
# Old - shouldn't need in 20-21 because no longer using these subjects but leaving just in case
# Fixing Homework and Behavior Grade/Percent Problem
# Fields were named incorrectly - have to contain either "Grade" or "Percent", but these contain both
# removing so grade and percent function will work

# Figure out which lists in the dataframe contain KAP 3, KAP 4, KOP 3, and KOP 4
# 
# kap_kop <- grade_df_df %>%
#   filter(str_detect(file_name, "KAP_|KOP_")) %>%
#   mutate(grade = str_extract(file_name, "\\d")) %>%
#   mutate(school = tolower(str_extract(file_name, "KAP|KOP")))
# 
# # Anti-join to remove original files from grade_df_df
# not_kap_kop <- grade_df_df %>%
#   filter(!str_detect(file_name, "KAP_|KOP_"))
# 
# # Apply column name fixing function to KAP and KOP
# # Comment out KOP_4 in 20-21 when grade level exists
# 
# kap_kop_fixed <- tribble(
#   ~file_name, ~df,
#   "KAP_3.csv", behavior_homework_name_fixer_by_row(kap_kop, 1),
#   "KAP_4.csv", behavior_homework_name_fixer_by_row(kap_kop, 2),
#   "KOP_3.csv", behavior_homework_name_fixer_by_row(kap_kop, 3),
#   # "KOP_4.csv", behavior_homework_name_fixer_by_row(kap_kop, 4)
# )
# 
# # Bind rows to join back to original dataframe of dataframes
# grade_df_df <- not_kap_kop %>%
#   bind_rows(kap_kop_fixed)

#-------------------------- ### Course Names for Grades 4-8 ###----------------------------------

course_names <- course_names_teachers %>%
  select(
    site_id = schoolid,
    student_id = student_number,
    course_long_name,
    course_number
  ) %>%
  left_join(student_schools, by = "student_id") %>%
  mutate(store_code = rc_quarter) %>%
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

#-------------------------- ### Current Quarter Report Card Data for Powerschool and Deans List and Retention Data and KTC ###----------------------------------

rc_letter_grades <- grade_df_df[[2]] %>% # grade_df_list %>%
  map_df(
    .f = get_q_grades_pct,
    grade_type = "grade",
    rc_quarter_input = rc_quarter
  ) %>%
  left_join(student_schools,
    by = "student_id"
  ) %>%
  filter(course_school == schoolabbreviation) %>%
  select(-c(ps_schoolid, schoolname, schoolabbreviation))

rc_percent <- grade_df_df[[2]] %>% # grade_df_list %>%
  map_df(
    .f = get_q_grades_pct,
    grade_type = "percent",
    rc_quarter_input = rc_quarter
  ) %>%
  left_join(student_schools,
    by = "student_id"
  ) %>%
  filter(course_school == schoolabbreviation) %>%
  select(-c(ps_schoolid, schoolname, schoolabbreviation))

quarter_grades <- rc_letter_grades %>%
  # left_join(course_names_teachers %>% rename(student_id = student_number,
  #                                            course_school = schoolabbreviation,
  #                                            site_id = schoolid) %>% filter(!str_detect(subject, "3rd")),
  #          by = c("student_id",
  #                 #"store_code",
  #                "course_school",
  #                 "site_id",
  #                 "subject")) %>% 
  # not working with courses, can't remember where this was used
  left_join(rc_percent,
            by = c("student_id",
                   "store_code",
                   "course_school",
                   "site_id", 
                   "subject")) %>% 
  mutate(percent = as.double(gsub("%", "", percent))) %>%
  left_join(students %>%
              select(student_id = student_number, first_name, last_name), by = "student_id") %>%
  filter(grade_level > 3)

quarter_grades_pivot_wide <- quarter_grades %>%
  select(student_id,
         #store_code,
         subject,
         grade, 
         percent) %>%
  group_by(subject,
           # student_id,
  ) %>%
  # mutate(row = row_number()) %>%
  pivot_wider(names_from = subject,
              values_from = c(grade, percent)) %>%
  unnest(cols = c(grade_ela, `grade_lit centers`, grade_math, grade_science, 
                  grade_social, grade_art, grade_explorations, grade_pe, grade_homework, 
                  grade_dance, grade_pre_algebra, grade_algebra, percent_ela, 
                  `percent_lit centers`, percent_math, percent_science, percent_social, 
                  percent_art, percent_explorations, percent_pe, percent_homework, 
                  percent_dance, percent_pre_algebra, percent_algebra))


# ----------------------------- ### Year Average Percentage for Powerschool and Illuminate ### --------------

# Run all quarters seperately then bind rows because setting rc_quarter_input = c("Q1", "Q2", "Q3", "Q4") was dropping subjects

quarter_1_final <- grade_df_df[[2]] %>% # grade_df_list %>%
  map_df(
    .f = get_q_grades_pct,
    grade_type = "percent",
    rc_quarter_input = c("Q1")
  )

quarter_2_final <- grade_df_df[[2]] %>% # grade_df_list %>%
  map_df(
    .f = get_q_grades_pct,
    grade_type = "percent",
    rc_quarter_input = c("Q2")
  )

# commented out because school closure, quarter 4 has both Q3 and Q3

# quarter_3_final <- grade_df_df[[2]] %>% #grade_df_list %>%
# map_df(.f = get_q_grades_pct,
#        grade_type = "percent",
#        rc_quarter_input = c("Q3"))

quarter_4_final <- grade_df_df[[2]] %>% # grade_df_list %>%
  map_df(
    .f = get_q_grades_pct,
    grade_type = "percent",
    rc_quarter_input = c("Q4")
  )

all_quarter_percents <- quarter_1_final %>%
  bind_rows(
    quarter_2_final, # quarter_3_final,
    quarter_4_final
  ) %>%
  mutate(percent = as.double(str_extract(percent, "[[:digit:]]+")))

final_percents <- all_quarter_percents %>%
  dplyr::group_by(site_id, student_id, subject) %>%
  summarize(percent = mean(percent)) %>%
  mutate(percent = case_when(
    percent < 70 ~ 70,
    TRUE ~ percent
  )) %>%
  mutate(percent = round(percent, 0)) %>%
  mutate(subject = case_when(
    subject == "social" ~ "social_studies",
    TRUE ~ subject
  ))

# ----------------------------- ### Year Average Grades for Powerschool and Illuminate ### --------------

# At first, calculate final grades from the final percentages availalble above.
# Upload information to Illuminate where it will stay for most students

# After that, use get_yavg_grades function to select letter grades for all students from
# report card export so grade modifications done in Illuminate will be reflected

if (calculated_type == "first_upload") {
  final_grades <- final_percents %>%
    mutate(percent = as.character(round(percent, 1))) %>%
    left_join(grade_percent_scale %>%
      mutate(percent = as.character(percent)),
    by = "percent"
    ) %>%
    select(-percent)
} else {
  final_grades <- grade_df_list %>% # grade_df_list_rm_prim %>%
    map_df(.f = get_yavg_grades) %>%
    select(site_id, student_id, subject, grade)
}


