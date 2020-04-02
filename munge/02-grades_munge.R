

# Grades Munge
# cleaning and transforming files from Illuminate for Illuminate, Deans List, and Powerschool

#-------------------------- ### Illuminate Report Card Data for Powerschool and Deans List ###----------------------------------

# Fixing Homework and Behavior Grade/Percent Problem
# Fields were named incorrectly - have to contain either "Grade" or "Percent", but these contain both
# removing so grade and percent function will work

# Figure out which lists in the dataframe contain KAP 3, KAP 4, KOP 3, and KOP 4

kap_kop <- grade_df_df %>%
  filter(str_detect(file_name, "KAP_|KOP_")) %>% 
  mutate(grade = str_extract(file_name, "\\d")) %>%
  mutate(school = tolower(str_extract(file_name, "KAP|KOP")))

# Anti-join to remove original files from grade_df_df
not_kap_kop <- grade_df_df %>%
  filter(!str_detect(file_name, "KAP_|KOP_")) 

# Apply column name fixing function to KAP and KOP
# Comment out KOP_4 in 20-21 when grade level exists

kap_kop_fixed <- tribble(
  ~file_name, ~df,
  "KAP_3.csv", behavior_homework_name_fixer_by_row(kap_kop, 1),
  "KAP_4.csv", behavior_homework_name_fixer_by_row(kap_kop, 2),
  "KOP_3.csv", behavior_homework_name_fixer_by_row(kap_kop, 3),
  #"KOP_4.csv", behavior_homework_name_fixer_by_row(kap_kop, 4) 
)

# Bind rows to join back to original dataframe of dataframes
grade_df_df <- not_kap_kop %>%
  bind_rows(kap_kop_fixed)

# Doesn't work for now, keeping trying

# kap_kop_fixed <- pmap(kap_kop, function(...){
#   current_df <- tibble(...)
#   behavior_homework_name_fixer_by_row(current_df)
# }
# )
# 
# kap_kop_fixed %>%
#   select(-file_name) %>%
#   pmap(behavior_homework_name_fixer)

#-------------------------- ### Fixing Algebra/Pre-Algebra Problem ###----------------------------------

# changing Math to Pre-Algebra and Algebra for KOA and KBCP 7th and KOA 8th
# was Math for Q1 because Alg and Pre-Alg field groups were not set up
# only needs to be run in 2019-2020 school year

# KOA 7 - Pre-Algebra

koa_7 <- grade_df_list[[18]]

koa_7_prealg <- koa_7 %>%
  filter(site_id == 400180,
         !is.na(sy19_20_rc_koa_7th_q2_pre_algebra_pre_algebra_grade)) %>%
  mutate(sy19_20_rc_koa_7th_q1_pre_algebra_pre_algebra_grade = sy19_20_rc_koa_7th_q1_math_math_grade,
         sy19_20_rc_koa_7th_q1_pre_algebra_pre_algebra_percent = sy19_20_rc_koa_7th_q1_math_math_percent) %>%
  mutate(sy19_20_rc_koa_7th_q1_math_math_grade = NA,
         sy19_20_rc_koa_7th_q1_math_math_percent = NA)

koa_7 <- koa_7 %>%
  anti_join(koa_7_prealg, by = "student_id")  # remove them all from grade_df_list then do bind so won't be duplicates

grade_df_list[[18]] <- bind_rows(koa_7_prealg, koa_7) 

# KOA 8 - Algebra

koa_8 <- grade_df_list[[19]]

koa_8_alg <- koa_8 %>%
  filter(site_id == 400180,
         !is.na(sy19_20_rc_koa_8th_q2_algebra_algebra_grade)) %>%
  mutate(sy19_20_rc_koa_8th_q1_algebra_algebra_grade = sy19_20_rc_koa_8th_q1_math_math_grade,
         sy19_20_rc_koa_8th_q1_algebra_algebra_percent = sy19_20_rc_koa_8th_q1_math_math_percent) %>%
  mutate(sy19_20_rc_koa_8th_q1_math_math_grade = NA,
         sy19_20_rc_koa_8th_q1_math_math_percent = NA)

koa_8 <- koa_8 %>%
  anti_join(koa_8_alg, by = "student_id")

grade_df_list[[19]] <- bind_rows(koa_8_alg, koa_8)

# KBCP 7 - Pre-Algebra and typo in course name

kbcp_7 <- grade_df_list[[14]]

kbcp_7_prealg <- kbcp_7 %>%
  filter(site_id == 400163, 
         !is.na(sy19_20_rc_kbcp_7th_q2_pre_algebra_pre_algebra_grade)) %>%
  mutate(sy19_20_rc_kbcp_7th_q1_pre_algebra_pre_algebra_grade = sy19_20_rc_kbcp_7th_q1_math_math_grade,
         sy19_20_rc_kbcp_7th_q1_pre_algebra_pre_algebra_percent = sy19_20_rc_kbcp_7th_q1_math_math_percent) %>%
  mutate(sy19_20_rc_kbcp_7th_q1_math_math_grade = NA,
         sy19_20_rc_kbcp_7th_q1_math_math_percent = NA) %>%

# Field group course name spelled incorrectly in Illuminate, has to be corrected to match up with subject 
  dplyr::rename(sy19_20_rc_kbcp_7th_q1_course_math_course_name_pre_algebra = sy19_20_rc_kbcp_7th_q1_course_math_course_name_pre_agebra,
         sy19_20_rc_kbcp_7th_q2_course_math_course_name_pre_algebra = sy19_20_rc_kbcp_7th_q2_course_math_course_name_pre_agebra,
         sy19_20_rc_kbcp_7th_q3_course_math_course_name_pre_algebra = sy19_20_rc_kbcp_7th_q3_course_math_course_name_pre_agebra,
         sy19_20_rc_kbcp_7th_q4_course_math_course_name_pre_algebra = sy19_20_rc_kbcp_7th_q1_course_math_course_name_pre_agebra)

kbcp_7 <- kbcp_7 %>%
  anti_join(kbcp_7_prealg, by = "student_id")

grade_df_list[[14]] <- bind_rows(kbcp_7_prealg, kbcp_7)

# KBCP 8 - Algebra

kbcp_8 <- grade_df_list[[15]] 

kbcp_8_alg <- kbcp_8 %>%
  filter(site_id == 400163,
         !is.na(sy19_20_rc_kbcp_8th_q2_algebra_algebra_grade)) %>%
  mutate(sy19_20_rc_kbcp_8th_q1_algebra_algebra_grade = sy19_20_rc_kbcp_8th_q1_math_math_grade,
         sy19_20_rc_kbcp_8th_q1_algebra_algebra_percent = sy19_20_rc_kbcp_8th_q1_math_math_percent) %>%
  mutate(sy19_20_rc_kbcp_8th_q1_math_math_grade = NA,
         sy19_20_rc_kbcp_8th_q1_math_math_percent = NA)

kbcp_8 <- kbcp_8 %>%
  anti_join(kbcp_8_alg, by = "student_id")

grade_df_list[[15]] <- bind_rows(kbcp_8_alg, kbcp_8)


#-------------------------- ### Course Names for Grades 4-8 ###----------------------------------

course_names <- course_names_teachers %>%
  select(site_id = schoolid,
         student_id = student_number, 
         course_long_name,
         course_number) %>%
  left_join(student_schools, by = "student_id") %>%
  mutate(store_code = rc_quarter) %>%
  dplyr::rename(course_school = schoolabbreviation) %>%
  mutate(subject = tolower(str_sub(course_long_name, start = 5))) %>%
  #filter(!grepl("behavior|choice reading|explorations|homework|musical theater|visual arts", subject)) %>%
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

rc_letter_grades <- grade_df_df[[2]] %>% #grade_df_list %>% 
  map_df(.f = get_q_grades_pct,
         grade_type = "grade",
         rc_quarter_input = rc_quarter
           ) %>% 
  left_join(student_schools,
            by = "student_id") %>% 
  filter(course_school == schoolabbreviation)  %>%
  select(-c(ps_schoolid, schoolname, schoolabbreviation))

rc_percent <- grade_df_df[[2]] %>% #grade_df_list %>% 
  map_df(.f = get_q_grades_pct,
         grade_type = "percent",
         rc_quarter_input = rc_quarter
         ) %>% 
  left_join(student_schools,
            by = "student_id") %>% 
  filter(course_school == schoolabbreviation)  %>%
  select(-c(ps_schoolid, schoolname, schoolabbreviation))

quarter_grades <-rc_letter_grades %>%
  left_join(course_names, 
            by = c("student_id",
                   "store_code",
                   "course_school",
                   "site_id",
                   "subject")) %>%
  left_join(rc_percent,
            by = c("student_id",
                   "store_code",
                   "course_school",
                   "site_id", 
                   "subject")) %>% 
  mutate(percent = as.double(gsub("%", "", percent))) %>%
  left_join(students %>%
              select(student_id = student_number, grade_level, first_name, last_name), by = "student_id")

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
  pivot_wider(names_from = c(subject), 
              values_from = c(grade, percent)) #%>%
  #select(-row)

# ----------------------------- ### Year Average Percentage for Powerschool and Illuminate ### --------------

# Run all quarters seperately then bind rows because setting rc_quarter_input = c("Q1", "Q2", "Q3", "Q4") was dropping subjects

quarter_1_final <- grade_df_df[[2]] %>% #grade_df_list %>% 
  map_df(.f = get_q_grades_pct,
         grade_type = "percent",
         rc_quarter_input = c("Q1"))

quarter_2_final <- grade_df_df[[2]] %>% #grade_df_list %>% 
  map_df(.f = get_q_grades_pct,
         grade_type = "percent",
         rc_quarter_input = c("Q2"))

quarter_3_final <- grade_df_df[[2]] %>% #grade_df_list %>% 
  map_df(.f = get_q_grades_pct,
         grade_type = "percent",
         rc_quarter_input = c("Q3"))

quarter_4_final <- grade_df_df[[2]] %>% #grade_df_list %>% 
  map_df(.f = get_q_grades_pct,
         grade_type = "percent",
         rc_quarter_input = c("Q4"))

all_quarter_percents <- quarter_1_final %>%
  bind_rows(quarter_2_final, quarter_3_final, quarter_4_final) %>%
  mutate(percent = as.double(str_extract(percent, "[[:digit:]]+"))) 

final_percents <- all_quarter_percents %>%
  dplyr::group_by(site_id, student_id, subject) %>%
  summarize(percent = mean(percent))

# ----------------------------- ### Year Average Grades for Powerschool and Illuminate ### --------------

# At first, calculate final grades from the final percentages availalble above. 
# Upload information to Illuminate where it will stay for most students

# After that, use get_yavg_grades function to select letter grades for all students from 
# report card export so grade modifications done in Illuminate will be reflected

if(calculated_type == "first_upload") {
  final_grades <- final_percents %>%
    mutate(percent = as.character(round(percent, 1))) %>%
    left_join(grade_percent_scale %>% 
                mutate(percent = as.character(percent)),
              by = "percent") %>%
    select(-percent)
} else {
  final_grades <- grade_df_list_rm_prim %>% 
    map_df(.f = get_yavg_grades) %>%
    select(site_id, student_id, subject, grade)
}



