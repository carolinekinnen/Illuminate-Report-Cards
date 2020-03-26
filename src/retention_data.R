
# library(ProjectTemplate)
# load.project()

# -------------------------------- ### Students Up for Retention ### --------------

# Need list of students who failed a quarter math or reading grade OR scored below 15 percentile on MAP

quarter_math_ela_grades <- quarter_grades_pivot_wide %>%
  select(student_id,
         grade_ela,
         percent_ela,
         grade_math,
         percent_math,
         grade_pre_algebra,
         percent_pre_algebra,
         grade_algebra,
         percent_algebra)

failing_quarter_math_ela <- quarter_math_ela_grades %>%
  filter(percent_math < 60 | percent_pre_algebra < 60 | percent_algebra < 60 | percent_ela < 60)

# Combining with MAP - different tables to accomodate all situations


# Failing Math or ELA, regardless of MAP score
failing_and_map_middle <- failing_quarter_math_ela %>%
  left_join(map_winter_pivot_students %>%
              filter(grade_level > 2), by = "student_id") %>%
  ungroup()

# Below 15 percentile on MAP, regardless of Math and ELA grades
below15_and_grades_middle <- map_winter_pivot_students %>%
  filter(test_percentile_Mathematics <= 15 | test_percentile_Reading <= 15,
         grade_level > 2) %>%
  left_join(quarter_math_ela_grades, by = "student_id")

# Both situations to cover all students who either failed Math or ELA OR scored below 15 percentile on MAP in Math or Reading
failing_or_below15_middle <- below15_and_grades_middle %>%
  rbind(failing_and_map_middle, by = "student_id") %>%
  filter(!is.na(term_name))


# ----------------------------- ##### Primary gradebook grades ##### ---------------------------
# done by gradebook instead of report card export because Illuminate doesn't store primary grades in same way

# Only need math and reading for retention tracker
q_grades_primary_math_reading <- q_grades_primary %>%
  group_by(gradebook_id,
           student_id) %>%
  filter(calculated_at == max(calculated_at)) %>%
  select(gradebook_id,
         mark,
         percentage,
         student_id) %>%
  left_join(ill_students_primary, by = "student_id") %>%
  ungroup() %>%
  mutate(ps_student_id = as.integer(local_student_id)) %>%
  left_join(students %>%
              mutate(ps_student_id = student_number),
            by = "ps_student_id") %>%
  filter(grade_level < 3,
         enroll_status == 0) %>%
  select(gradebook_id, mark, percentage, student_id, ps_student_id) %>%
  left_join(gradebooks, by = "gradebook_id") %>%
  mutate(gradebook_name = tolower(gradebook_name)) %>%
  mutate(mark = case_when(
    ps_student_id == 50326914 & gradebook_id == 2390 ~ "NA",      # student with a DL and gen ed grade
    ps_student_id == 50361046 & gradebook_id == 2556 ~ "NA",      # no reason he was ever in Penn state gradebooks 
    ps_student_id == 50361046 & gradebook_id == 2553 ~ "NA",
    ps_student_id == 50440882 & gradebook_id == 2391 ~ "NA",
    ps_student_id == 50457424 & gradebook_id == 2389 ~ "NA",    
    ps_student_id == 50457424 & gradebook_id == 2388 ~ "NA",    
    ps_student_id == 50484496 & gradebook_id == 2587 ~ "NA",   
    ps_student_id == 50484496 & gradebook_id == 2558 ~ "NA",    
    ps_student_id == 	50494343 & gradebook_id == 2391 ~ "NA",    # had a DL and gen ed grades
    ps_student_id == 50507003 & gradebook_id == 2391 ~ "NA",    
    ps_student_id == 50510943 & gradebook_id == 2390 ~ "NA",   
    ps_student_id == 50522545 & gradebook_id == 2445 ~ "NA",    
    ps_student_id == 50522545 & gradebook_id == 2446 ~ "NA",    
    ps_student_id == 50544537 & gradebook_id == 2594 ~ "NA",   
    ps_student_id == 50544537 & gradebook_id == 2595 ~ "NA",  
    ps_student_id == 50557273 & gradebook_id == 2553 ~ "NA",    
    ps_student_id == 50557273 & gradebook_id == 2556 ~ "NA",   
    ps_student_id == 50565390 & gradebook_id == 2457 ~ "NA",    
    ps_student_id == 50565390 & gradebook_id == 2513 ~ "NA",   
    ps_student_id == 50571821 & gradebook_id == 2409 ~ "NA",    
    ps_student_id == 50571821 & gradebook_id == 2408 ~ "NA",   
    ps_student_id == 50578622 & gradebook_id == 2581 ~ "NA",    
    ps_student_id == 50584771 & gradebook_id == 2229 ~ "NA",   
    ps_student_id == 50606235 & gradebook_id == 2391 ~ "NA",    
    ps_student_id == 50618585 & gradebook_id == 2580 ~ "NA",   
    ps_student_id == 50618585 & gradebook_id == 2581 ~ "NA",    
    ps_student_id == 50711673 & gradebook_id == 2313 ~ "NA",   
    ps_student_id == 60021664 & gradebook_id == 2313 ~ "NA",   
    ps_student_id == 60022198 & gradebook_id == 2193 ~ "NA",   
    ps_student_id == 60035287 & gradebook_id == 2408 ~ "NA",
    ps_student_id == 60035287 & gradebook_id == 2409 ~ "NA",
    ps_student_id == 60053534 & gradebook_id == 2311 ~ "NA",
    TRUE ~ mark
  )) %>%
  filter(active, !is_deleted) %>%
  filter(grepl("reading|math", gradebook_name),
         !grepl("guided|choice|template", gradebook_name)) %>%
  mutate(subject = str_extract(gradebook_name, "math|reading|math_dl")) %>% 
  filter(!is.na(mark)) %>% 
  filter(mark != "NA") %>%
  select(student_id = ps_student_id,
         subject,
         mark,
         percentage) %>%
  group_by(student_id, subject) %>%
  mutate(row = row_number()) %>% 
  pivot_wider(names_from = subject, values_from = c(mark, percentage)) %>%
  select(student_id,
         ela = percentage_reading,
         math = percentage_math)

# Combining with MAP

failing_and_map_primary <- q_grades_primary_math_reading %>%
  filter(math < 60 | ela < 60) %>%
  left_join(map_winter,
              by = "student_id")

below15_and_grades_primary <- map_winter_pivot %>%
  left_join(students %>% select(student_id = student_number,
                                grade_level), 
            by = "student_id") %>%
  filter(test_percentile_Mathematics <= 15 | test_percentile_Reading <= 15,
         grade_level < 3) %>%
  left_join(q_grades_primary_math_reading, by = "student_id") %>%
  select(-grade_level)

failing_or_below15_primary <- below15_and_grades_primary %>%
  bind_rows(failing_and_map_primary) %>%
  filter(!is.na(term_name)) %>%
  mutate_all(as.character)

failing_or_below15 <- failing_or_below15_middle %>%
  bind_rows(failing_or_below15_primary) %>%
  unique()
