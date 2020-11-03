
library('ProjectTemplate')
load.project()

# ------------------------ ### Combining Tables ###-------------------------------------

# combine attendance and attendance code tables
attendance_complete <- attendance %>%
  right_join(attendance_code %>% 
               select(attendance_codeid = id,
                      att_code),
             by = "attendance_codeid")

# combine membership with attendance complete table
member_att <- membership  %>%
  left_join(attendance_complete %>%
              select(studentid,
                     att_date,
                     att_code
                     #presence_status_cd
              ),
            by =c("studentid",
                  "date" = "att_date"))

# Identify whether att_code is enrolled, present, absent, or tardy for each student by day

attend_student <- member_att %>%
  filter(date >= as_date(str_c(sy, "08", "01", sep = "-"))) %>%
  mutate(enrolled0 = 1,
         enrolled = if_else(att_code == "D" & !is.na(att_code), 0, enrolled0),
         present0 = ifelse(is.na(att_code) | att_code == "", 1, 0),
         present1 = ifelse(att_code %in%  c("A", "S"), 0, present0),
         present2 = ifelse(att_code == "H", 0.5, present1),
         present3 = ifelse(att_code %in% c("T", "E", "L", "I"), 1, present2),
         present = ifelse(is.na(present2), 1, present3),
         absent = (1 - present)*enrolled,
         tardy = ifelse(att_code %in% "T", 1, 0)) %>%
  left_join(students %>%
              select(studentid = id, 
                     student_number,
                     first_name,
                     last_name,
                     home_room),
            by="studentid") %>%
  inner_join(schools, by=c("schoolid")) %>%
  select(studentid,
         student_number,
         first_name,
         last_name,
         grade_level,
         schoolid,
         schoolname,
         schoolabbreviation,
         home_room,
         date,
         att_code,
         enrolled,
         present,
         absent,
         tardy)

#------------------------ ### Summarize Table ###-------------------------------------


attend_school_grade_student <- attend_student %>%
  filter(date <= rc_quarter_last_day) %>% 
  fuzzy_left_join(terms %>% filter(str_detect(abbreviation, "Q")) %>% select(-id) %>% 
                  dplyr::rename(quarter = abbreviation), 
                  by = c("date" = "firstday",
                         "date" = "lastday"),
                  match_fun = list(`>=`, `<=`)) %>%
  select(-c(firstday, lastday)) %>%
  group_by(schoolabbreviation, grade_level, student_number, first_name, last_name, quarter) %>%
  summarize(enrolled = sum(enrolled),
            present = sum(present),
            absent = sum(absent),
            tardy = sum(tardy)) %>%
  arrange(schoolabbreviation,
          grade_level)

enrolled_quarter <- str_c("enrolled_", rc_quarter)

# Pivot table so each attendance category and quarter is seperate row , calculate totals

attend_school_grade_student_totals <- attend_school_grade_student %>%
  pivot_wider(names_from = quarter, values_from = c(enrolled, present, absent, tardy)) %>%
  ungroup() %>%
  mutate(total_absent = rowSums(.[grep("absent", names(.))], na.rm = TRUE),
         total_tardy = rowSums(.[grep("tardy", names(.))], na.rm = TRUE),
         total_present = rowSums(.[grep("present", names(.))], na.rm = TRUE),
         total_enrolled = rowSums(.[grep("enrolled", names(.))], na.rm = TRUE), 
         rate = paste0(round(total_present/total_enrolled * 100, 1), "%")) # %>% 
  #filter(!is.na(enrolled_Q2))


