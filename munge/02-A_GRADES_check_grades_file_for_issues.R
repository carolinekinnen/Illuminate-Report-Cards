# Let's find issues with grades file

kc_grades_distinct <- 
  kc_grades_full %>%
  
  select(-c(Teacher, `Section ID`)) %>% 
  distinct() %>%
  filter(!is.na(Percentage))


mask <- kc_grades_distinct %>%
  select(`Student ID`, 
         `Course Name`) %>%
  duplicated()

duplicate_grades <- kc_grades_distinct[mask, ]

include_attendance <- kc_grades_distinct %>%
  mutate(`Course Name` = str_to_upper(`Course Name`)) %>%
  filter(grepl("ATT", `Course Name`))

View(duplicate_grades)
View(include_attendance)
