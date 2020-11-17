
#------------------------ ### Co-teachers Table ###-------------------------------------

# table to be manually updated every year or at school request to change teacher names to include co-teachers

users_names <- users %>%
  mutate(teacher_full_name = str_c(first_name, last_name, sep = " ")) %>%
  mutate(teacher_full_name = case_when(
    teacher_full_name == "Alison Suranovic" ~ "Alison Suranovic/Ciera Jamison",
    teacher_full_name == "Victoria Jones" ~ "Victoria Jones/Ricarda Bloom", 
    teacher_full_name == "Kynadi Gray" ~ "Kynadi Gray/Danielle Stuckey", 
    teacher_full_name == "Briana Hasan" ~ "Briana Hasan/Emily Schwartz",
    teacher_full_name == "Kelley Logan" ~ "Kelley Logan/Jasmine Washington",
    teacher_full_name == "April Hudson" ~ "April Hudson/Suzana Arambula",
    teacher_full_name == "Cassaundra Parker" ~ "Cassaundra Parker/Theresa Pickett",
    TRUE ~ teacher_full_name
  ))