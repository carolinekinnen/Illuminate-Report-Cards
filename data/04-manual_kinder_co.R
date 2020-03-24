
#------------------------ ### Co-teachers Table ###-------------------------------------

# table to be manually updated every year or at school request to change teacher names to include co-teachers

users_names <- users %>%
  mutate(teacher_full_name = str_c(first_name, last_name, sep = " ")) %>%
  mutate(teacher_full_name = case_when(
    teacher_full_name == "Alison Suranovic" ~ "Alison Suranovic/Richarda Bloom",
    teacher_full_name == "Claire Grossheim" ~ "Claire Grossheim/Victoria Jones", 
    teacher_full_name == "Danielle Stuckey" ~ "Danielle Stuckey/Darnell Dixon", 
    teacher_full_name == "Margaret Leiby" ~ "Margaret Leiby/Pariz Robinson",
    teacher_full_name == "LA Ellis" ~ "LA Ellis/Bryanna Ross",
    teacher_full_name == "Alejandra Vazquez" ~ "Alejandra Vazquez/April Hudson",
    teacher_full_name == "Kelley Logan" ~ "Kelley Logan/Jillian Lindenberg",
    teacher_full_name == "Kelli Gardner" ~ "Kelli Gardner/Jasmine Washington",
    teacher_full_name == "Fabiola Rosado" ~ "Fabiola Rosado/Theresa Pickett",
    teacher_full_name == "Noa Parker" ~ "Noa Parker/Francesca Varias",
    teacher_full_name == "Emily Lemick" ~ "Emily Lemick/Jackie Roman",
    TRUE ~ teacher_full_name
  ))
