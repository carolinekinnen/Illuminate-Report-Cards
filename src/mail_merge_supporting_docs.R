

#----------------------- ### Q3/Q4 Mail Merge ### ---------------------------------

students_mail <- get_powerschool("students") %>%
  select(schoolid,
         student_number,
         first_name,
         last_name,
         grade_level,
         guardianemail) %>%
  collect()

read_sheet_username <- function(url) {
  read_sheet(url) %>%
    janitor::clean_names() %>%
    select(student_id,
           user_name = g_suite_user_name) %>%
    mutate(user_name = str_remove(user_name, "@kippchicago.org"))
}

kams_usernames <- read_sheet_username("https://docs.google.com/spreadsheets/d/1nhs3-xLCpZXeDlvivuw5BsflsCD9oR2ISH7YVj3pEK8/edit#gid=816950852")
kac_usernames <- read_sheet_username("https://docs.google.com/spreadsheets/d/1kddedbc_oItI8s7lmIcUgjqrZnPpYVb1RU6MIvpGEP4/edit#gid=618817768")
kacp_usernames <- read_sheet_username("https://docs.google.com/spreadsheets/d/183Akb2VV06dawd4QLmdKT8rofW7VyZxB7iEGQ6-4vlM/edit#gid=618817768")
kap_usernames <- read_sheet_username("https://docs.google.com/spreadsheets/d/14ymI1SrffooKhn4giu7csOAPkb7ndLZ_-dELmxTqq9U/edit#gid=1948448240")
kbcp_usernames <- read_sheet_username("https://docs.google.com/spreadsheets/d/1RiLo144-WniO1imvvRl5mQWUr2fa-giP_FgRYPd-7EU/edit#gid=604088811")
kbp_usernames <- read_sheet_username("https://docs.google.com/spreadsheets/d/1ImUTBtB-GsX0834sbr2KjLK-5F8XX4Ds3Qy88xC-3JA/edit#gid=1646041824")
koa_usernames <- read_sheet_username("https://docs.google.com/spreadsheets/d/1pzi438YupaE-sCBZfbvE0xJ9LRt-neRAO3QFv3unt3o/edit#gid=928285823")
kop_usernames <- read_sheet_username("https://docs.google.com/spreadsheets/d/1USYAf9nIHTZ08cF72lzBZIXWXpPly-fdmrWCXlujR9g/edit#gid=720655570")

usernames <- bind_rows(kams_usernames,
                       kap_usernames,
                       kac_usernames,
                       kacp_usernames,
                       kbcp_usernames,
                       kbp_usernames,
                       koa_usernames,
                       kop_usernames)

students_mail_usernames <- students_mail %>%
  left_join(usernames, by = c("student_number" = "student_id")) %>%
  filter(!is.na(user_name))


drive_mkdir(name = "Q3/Q4 Report Card Mail Merge", overwrite = TRUE)



gs4_create(name = "KOP Q3/Q4 Mail Merge Sheet", 
           sheets = students_mail_usernames %>% filter(schoolid == 4001802))



# Primary Custom Data

students_primary <- get_powerschool("students") %>%
  select(schoolid,
         student_number,
         first_name,
         last_name,
         grade_level,
         home_room,
         enroll_status) %>%
  collect()

students_primary <- students_primary %>%
  filter(enroll_status == 0,
         grade_level %in% c(0,1,2))





hrs <- students_primary %>% filter(schoolid == 4001632) %>% select(home_room) %>% unique()
hrs <- hrs[order(hrs$home_room),]


write_hr_function <- function(hr){
  
  hr_data <- students_primary %>% filter(schoolid == 4001632) %>% 
    select(student_number, first_name, last_name, home_room) %>% 
    filter(home_room == hr) %>%
    mutate("Remote Learning Overall Score" = " ")

  sheet_write(hr_data, ss = ss_kbp, sheet = hr)         
           
}


ss_kbp <- gs4_create(
  "19-20 Q3/4 KBP Remote Learning Values",
  sheets = list("Grading Guidelines" = data.frame(Key = c("Exceeds", "Meets", "Approaching", "Not Yet"),
                                                  Value = c(" ", " ", " ", " "))))


for(i in 1:nrow(hrs)) {
 write_hr_function(as.character(hrs[i,]))
}





  
