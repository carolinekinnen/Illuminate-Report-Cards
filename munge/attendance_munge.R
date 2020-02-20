
# Munge data that was pulled from BigQuery

# -------------------------- ### Creating Parameters ###-------------------------------------------

# find School Year and Term ID to filter large attendance tables

sy <- silounloadr::calc_academic_year(today(), format = 'firstyear')

ps_termid <- calc_ps_termid(current_first_year)

current_first_year <- calc_academic_year(lubridate::today(),
                                         format = "first_year")

terms <- get_powerschool("terms") %>%
  filter(id >= ps_termid) %>%
  select(id,
         abbreviation,
         firstday,
         lastday) %>%
  collect()  %>%
  unique()

identify_quarter <- function(date) {
  case_when(
    date >= terms$firstday[1] & date < terms$firstday[3] ~ "Q1",
    date >= terms$firstday[3] & date < terms$firstday[4] ~ "Q2",
    date >= terms$firstday[4] & date < terms$firstday[5] ~ "Q3",
    date >= terms$firstday[5] & date <= terms$lastday[5] ~ "Q4",
    TRUE ~ "not in SY")
}

# Last day of quarter in which report cards are being generated 
rc_quarter_table <- terms %>% 
  filter(abbreviation == identify_quarter(today() - 30)) %>%
  select(lastday)

rc_quarter_last_day <- rc_quarter_table$lastday

rc_quarter <- identify_quarter(today() - 30)

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

