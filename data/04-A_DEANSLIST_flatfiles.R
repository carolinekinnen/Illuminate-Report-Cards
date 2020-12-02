
# PARAMETERS --------------------------------------------------------------

sy <- calc_academic_year(today(), format = "firstyear")

current_first_year <- calc_academic_year(lubridate::today(),
                                         format = "first_year"
)

current_last_year <- calc_academic_year(today(),
                                        format = "second_year"
)

ps_termid <- calc_ps_termid(current_first_year)

terms <- get_powerschool("terms") %>%
  filter(id >= ps_termid) %>%
  select(
    id,
    abbreviation,
    firstday,
    lastday
  ) %>%
  collect() %>%
  unique() %>%
  arrange(id)

sy_abbreviation <- terms$abbreviation[1]


#----------------------- ### SIS Roster Links from Deans List ###---------------------------
# Has to be updated once each year OR if new gradebooks are added
# Only KAC, KACP, KAMS, KBCP, and KOA need to have updated final quarter grades on Deans List

get_objects_roster_links("KOA")


map(.x = c("KAC", "KAP", "KAMS", "KBCP", "KOA"),
    .f = get_objects_roster_links)

# Gradebook names that link subjects to grades, needs to be downloaded from Deans List
sis_roster_links <- dir(path = here::here("data/flatfiles/DL_roster_links"),
                        pattern = "20-21_roster_links", full.names = TRUE)

dl_rosters <- sis_roster_links %>%
  map_df(read_csv) %>%
  clean_names() %>%
  filter(!is.na(gradebook_name_at_load)) %>%
  select(sec_id = secondary_integration_id_at_load,
         gb_name = gradebook_name_at_load)
