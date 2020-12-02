
dl_upload_file_name_input <- function(schoolname) {
  
  file <- sprintf(paste0("output/",
                         sy_abbreviation,
                         " Files/deanslist/deanslist_upload_",
                         schoolname, 
                         "_",
                         rc_quarter,
                         "_%s.csv"),
                  today())
}

write.csv(dl_upload_kac,
          file = dl_upload_file_name_input("KAC"),
          row.names = FALSE)

write.csv(dl_upload_kap,
          file = dl_upload_file_name_input("KAP"),
          row.names = FALSE)

write.csv(dl_upload_kams,
          file = dl_upload_file_name_input("KAMS"),
          row.names = FALSE)

write.csv(dl_upload_kbcp,
          file = dl_upload_file_name_input("KBCP"),
          row.names = FALSE)

write.csv(dl_upload_koa,
          file = dl_upload_file_name_input("KOA"),
          row.names = FALSE)