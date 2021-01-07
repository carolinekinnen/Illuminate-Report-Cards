
write.csv(ps_upload_final, 
          here::here("output", 
                     "20-21 Files", 
                     "transcript", 
                     "powerschool_upload_final_grades_Q1.csv"),
          row.names = FALSE)
