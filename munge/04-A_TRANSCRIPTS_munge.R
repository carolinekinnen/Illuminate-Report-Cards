# Connect students table from Powerschool with manual schools table
student_schools <- students_powerschool_transcripts %>%
  select(
    ps_schoolid,
    student_id
  ) %>%
  left_join(schools,
            by = c("ps_schoolid" = "schoolid")
  )