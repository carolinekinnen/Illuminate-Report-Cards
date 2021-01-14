
#---------------------- #### Tables Created by Hand ### -----------------------

attendance_code_full <-
  tribble(
    ~behavior, ~att_code, ~attendance,
    "Tardy (Remote)", "Y", "Tardy (Remote)",
    "Absent (Remote)", "N", "Absent (Remote)",
    "Present (Remote)", "R", "Present (Remote)",
    '"A" Absent', "A", "Absent",
    "Absent", "A", "Absent",
    '"T" Tardy', "T", "Tardy",
    '"I" In School Suspsension', "I", "In School Suspension",
    "Suspension (Out of School)", "S", "Out of School Suspension",
    "D - Did Not Arrive", "D", "Did Not Arrive",
    '"E" Early Dismissal', "E", "Early Dismissal",
    '"D" Did Not Arrive', "D", "Did Not Arrive",
    '"H" Hospital', "H", "Hospital",
    '"S" Suspension Out of School', "S", "Out of School Suspension",
    "Academic Contact", "C", "Academic Contact",
    "Hospital", "H", "Hospital",
    '"S" Suspension (Out of School)', "S", "Out of School Suspension",
    "In School Suspension", "I", "In School Suspension",
    "Did Not Arrive", "D", "Did Not Arrive",
    '"D" Did No Arrive', "D", "Did Not Arrive",
    '"L" Tardy and Early Dismissal', "L", "Tardy and Early Dismissal",
    "Early Dismissal", "E", "Early Dismissal",
    "Early Dismissal", "D", "Did Not Arrive",
    "Early Dismissal", "H", "Hospital",
    '"I" In School Suspension', "I", "In School Suspension",
    '"X" Excused Absence', "X", "Excused Absence",
    "Absent (COVID related)", "V", "Absent (COVID related)",
    "Absent (COVID Related)", "V", "Absent (COVID related)",
    '"" Present', "P", "Present",
    "Tardy", "T", "Tardy",
    "Present", "P", "Present",
    "Excused Absence", "E", "Early Dismissal",
    "Excused Absence", "X", "Excused Absence"
  )

# CPS School ID & Home RCDTS
school_id_table <-
  tribble(
    ~schoolid, ~abbr, ~cps_school_id,
    "78102", "KAP", "400044",
    "7810", "KAMS", "400044",
    "400146", "KAC", "400146",
    "4001462", "KACP", "400146",
    "4001632", "KBP", "400163",
    "400163", "KBCP", "400163",
    "4001802", "KOP", "400180",
    "400180", "KOA", "400180",
  )

