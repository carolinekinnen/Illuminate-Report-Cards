
#---------------------- #### Tables Created by Hand ### -----------------------

# School table
schools <- tibble(schoolid = c(78102, 7810, 400146, 4001462, 400163, 4001802, 400180, 4001632),
                      schoolname = c("Ascend Primary", "Ascend Middle", "Academy Chicago","Academy Chicago Primary", "Bloom", "One Primary", "One Academy", "Bloom Primary"),
                      schoolabbreviation =c("KAP", "KAMS", "KAC", "KACP", "KBCP", "KOP", "KOA", "KBP"))

# GPA Grade Scale 
grade_scale <- tribble(
  ~grade, ~points,
  "A+",   4.0,
  "A",    4.0,
  "A-",   4.0,
  "B+",   3.0,
  "B",    3.0,
  "B-",   3.0,
  "C+",   2.0,
  "C",    2.0,
  "C-",   2.0,
  "D+",   1.0,
  "D",    1.0,
  "D-",   1.0,
  "F",    0.0
)

# Outdated, should not be used anymore but kept for record keeping and to calculate past years
grade_scale_old <- tribble(
  ~grade, ~points,
  "A+",   4.0,
  "A",    4.0,
  "A-",   3.66,
  "B+",   3.33,
  "B",    3.0,
  "B-",   2.66,
  "C+",   2.33,
  "C",    2.0,
  "C-",   1.66,
  "F",    0.0
)

# Grade and percent 
grade_percent_scale <- tibble(
  grade = c(rep("A+", 21),
            rep("A", 40),
            rep("A-", 40),
            rep("B+", 30),
            rep("B", 40),
            rep("B-", 30),
            rep("C+", 30),
            rep("C", 40),
            rep("C-", 30),
            rep("D+", 30),
            rep("D", 40),
            rep("D-", 30),
            rep("F", 600)),
  percent = c(seq(98, 100, .1),
              seq(94, 97.9, .1),
              seq(90, 93.9, .1),
              seq(87, 89.9, .1),
              seq(83, 86.9, .1),
              seq(80, 82.9, .1),
              seq(77, 79.9, .1),
              seq(73, 76.9, .1),
              seq(70, 72.9, .1),
              seq(67, 69.9, .1),
              seq(63, 66.9, .1),
              seq(60, 62.9, .1),
              seq(0, 59.9, .1)) 
) %>%
  arrange(desc(percent))


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
