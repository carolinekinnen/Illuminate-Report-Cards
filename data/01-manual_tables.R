
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


