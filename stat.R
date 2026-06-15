library(readxl)
library(tidyverse)
library(janitor)
library(writexl)

path = "data/"


###----Import prescriptions and clean----
stat_doses <- read_excel(paste0(path, "stat_doses.xlsx"),
                                    col_types = c("numeric", "skip", "skip", 
                                                  "skip", "skip", "date", "date", "date", 
                                                  "skip", "skip", "skip", "text", "text", 
                                                  "text", "skip", "skip", "skip", "skip", 
                                                  "skip", "skip", "skip", "skip", 
                                                  "skip", "skip", "skip"))

stat_doses <- stat_doses |> clean_names() |> 
  rename(hosp_no = patient_number,
         stat_dtm = requested_dtm)

cont_courses <- read_excel("Jasper/cont_courses.xlsx", 
                           col_types = c("numeric", "numeric", "date", 
                                         "date", "numeric", "date", "date", 
                                         "date", "text", "text", "text", "text", 
                                         "numeric"))

cont_courses <- cont_courses |> 
  mutate(ab = case_when(str_detect(antibiotic, "Mero") ~ "Meropenem", 
                        str_detect(antibiotic, "Pip") ~ "Pip-taz"))


stat_doses <- stat_doses |> 
  mutate(ab = case_when(str_detect(antibiotic, "Mero") ~ "Meropenem", 
                        str_detect(antibiotic, "Pip") ~ "Pip-taz"))

int_doses <- stat_doses |>  filter(str_detect(summary_line, "ONCE ONLY") == FALSE)
stat_doses <- stat_doses |>  filter(str_detect(summary_line, "ONCE ONLY") == TRUE)

stat_b4_cont <- stat_doses |> select(c(hosp_no, ward_start_dtm, stat_dtm, 
                                       antibiotic, ab))

stat_b4_cont <- cont_courses |> select(c(hosp_no, course_start, ward_start_dtm, antibiotic, 
                                         presc_dtm, ab)) |> 
  left_join(stat_b4_cont, join_by(hosp_no, ward_start_dtm, ab,
                                  closest(course_start >= stat_dtm)))

no_stat <- stat_b4_cont |> filter(is.na(stat_dtm))

no_stat <- no_stat |> left_join(int_doses, join_by(hosp_no, ab, 
                                                   ward_start_dtm,
                                                   closest(course_start >= stat_dtm)))

stat_b4_cont <- stat_b4_cont |> select(-c(antibiotic.x, ab, presc_dtm))

int_doses <- int_doses |> semi_join(no_stat, by = "hosp_no")


writexl::write_xlsx(stat_b4_cont, path = "stat_cont.xlsx")

writexl::write_xlsx(int_doses, path = "no_stat_int.xlsx")

