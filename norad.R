library(tidyverse)
library(readxl)
library(janitor)


norad <- read_excel("data/norad.xlsx", col_types = c("skip", 
                                                     "skip", "numeric", "skip", "skip", "text", 
                                                     "skip", "skip", "skip", "date", "text", 
                                                     "text", "text", "skip"))

norad <- norad |> clean_names() |> 
  rename(hosp_no = district_number, 
         event_dtm = significant_dtm)

norad <- norad |> arrange(hosp_no, event_dtm) |> 
  filter(task_status_code == "Performed") |> 
  mutate(drug = "Norad") |> 
  mutate(strength = case_when(str_detect(order_name, "4mg/50ml") ~ "single", 
                              str_detect(order_name, "8mg/50ml") ~ "double", 
                              .default = "other")) |> 
  group_by(hosp_no, drug) |>
  mutate(
    gap_hours = round(difftime(event_dtm, lag(event_dtm), units = "hours"),2), 
    new_course = is.na(gap_hours) | gap_hours > 24
  ) |> 
  ungroup() |> 
  group_by(hosp_no) |> 
  mutate(
    course_no = cumsum(new_course)
  ) |> 
  ungroup()

norad_sum <- norad |>   
  summarise(
  course_start_na = min(event_dtm),
  course_end_na = max(event_dtm),
  duration = round(as.numeric(difftime(course_end_na, course_start_na, units = "hours")), 1),
  total_entries = n(), 
  .by = c(hosp_no, course_no, drug)
) 

norad_sum <- norad_sum |> 
  mutate(final_hour = hour(course_end_na),
  time_proof_credit = case_when(
    final_hour >= 8  & final_hour < 16 ~ 4,  # Midpoint of 8hr window (0800-1600)
    final_hour >= 16 & final_hour < 22 ~ 3,  # Midpoint of 6hr window (1600-2200)
    TRUE                               ~ 5   # Midpoint of 10hr window (2200-0800)
  ),
  duration_hours = duration + time_proof_credit)

norad_sum <- norad_sum |> select(c(hosp_no, drug, course_start_na, course_end_na, 
                                   duration_hours))


infect <- read_excel("Jasper/infect.xlsx", 
                     col_types = c("numeric", "date", "date", 
                                   "text", "numeric", "text", "text", 
                                   "date", "text", "text", "text", "date", 
                                   "text", "numeric", "numeric", "numeric", 
                                   "date", "numeric", "text"))

infect <- infect |> select(hosp_no, adm_time, course_start, cont)

infect <- infect |> left_join(norad_sum, by = join_by(hosp_no, 
                                                      adm_time < course_start_na))

writexl::write_xlsx(infect, path = "norad_infect.xlsx")
