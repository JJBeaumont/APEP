library(tidyverse)
library(readxl)
library(janitor)


steroid <- read_excel("data/steroids.xlsx", col_types = c("skip", 
                                                     "skip", "numeric", "skip", "skip", "text", 
                                                     "skip", "skip", "skip", "date", "text", 
                                                     "text", "text", "skip"))

steroid <- steroid |> clean_names() |> 
  rename(hosp_no = district_number, 
         event_dtm = significant_dtm)

steroid <- steroid |> arrange(hosp_no, event_dtm) |> 
  filter(task_status_code == "Performed") |> 
  mutate(drug = str_extract(order_name, "(^[A-Za-z]+)?")) |> 
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

steroid_sum <- steroid |>   
  summarise(
    steroid_start = min(event_dtm),
    steroid_end = max(event_dtm),
    duration_hrs = round(as.numeric(difftime(steroid_end, steroid_start, units = "hours")), 1),
    .by = c(hosp_no, course_no, drug)
  ) 



infect <- read_excel("Jasper/infect.xlsx", 
                     col_types = c("numeric", "date", "date", 
                                   "text", "numeric", "text", "text", 
                                   "date", "text", "text", "text", "date", 
                                   "text", "numeric", "numeric", "numeric", 
                                   "date", "numeric", "text"))

infect <- infect |> select(hosp_no, adm_time, course_start, cont)

infect <- infect |> left_join(steroid_sum, by = join_by(hosp_no, 
                                                      adm_time < steroid_start, 
                                                      course_start < steroid_end
                                                      ))
infect <- infect |> mutate(steroid_gap =
                             difftime(steroid_start, course_start, units = "hours"), 
                           steroid_true = if_else(steroid_gap < 24, TRUE, FALSE)
                        ) |> 
  filter(steroid_true == TRUE) |> select(-c(course_no, steroid_gap))

infect2 <- read_excel("Jasper/infect.xlsx", 
                     col_types = c("numeric", "date", "date", 
                                   "text", "numeric", "text", "text", 
                                   "date", "text", "text", "text", "date", 
                                   "text", "numeric", "numeric", "numeric", 
                                   "date", "numeric", "text"))
infect2 <- infect2 |> select(hosp_no, adm_time, course_start, cont)

infect <- infect |>  select(c(hosp_no, steroid_start, steroid_end, steroid_true, drug, adm_time, 
                           course_start)) |> 
    right_join(infect2, join_by(hosp_no, adm_time, course_start))

writexl::write_xlsx(infect, path = "steroid_infect.xlsx")
