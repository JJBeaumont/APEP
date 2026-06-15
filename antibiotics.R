library(readxl)
library(tidyverse)
library(janitor)
library(writexl)

path = "data/"


###----Import prescriptions and clean----
mero_taz_noncon <- read_excel(paste0(path, "2025_mero_taz_noncon.xlsx"), 
                            col_types = c("numeric", "skip", "skip", 
                                          "skip", "skip", "date", "date", "date", 
                                          "skip", "skip", "skip", "text", "text", 
                                          "text", "skip", "skip", "skip", "skip", 
                                          "skip", "skip", "skip", "skip", 
                                          "skip"))


mero_taz_noncon <- mero_taz_noncon |> clean_names() |> 
  drop_na(ward_end_dtm) |> 
  rename(hosp_no = patient_number,
         presc_dtm = requested_dtm)

  
mero_taz_noncon <- mero_taz_noncon |>
  filter(str_detect(summary_line, "ONCE ONLY") == FALSE) |> #remove stat doses
  group_by(hosp_no, ward_start_dtm) |> 
  mutate(pres_course = row_number()) |> 
  ungroup()


###----Import administrations----
mero_taz_admin <- read_excel(paste0(path,"2025_non_admin.xlsx"), 
                               col_types = c("skip", "skip", "numeric", 
                                             "skip", "skip", "text", "skip", "skip", 
                                             "skip", "date", "text", "skip", "skip", 
                                             "skip"))

mero_taz_admin <- mero_taz_admin |> clean_names() |> 
  rename(hosp_no = district_number, 
         event_dtm = significant_dtm, 
         antibiotic = order_name) |> drop_na(event_dtm)

mero_taz_admin <- mero_taz_admin |> 
  arrange(hosp_no, event_dtm) |> 
  filter(task_status_code == "Performed") |> 
  group_by(hosp_no, antibiotic) |>
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

mero_taz_admin_sum <- mero_taz_admin |>
  summarise(
    course_start = min(event_dtm),
    course_end = max(event_dtm),
    duration_days = round(as.numeric(difftime(course_end, course_start, units = "days")), 1),
    total_entries = n(), 
    .by = c(hosp_no, course_no, antibiotic)
  ) |> 
  filter(total_entries > 1) |> 
  select(c(hosp_no, course_no, course_start, course_end, duration_days))

##---Merge with prescriptions----
mero_taz_courses <- mero_taz_admin_sum |> 
  left_join(mero_taz_noncon, by = join_by(hosp_no, closest(course_start >= presc_dtm))) |> 
  drop_na(ward_start_dtm)

##---There are probably quite a lot of descriptive statistics which could come in here
##---We'll add more information about the admissions


##---Import ICNARC data
icnarc_adm <- read_csv(paste0("/Users/Scott/OneDrive - Greater Manchester/Coding/data/2025/admis.csv"),
               col_types = cols(`Adm date` = col_date(format = "%d/%m/%Y"),
                                `Adm time` = col_time(format = "%H:%M"),
                                `Dis date` = col_date(format = "%d/%m/%Y"),
                                `Hosp disc` = col_date(format = "%d/%m/%Y"),
                                `Death date` = col_date(format = "%d/%m/%Y"),
                                `FFW date` = col_date(format = "%d/%m/%Y"),
                                `FFW time` = col_time(format = "%H:%M")))

icnarc_adm <- icnarc_adm |> clean_names() |>
  rename(level3_days = l3_days,
         hosp_los = hospital_los,
         adm_source = admis_source,
         surg_code = diagnosis_surgery_reason_prior_to_admission_code_only,
         dis_loc = discharged_loc) |>
  mutate(
    adm_time = as.POSIXct(paste(adm_date, adm_time), format = "%Y-%m-%d %H:%M"),
    discharge_time = as.POSIXct(paste(dis_date, discharge_time), format = "%Y-%m-%d %H:%M"),
    ffw_time = as.POSIXct(paste(ffw_date, ffw_time), format = "%Y-%m-%d %H:%M"),
    ffw_los = round(difftime(ffw_time, adm_time, units = "days"), 2)
  )

#We need to fix the gaps caused by medicus export issue
icnarc_adm <- icnarc_adm |> mutate(
  across(
    c(physio_score, icnarc_prob),  # columns you might need to pull up
    ~ ifelse(is.na(.x) & !is.na(lead(.x)), lead(.x), .x)
  )
) |> mutate(
    #month_admit = factor(format(adm_date, "%b"), levels = month.abb),
    primary = if_else(is.na(primary), surg_code, primary), #surg_code if primary not set
  ) |>
  select(-c(starts_with("x"), surg_code, dis_loc, dis_trans, transfer_discharge_transferred_to, 
            cfs, hosp_disc, level3_days, secondary, ends_with("_date"), nhs_no)) |> drop_na(adm_time)


adm_data = mero_taz_courses |> 
  select(hosp_no, course_start) |> 
  left_join(icnarc_adm, by = join_by(hosp_no, closest(course_start >= adm_time))) |> 
  arrange(adm_time) |> 
  group_by(hosp_no, adm_time) |> 
  slice(1) |> ungroup()

#----Writing to files---
write_xlsx(adm_data, path = paste0(path, "int_adm_data.xlsx"))
write_xlsx(mero_taz_courses, path = paste0(path, "int_courses.xlsx"))

