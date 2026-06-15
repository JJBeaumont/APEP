library(readxl)
library(tidyverse)
library(MatchIt)
library(cobalt)

path = "data/"

continuous = read_excel(paste0(path, "c_adm_data.xlsx"))
intermittent = read_excel(paste0(path, "int_adm_data.xlsx"))

all = continuous |> mutate(cont = 1) |> 
  bind_rows(intermittent) |> mutate(cont = if_else(is.na(cont), 0, cont)) |> 
  drop_na(adm_type, icnarc_prob) |> mutate(inf_group = as.factor(str_detect(primary, "\\.27\\.")))

inf = all |> filter(inf_group == TRUE)

matched = matchit(cont ~ age + icnarc_prob, 
                  data = inf, 
                  method = "nearest", 
                  distance = "glm", 
                  ratio = 1, replace = FALSE)



# Create the Love Plot
love.plot(matched, 
          thresholds = c(m = 0.1), # Adds the 0.1 "success" line
          binary = "std",         # Standardizes binary variables
          abs = TRUE,             # Shows absolute differences
          var.order = "adjusted", # Orders by how biased they were originally
          colors = c("red", "blue"),
          stars = "std",
          main = "Covariate Balance: Intermittent vs Continuous Infusion")

matched_data <- match.data(matched)
top_30_pairs <- matched_data |>
  arrange(distance) |>
  head(41)


write_xlsx(inf, path = paste0(path, "infect.xlsx"))
