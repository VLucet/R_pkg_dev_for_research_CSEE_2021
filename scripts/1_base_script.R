# Package {carbon}
# Base script

# Load packages
library(lubridate)
library(dplyr)

# 1. Download Mauna Loa data ----------------------------------------------

weekly_data_url <- 
  "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_weekly_mlo.txt"
co2_data <- read.table(weekly_data_url, skip = 49)

names(co2_data) <- c("year", "month", "day", "year_decimal", 
                     "co2_ppm", "nb_days", "1_year_ago", 
                     "10_years_ago", "increase_since_1980")

co2_data_clean <- co2_data |> 
  mutate(date = ymd(paste(year, month, day, sep = "-"))) |> 
  select(-year, -month, -day) |> 
  relocate(date)

# 2. Find concentration closest to a date ---------------------------------

birthday <- ymd("1996-10-29")

ppm_from_date <- co2_data_clean |> 
  mutate(diff = abs(date - birthday)) |> 
  arrange(diff) |> 
  slice(1) |> 
  pull(co2_ppm)

print(ppm_from_date)
