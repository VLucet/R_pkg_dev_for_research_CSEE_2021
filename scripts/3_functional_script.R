# Package {carbon}
# Functional script

# Load packages
library(lubridate)
library(dplyr)

# Source functions
source("scripts/2_functions.R")

# 1. Download Mauna Loa data ----------------------------------------------

weekly_data <- get_weekly_data()
  
print(head(weekly_data))

# 2. Find concentration closest to a date ---------------------------------

birthday <- ymd("1996-10-29")

ppm_from_date(birthday)
