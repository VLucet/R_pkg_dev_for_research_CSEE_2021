# My eBird map
# functionnal script

# Load packages
library(dplyr)     # To manipulate data
library(janitor)   # To clean data
library(lubridate) # To clean date
library(sf)        # To handle vector data
library(leaflet)   # To make nice maps
library(htmltools) # To make nice map marker labels

source("scripts/2_functions.R")

my_ebird_map("data/raw_ebird_data.csv")
