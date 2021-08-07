# EBIRD road trip planner script (functionnal)

# Load packages
library(readr)   # To import data
library(dplyr)   # To manipulate data

library(rebird)  # To access eBird data
library(myebird) # To process personal eBird data

library(sf)      # To handle vector data
library(leaflet) # To make nice maps

# 0. Load data ------------------------------------------------------------

# Load road trip data
track <- st_read("data/Itinerary.gpx", layer = "tracks")

# Load my eBird data
my_data <- ebirdclean("data/MyEBirdData.csv")

# Plan the trip with a distance of 10 km ----------------------------------

source("scripts/2_functions.R")

plan_roadtrip(track = track, ebird_data = my_data, dist = 15)
