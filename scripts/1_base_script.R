# My eBird map
# Base script

# Load packages
library(dplyr)     # To manipulate data
library(janitor)   # To clean data
library(lubridate) # To clean date
library(sf)        # To handle vector data
library(leaflet)   # To make nice maps
library(htmltools) # To make nice map marker labels

# 0. Load eBird data ------------------------------------------------------

my_data <- read.csv("data/raw_ebird_data.csv")

# 1. Clean data -----------------------------------------------------------

my_data_clean <- my_data %>% 
  clean_names() %>% 
  mutate(date = ymd(date)) %>%
  select(-observation_details, -checklist_comments)

# 2. Turn into a spatial object -------------------------------------------

# Let's aggregate by checklist (submission_id)
my_data_clean_ag <- my_data_clean %>% 
  group_by(submission_id, latitude, longitude, county, location,
           date, duration_min, distance_traveled_km) %>% 
  summarise(num_species = n(), .groups = "drop")

my_data_clean_ag_spatial <- my_data_clean_ag %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# 4. Produce the map ------------------------------------------------------

# Get the check icon
icons <- awesomeIcons(
  icon = 'fa-check',
  iconColor = 'black',
  markerColor = "green",
  library = 'fa')

# Format the labels
marker_labels <- sprintf(
  "<strong> %s <br/> 
  %d species <br/> </strong> 
  %s, %s minutes <br/>
  %s km travelled",
  my_data_clean_ag_spatial$location,
  my_data_clean_ag_spatial$num_species, 
  my_data_clean_ag_spatial$date, 
  my_data_clean_ag_spatial$duration_min, 
  my_data_clean_ag_spatial$distance_traveled_km) %>% 
  lapply(HTML)

# Plot the map
leaflet(data = my_data_clean_ag_spatial) %>% 
  addProviderTiles("OpenStreetMap.DE") %>% 
  addAwesomeMarkers(
    icon = icons, 
    clusterOptions = markerClusterOptions(), 
    label = marker_labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"))
