# EBIRD road trip planner script

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

# Get my API key (sorted in my .Renviron file in my home folder)
my_key <- Sys.getenv("EBIRD_KEY")

# 1. Plot the track with starting/ending points ---------------------------

# Make a simple map
track_map <- leaflet(track) %>% 
  addTiles() %>% 
  addPolylines()
track_map

# 2. Subset points based on interval --------------------------------------

# Subset 50 points
pts_sub <- st_sample(track, 50, type = "regular") %>% 
  st_as_sf() %>% 
  st_cast("POINT")

# Plot the subset points
track_map_with_points <- leaflet(track) %>% 
  addTiles() %>% 
  addPolylines() %>% 
  addMarkers(data = pts_sub)
track_map_with_points

# 3. Get observations at hotspots near the road ---------------------------

# Get the coordinates of these points
coords <- as.data.frame(st_coordinates(pts_sub))
head(coords)

# Sample nearest observations
all_obs <- list()

for(coord_id in seq_len(nrow(coords))){
  the_row <- coords[coord_id, ]
  all_obs[[coord_id]] <- 
    nearestobs(lng = the_row$X, lat = the_row$Y, dist = 5,
               speciesCode = NULL, key = my_key)
}

# remove duplicates and find the list of species observed
all_obs_df <- bind_rows(all_obs) %>% 
  unique()
all_species <- unique(all_obs_df$sciName)

# 4. Cross reference with my data and produce final plot ------------------

# Cross reference with my species list
all_my_species <- unique(my_data$sciName)
ids_of_unmatched <- which(!(all_species %in% all_my_species))
all_unmatches_species <- all_species[ids_of_unmatched]

all_obs_df_unmatches_species <- all_obs_df %>% 
  filter(sciName %in% all_unmatches_species)

# Prepare the dataset for plotting the final map
format_species_names <- function(df) {
  # browser()
  the_species <- unique(df$comName)
  df$all_species <-  paste0(the_species, collapse = " <br/>")
  return(df)
}

all_obs_df_unmatches_species_summary <- all_obs_df_unmatches_species %>% 
  group_by(locId, locName, lat, lng) %>% 
  group_map(~format_species_names(.), .keep = TRUE) %>% 
  bind_rows() %>% 
  group_by(locId, locName, lat, lng, all_species) %>% 
  summarise(nb_species=length(unique(speciesCode))) %>% 
  st_as_sf(coords = c("lng", "lat"))

# Make a palette
the_pal <- colorNumeric(palette = "Purples", 
                        domain = all_obs_df_unmatches_species_summary$nb_species)

# Make the labels
labels <- paste0(sprintf(
  "<strong>%s<br/>%g unseen species <br/></strong>",
  all_obs_df_unmatches_species_summary$locName,
  all_obs_df_unmatches_species_summary$nb_species
), all_obs_df_unmatches_species_summary$all_species) %>% 
  lapply(htmltools::HTML)

# Plot it
track_map %>% 
  addCircleMarkers(
    data = all_obs_df_unmatches_species_summary,
    radius = 5,
    color = ~the_pal(nb_species),
    stroke = FALSE, 
    fillOpacity = 0.9,
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% 
  addLegend(pal = the_pal, 
            title = "Unseen<br/> species",
            values = ~all_obs_df_unmatches_species_summary$nb_species, 
            opacity = 0.7,
            position = "topright")

# -------------------------------------------------------------------------
