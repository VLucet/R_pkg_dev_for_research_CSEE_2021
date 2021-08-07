# Turning script into functions

# 1. Plot the track with starting/ending points ---------------------------

plot_track <- function(track, pts = NULL){
  
  # Make base map
  track_map <- leaflet::leaflet(track) %>% 
    leaflet::addTiles() %>% 
    leaflet::addPolylines()
  
  # If points are given, plot them as well
  if(!is.null(pts)){
    track_map <- track_map  %>% 
      leaflet::addMarkers(data = pts)
  }
  
  return(track_map)
}

# 2. Subset points based on interval --------------------------------------

sample_track_regularly <- function(track, size){
  
  pts_sample <- sf::st_sample(track, size = size, type = "regular") 
  
  return(pts_sample)
}

# 3. Get observations at hotspots near the road ---------------------------

get_obs_near_itinerary <- function(track, pts = NULL, size = 50, dist = 5, 
                                   speciesCode = NULL,
                                   key = Sys.getenv("EBIRD_KEY")){
  
  # If points are absent, sample the track
  if(is.null(pts)){
    pts <- sample_track_regularly(track, size)
  }
  
  # Get the coordinates
  coords <- as.data.frame(sf::st_coordinates(pts))
  
  # For all points, get the nearest observations
  nearest_obs <- 
    mapply(rebird::nearestobs, 
           lng = coords$X, lat = coords$Y, 
           MoreArgs = list(dist = dist, speciesCode = speciesCode, 
                           key = key), SIMPLIFY = FALSE) %>% 
    dplyr::bind_rows() %>% 
    dplyr::distinct()
  
  return(nearest_obs)
}

# 4. Cross reference with my data and produce final plot ------------------

plan_roadtrip <- function(track, ebird_data, ...){
  
  nearest_obs <- get_obs_near_itinerary(track, ...)
  all_species <- unique(nearest_obs$sciName)
  
  all_my_species <- unique(ebird_data$sciName)
  ids_of_unmatched <- which(!(all_species %in% all_my_species))
  all_unmatches_species <- all_species[ids_of_unmatched]
  
  nearest_obs_unmatches_species <- nearest_obs %>% 
    dplyr::filter(.data$sciName %in% all_unmatches_species)
  
  nearest_obs_unmatches_species_summary <- nearest_obs_unmatches_species %>% 
    dplyr::group_by(.data$locId, .data$locName, .data$lat, .data$lng) %>% 
    dplyr::group_map(~format_species_names(.), .keep = TRUE) %>% 
    dplyr::bind_rows() %>% 
    dplyr::group_by(.data$locId, .data$locName, .data$lat, .data$lng, .data$all_species) %>% 
    dplyr::summarise(nb_species = length(unique(.data$speciesCode))) %>% 
    sf::st_as_sf(coords = c("lng", "lat"))
  
  # Make a palette
  the_pal <- colorNumeric(palette = "Purples", 
                          domain = nearest_obs_unmatches_species_summary$nb_species)
  
  # Make the labels
  labels <- paste0(sprintf(
    "<strong>%s<br/>%g unseen species <br/></strong>",
    nearest_obs_unmatches_species_summary$locName,
    nearest_obs_unmatches_species_summary$nb_species
  ), nearest_obs_unmatches_species_summary$all_species) %>% 
    lapply(htmltools::HTML)
  
  # Plot it
  plan <- 
    plot_track(track) %>% 
    addCircleMarkers(
      data = nearest_obs_unmatches_species_summary,
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
              values = ~nearest_obs_unmatches_species_summary$nb_species, 
              opacity = 0.7,
              position = "topright")
  
  
  return(plan)
}

# Helpers -----------------------------------------------------------------

format_species_names <- function(df) {
  the_species <- unique(df$comName)
  df$all_species <-  paste0(the_species, collapse = " <br/>")
  return(df)
}

# -------------------------------------------------------------------------
