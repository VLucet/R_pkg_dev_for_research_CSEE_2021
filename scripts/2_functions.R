# My eBird map
# Turning script into functions

# 1. Clean data -----------------------------------------------------------

clean_ebird_data <- function(file){
  
  data_clean <- read.csv(file) %>%
    janitor::clean_names() %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::select(-observation_details, -checklist_comments)
  
  return(data_clean)
}

# 2. Turn into a spatial object -------------------------------------------

aggregate_ebird_data <- function(ebird_data){
  
  ebird_data_ag <- ebird_data %>%
    dplyr::group_by(submission_id, latitude, longitude, location,
                    date, duration_min, distance_traveled_km) %>%
    dplyr::summarise(num_species = n(), .groups = "drop")
  
  return(ebird_data_ag)
}

spatialize_ebird_data <- function(ebird_data){
  
  ebird_data_spatial <- ebird_data %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  return(ebird_data_spatial)
}

# 4. Produce the map ------------------------------------------------------

# Get the check icon
get_awesome_icons <- function(){
  
  icons <- leaflet::awesomeIcons(
    icon = 'fa-check',
    iconColor = 'black',
    markerColor = "green",
    library = 'fa')
  
  return(icons)
}

# Format the labels
format_marker_labels <- function(ebird_data){
  
  marker_labels <- sprintf(
    "<strong> %s <br/> %d species <br/> </strong> %s, %s minutes",
    ebird_data$location, ebird_data$num_species,
    ebird_data$date, ebird_data$duration_min) %>%
    lapply(htmltools::HTML)
  
  return(marker_labels)
}

# Plot the map
my_ebird_map <- function(file, aggregate_data = TRUE){
  
  clean_data <- clean_ebird_data(file)
  
  if(aggregate_data){
    
    spatial_data <- clean_data %>%
      aggregate_ebird_data() %>%
      spatialize_ebird_data()
    
    the_labels <- format_marker_labels(spatial_data)
    
  } else {
    
    spatial_data <- clean_data %>%
      spatialize_ebird_data()
    
    the_labels <- NULL
    
  }
  
  the_map <- leaflet::leaflet(data = spatial_data) %>%
    leaflet::addProviderTiles("OpenStreetMap.DE") %>%
    leaflet::addAwesomeMarkers(
      icon = get_awesome_icons(),
      clusterOptions = markerClusterOptions(),
      label = the_labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal",
                     padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
  
  return(the_map)
}
