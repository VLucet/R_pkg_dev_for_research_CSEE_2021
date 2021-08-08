# Package {carbon}
# Turning script into functions

# 1. Download Mauna Loa data ----------------------------------------------

get_weekly_data <- function(clean = TRUE){
  
  url <- "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_weekly_mlo.txt"
  
  co2_data <- read.table(url, skip = 49)
  names(co2_data) <- c("year", "month", "day", "year_decimal", "co2_ppm", "nb_days", 
                       "1_year_ago", "10_years_ago", "increase_since_1980")
  
  if (clean) {
    co2_data <- co2_data %>% 
      dplyr::mutate(date = lubridate::ymd(paste(year, month, day, sep = "-"))) %>% 
      dplyr::select(-year, -month, -day) %>% 
      dplyr::relocate(date)
  } 
  
  return(co2_data)
}

# 2. Find concentration closest to a date ---------------------------------

ppm_from_date <- function(date){
  
  date_formatted <- lubridate::ymd("1996-10-29")
  
  clean_data <- get_weekly_data()
  
  ppm <- clean_data %>% 
    dplyr::mutate(diff = abs(date - date_formatted)) %>% 
    dplyr::arrange(diff) %>% 
    dplyr::slice(1) %>% 
    dplyr::pull(co2_ppm)
  
  return(ppm)
}
