# Short script to install all dependencies

packages <- c("readr", "dplyr", "sf", "leaflet", "rebird")
install.packages(packages)
devtools::install_github("sebpardo/myebird")
