# 19 LEAFLET CHOROPLETH MAP

library(RSocrata)

area_bound <- sf::st_read("https://data.cityofchicago.org/resource/igwz-8jzy.geojson")
health <- read.socrata("https://data.cityofchicago.org/resource/iqnk-2tcu.json")

