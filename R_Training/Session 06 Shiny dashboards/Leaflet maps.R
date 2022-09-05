# LEAFLEFT MAPS IN R 
# Run this script to produce map

library(sf)
library(here)
library(dplyr)
library(ggplot2)
library(readxl)
library(janitor)
here()


here("Shapefiles")
list.files(here("Shapefiles"))

here()

CCG_boundaries <- st_read(here("Shapefiles","CCG_APR_2021_EN_BFC.shp"))

CCG_boundaries

GGC_map <- ggplot() +
            geom_sf(data = CCG_boundaries, size = 0.5, color = "black", fill ="coral") +
            ggtitle("CCG Boundaries plot. April 2021") +
            coord_sf()
GGC_map
ggsave(paste0("CCG Boundaries April 2021_coral",".jpeg"),width = 30, height = 20, dpi = 150, units = "cm")

library(readxl)
library(janitor)

Metrics <- read_excel(here("Data", "CCG_1_2_I00754_D.xls"), sheet = 3,skip = 17) %>%
            clean_names()
Metrics


Metrics_sub <- Metrics %>% 
               select(reporting_period,breakdown,indicator_value,ons_code)
Metrics_sub

Metrics_sub_ren <- Metrics %>% 
                    select(reporting_period,
                           breakdown,
                           indicator_value,
                           CCG21CD = ons_code)
Metrics_sub_ren
names(Metrics_sub_ren)

CCG_boundaries_MAP <- CCG_boundaries
Metrics_sub_ren_MAP <- Metrics_sub_ren


library(dplyr)

mapdata <- left_join(CCG_boundaries_MAP, Metrics_sub_ren_MAP, by = "CCG21CD")

MAPDATA <- st_transform(mapdata,"+proj=longlat", "+datum=WGS84")

library(leaflet)
pal <- colorNumeric(palette = "Blues",domain = MAPDATA$indicator_value)

rm(list=ls()[! ls() %in% c("MAPDATA")])

Leafmap_min <- MAPDATA %>% 
                   leaflet() %>%  
                   addTiles() %>% 
                   addPolygons(
                     stroke = TRUE,smoothFactor = 0.2, fillOpacity = 1,color = ~pal(indicator_value),
                   # Highlight plygons upon mouseover
                   highlight = highlightOptions(weight = 3,fillOpacity = 0,
                      color = "black",opacity = 1.0,
                        bringToFront = TRUE,sendToBack = TRUE)
                              ) %>% 
                        setView(lng = -2, lat = 54, zoom = 5) %>% 
                        # Include LEGEND
                        addLegend("topright", pal = pal, values = ~indicator_value,title = "under-75-mortality-from-cardiovascular-disease",opacity = 1)
Leafmap_min








