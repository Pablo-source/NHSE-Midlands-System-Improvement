# R Script:  
# SHINY Leaflet map template.R 

# Load required libraries
library(sf)
library(here)
library(dplyr)
library(ggplot2)
library(readxl)
library(janitor)
library(shiny)
# We use LEAFLET library to create interactive map
library(leaflet)

# [1-2]  User interface (UI)
ui <- dashboardPage(
    dashboardHeader(title = "Leaflet map template"),
                    # This Sidebar menu allows us to include new items on the sidebar
                    dashboardSidebar(
                      sidebarMenu(
                        # Setting id makes input$tabs give the tabName of currently-selected tab
                        id = "tabs",
                        # Main Overview tab displayed in the navigation menu 
                        menuItem("Map", tabName = "map", icon = icon("map")
        )
      )
    )
  ,    
  # Follow this rule (Two kpi width (6,6), three KPI width (4,4,4)), always max lenght is 12
  dashboardBody(tabItems(tabItem(
            tabName ="map",
              h2("World map COVID19 deaths by contry"),
            fluidRow(  box(leafletOutput("map"),p("Interactive LEAFLET map"),
              width = 15 )
       )
     )
   )
 ) # UI Closing Dashboard Body parenthesis 
) # UI  Closing Dashboard Page parenthesis 
            

# [2-2] Server  (server) 
server <- function(input,output) {
  
  # LEAFLET MAP
  output$map = renderLeaflet ({

    CCG_boundaries <- st_read(here("Shapefiles","CCG_APR_2021_EN_BFC.shp"))
    # We use clean_names() function from JANITOR package to work with clean variable names
    Metrics <- read_excel(here("Data", "CCG_1_2_I00754_D.xls"), sheet = 3,skip = 17) %>%
               clean_names()
    Metrics_sub <- Metrics %>% 
                    select(reporting_period,breakdown,indicator_value,ons_code) %>% 
                    filter(reporting_period =="2019")

    Metrics_sub_ren <- Metrics_sub %>% 
                          select(reporting_period,
                                 breakdown,
                                 indicator_value,
                                 CCG21CD = ons_code)
    # Rename map shapefile and data files prior to creating the LEAFLET map
    CCG_boundaries_MAP <- CCG_boundaries
    Metrics_sub_ren_MAP <- Metrics_sub_ren
    
   # Merge both files using DPLYR left_join() function
    mapdata <- left_join(CCG_boundaries_MAP, Metrics_sub_ren_MAP, by = "CCG21CD")
   # Apply required projection to the map created by merging shapefile and data files
    MAPDATA <- st_transform(mapdata,"+proj=longlat", "+datum=WGS84")
   # Create BLUE color palette based on metric values (indicator_value)
    pal <- colorNumeric(palette = "Blues",domain = MAPDATA$indicator_value)
  # AND FINALLY WE DRAW THE MAP USING LEAFLET
    Leafmap_map <- MAPDATA %>% 
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
    Leafmap_map
  })        
 
}
# Launch SHINY APP with LEAFLET MAP
shinyApp(ui = ui,server = server)

