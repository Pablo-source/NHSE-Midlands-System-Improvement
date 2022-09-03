# R Script: 
# 02 Single KPI Template.R

# AIM: create a KPI in a shiny app with one single KPI at the top.
# KPI: All pathways admitted patients

library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(leaflet)
library(plotly)


# [1-2]  User interface (UI)
ui <- dashboardPage(
  
  dashboardHeader(title = "Referral to treatment"),
  # This Sidebar menu allows us to include new items on the sidebar
  dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      # Main Overview tab displayed in the navigation menu
      menuItem("Overview", tabName = "Overview_content", icon = icon("map"))
    )
  ),
  
  dashboardBody( 
    fluidRow(
      infoBoxOutput("Adm_allpath", width = 5)
    )
    # Now in the same BODY we want to include a data table 
  )  # Closing parenthesis for tabItems
  # Closing parenthesis for DashboardBody
)    # Closing parenthesis for  dashboardPage FOR THE ENTIRE UI section

# [2-2] Server  (server)
server <- function(input,output) {
          # INFOBOX 01
          output$Adm_allpath <- renderValueBox({
          # MOST RECENT VALUE FOR METRIC 
          data_kpi_01 <- data_shiny %>% 
                  select(DATE,Adm_allpath) %>% 
                  arrange(desc(DATE)) %>% 
                  mutate (MAX_FLAG =  ifelse(!is.na(Adm_allpath),1,0)) %>% 
                  filter(MAX_FLAG ==1) %>% 
                  slice(1)  %>% 
                  select(Adm_allpath)
          data_kpi_01
        # We want to create a KPI figure including thousands separator (using big.mark)
     valueBox(paste0(format(data_kpi_01$Adm_allpath, big.mark = ','))
    , "All pathways admitted", icon = icon("bar-chart-o"),color = "green") 
  })                     
  
}

# Launch it
shinyApp(ui = ui,server = server)