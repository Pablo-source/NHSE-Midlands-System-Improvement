# R Script: 
# 03 Two KPIs Template.R 

# AIM: create a KPI in a shiny app with TWO  KPI at the top.
# KPI 01 RTT All referral to treatment 
# KPI 02 RTT Admitted within 18 weeks after referral to treatment   

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
      infoBoxOutput("Adm_allpath", width = 5),
      infoBoxOutput("Adm_18w",width=5)
      ) # closing parenthesis for fluidRow
    )  # Closing parenthesis for DashboardBody
  )  # Closing parenthesis for dashboardPage 
  
# [2-2] Server  (server)
server <- function(input,output) {
          
# INFOBOX 01
output$Adm_allpath <- renderValueBox({
          # MOST RECENT VALUE FOR Admissions all pathways METRIC 
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

# INFOBOX 02  
output$Adm_18w <- renderValueBox({
  # MOST RECENT VALUE FOR Admissions within 18 weeks METRIC 
  data_kpi_02 <- data_shiny %>% 
                  select(DATE,Adm_18w) %>% 
                  arrange(desc(DATE)) %>% 
                  mutate (MAX_FLAG =  ifelse(!is.na(Adm_18w),1,0)) %>% 
                  filter(MAX_FLAG ==1) %>% 
                  slice(1)  %>% 
                  select(DATE,Adm_18w)
  data_kpi_02
  # We want to create a KPI figure including thousands separator (using big.mark)
  valueBox(paste0(format(data_kpi_02$Adm_18w, big.mark = ','))
           , "Admitted within 18 weeks", icon = icon("desktop"),color = "blue") 
}) 

}

# Launch it
shinyApp(ui = ui,server = server)