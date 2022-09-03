# R Script: 
# 04 Two KPIs and date template.R  

# AIM: create a set of KPIs in a shiny app with TWO  KPIs and a DATE box at the top.
# KPI 01 RTT All referral to treatment 
# KPI 02 RTT Admitted within 18 weeks after referral to treatment   
# KPI 03 DATE (Month-year format)

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
  
  # IMPORTANT: To ensure all three KPI (including dates) fit at the top
  # we must give with 4 to each of them, as maximum width is always 12 for the dashboard
  # Follow this rule (Two kpi width (6,6), three KPI width (4,4,4)), always max lenght is 12
  dashboardBody( 
    fluidRow(
      infoBoxOutput("Adm_allpath", width = 4),
      infoBoxOutput("Adm_18w",width=4),
      infoBoxOutput("Date", width = 4)
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

# INFOBOX03 DATE
output$Date <- renderValueBox({
  
  # We apply specific Year month format to original date variable from input data set
  data_kpi_Date  <- data_shiny %>% 
                      select(DATE,Adm_52w) %>% 
                      arrange(desc(DATE)) %>% 
                      mutate (MAX_FLAG =  ifelse(!is.na(Adm_52w),1,0)) %>% 
                      filter(MAX_FLAG ==1) %>% 
                      slice(1)  %>% 
                      select(DATE) %>% 
                      mutate(Month_Yr = format(as.Date(DATE),"%Y-%m"))
  data_kpi_Date 
  
  # We format this KPI to display a date max date in the datbase with latest data
        valueBox(
          data_kpi_Date$Month_Yr, "Date | Monthly figures", icon = icon("calendar"),
          color = "yellow")
})

}
# Launch it
shinyApp(ui = ui,server = server)

