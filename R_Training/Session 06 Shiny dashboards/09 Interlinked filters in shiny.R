# 09 Interlinked filters in shiny.R

# 23/09/2022

# AIM: How to introduce a condition in one filter to influence the set of values displayed by a second filter
# Explanation: There are two filters "Indicator Name" and "Geography"
# By Interlinked filters I mean that the values selected on the "Indicator Name" will influence the values
# displayed by the second filter "Geography", so both filters will combine to subset the underlying data.
# This feature can be extended to further number of filters besides the two shown in this example

# 1 Load required libraries
library(dplyr)
library(ggplot2)
library(shiny)

## source 
# [1-2]  User interface 
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Dynamic Filter Test App"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("indic_name"),
      uiOutput("geography")
    ),
    
    mainPanel(
      dataTableOutput("table")
    )
  )
))

# [2-2] Server  
server <- shinyServer(function(input, output) {
  
  METRICSraw <- METRICS %>% 
    
    select (indic_name_clean,geography_namec,value)
  
  output$indic_name <- renderUI({
    
    metriclist <- sort(unique(as.vector(METRICSraw$indic_name_clean)), decreasing = FALSE)
    metriclist <- append(metriclist, "All", after =  0)
    selectizeInput("metricchoose", "Metric:", metriclist)
    
  })
  
  output$geography <- renderUI({
    
    geographylist <- sort(unique(as.vector(METRICSraw$geography_namec)), decreasing = FALSE)
    geographylist <- append(geographylist, "All", 0)
    selectizeInput("geographychoose", "CCG:", geographylist)
    
  }) 
  
  
  data <- reactive({
    
    req(input$geographychoose)
    req(input$metricchoose)
    
    if (input$geographychoose == "All") {filt1 <- quote(geography_namec != "@?><")} else 
    {filt1 <- quote(geography_namec == input$geographychoose)}
    
    if (input$metricchoose == "All") {filt2 <- quote(indic_name_clean != "@?><")} else 
    {filt2 <- quote(indic_name_clean == input$metricchoose)}
    
    METRICSraw %>%
      filter_(filt1) %>%
      filter_(filt2)
  })
  
  output$table <- renderDataTable({
    data()
    
    
  })
  
})
# Launch it
shinyApp(ui = ui,server = server)