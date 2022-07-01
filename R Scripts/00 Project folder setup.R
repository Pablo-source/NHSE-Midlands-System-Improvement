
# Function to setup standard project folder structure on a new project
# 15 June 2022
# PLR

library(here)

here::i_am("00 Project folder setup.R")

# 01 Create folder structure making use of HERE package 

project_folder_setup <-function(){
  
  if(!dir.exists("Data")){dir.create(here::here("Data"))}
  if(!dir.exists("Markdown")){dir.create(here::here("Markdown"))}
  if(!dir.exists("Functions")){dir.create(here::here("Functions"))}
  if(!dir.exists("Test")){dir.create(here::here("Test"))}
  if(!dir.exists("Report")){dir.create(here::here("Report"))}
  if(!dir.exists("Model")){dir.create(here::here("Model"))}
  
}

# Execute the function
project_folder_setup()
