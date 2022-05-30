
# 00 Setup project structure
# 11/04/2022
# AIM: At the start of any new report you design first you create a brand new Project Folder to host all the scripts alongside input and output files
#      Then the next action you take is to create a Folder structure within that Project folder to organise your workflow and R scripts


# FUNCTION TO SETUP PROJECT STRUCTURE WITH FOLDERS AND SUB FOLDERS

project_setup <-function(){
  
  if(!dir.exists("data")){dir.create("data")}
  if(!dir.exists("Output")){dir.create("Output")}
  if(!dir.exists("Shapefiles")){dir.create("Shapefiles")}
  if(!dir.exists("Checks")){dir.create("Checks")}
  if(!dir.exists("Maps")){dir.create("Maps")}
  if(!dir.exists("Archive")){dir.create("Archive")}
  #Create sub-folders within folders
  if(dir.exists("Archive")){dir.create("/home/pablo/Documents/R desktop/R Warehouse/Data wrangling/Data wrangling/Project_01/subfolder_name")}
  #if(dir.exists("Archive")){dir.create(~/Archive."Archive_Sub")}
  #if(!dir.exists(".Archive/Test_subf")){dir.create(file.path(getwd(),"subfolder"), recursive = TRUE)}  
} 

# Run code below to use function and create folder structure
project_setup()


