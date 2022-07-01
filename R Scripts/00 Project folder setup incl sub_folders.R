## 00 Processed Folder setup.R 
# 01/07/2022

library(here)
# here() starts at N:/_Everyone/Lee_CA/HistoPathReturns/PathologyReturns
# [1] "N:/_Everyone/Lee_CA/HistoPathReturns/PathologyReturns/00 Processed folder setup.R"

here("00 Processed folder setup.R")

# Diagnostics project 
# Applied to Lee folder structure project


# This will be your function LEE

project_folder<-function(){
  
  
  if(!dir.exists("Archive")){dir.create("Archive")}
  if(!dir.exists("BackupScripts")){dir.create("BackupScripts")}
  if(!dir.exists("Checks")){dir.create("Checks")}
  if(!dir.exists("Input_files")){dir.create("Input_files")}
  if(!dir.exists("Output_files")){dir.create("Output_files")}
  if(!dir.exists("R_scripts")){dir.create("R_scripts")}
  if(!dir.exists("Test")){dir.create("Test")}
  
  # Create SUB_FOLDERS under Processed folder to transfer function output to each system folder (M1-M7)
  if(!dir.exists("Processed")){dir.create("Processed")}
  
  # Premises for sub_folders to work
  # a) You need to creae FIRST the parent folder
  # b) To create child files you need to use a POSITIVE if statement if(dir.exists())
  
  if(dir.exists("Processed")){dir.create("N:\\_Everyone\\Lee_CA\\HistoPathReturns\\PathologyReturns\\Processed\\ME1")}
  if(dir.exists("Processed")){dir.create("N:\\_Everyone\\Lee_CA\\HistoPathReturns\\PathologyReturns\\Processed\\ME2")}
  if(dir.exists("Processed")){dir.create("N:\\_Everyone\\Lee_CA\\HistoPathReturns\\PathologyReturns\\Processed\\ME3")}
  if(dir.exists("Processed")){dir.create("N:\\_Everyone\\Lee_CA\\HistoPathReturns\\PathologyReturns\\Processed\\ME4")}
  if(dir.exists("Processed")){dir.create("N:\\_Everyone\\Lee_CA\\HistoPathReturns\\PathologyReturns\\Processed\\ME5")}
  if(dir.exists("Processed")){dir.create("N:\\_Everyone\\Lee_CA\\HistoPathReturns\\PathologyReturns\\Processed\\ME6")}
  if(dir.exists("Processed")){dir.create("N:\\_Everyone\\Lee_CA\\HistoPathReturns\\PathologyReturns\\Processed\\ME7")}
  
  
}

# Now you execute the function

project_folder()
