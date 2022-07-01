# R Script : 01 Histopathology_weekly_report.R
# Date: 29/06/2022

#[1] "N:/_Everyone/Lee_CA/HistoPathReturns/PathologyReturns"

# Libraries 
library(readxl)
library(dplyr)
library(here)
library(writexl)
library(tidyverse)

# 0 Check where here point to
here()

# 1. List files on "Input_files" folder

list.files('Input_files')

# INSTRUCTIONS> Select and run script up to line 210 to load three required functions
# # Function section Tab 01-03: HLS_diagnostics_report()
# # Function section Tab 02-03: UH_diagnostics_report()
# # Function section Tab 03-03: RH_diagnostics_report()

# We will run three function on each file to produce the required three outputs 
# HLS_diagnostics_report("ME1 20220612.xlsx","ME1 20220612.xlsx","ME1 20220612.xlsx")
# UH_diagnostics_report("ME1 20220612.xlsx","ME1 20220612.xlsx","ME1 20220612.xlsx")
# RH_diagnostics_report("ME1 20220612.xlsx","ME1 20220612.xlsx","ME1 20220612.xlsx")

# TAB01 HLS
# Part 01-03
HLS_diagnostics_report <-function(DataA,NetworkA,WeekA) {  
  
  # Tab0103OUTPUT > HLS tab formatted output  
  
  # A. Provide EXcel file name for InfileA HLS_tab function argument  
  Tab10202in <- read_excel(here("Input_files",DataA),sheet = 1,range = "C10:F18",skip = 1,na = "")
  
  Tab1TABLE0202<- Tab10202in %>% pivot_longer(names_to = "metric", cols = 2:ncol(Tab10202in))
  Tab0103 <-Tab1TABLE0202
  
  # B. Provide EXcel file name for InfileB HLS_tab function argument  
  Network_name_prep <- read_excel(here("Input_files",NetworkA),sheet = 1,range = "C3:D4",skip = 1,na = "")
  
  Network_name_prep1 <-  Network_name_prep %>% select(starts_with("...2")) 
  Network_name_prep2 <- Network_name_prep %>% select(Network_name = "...2") 
  
  # C. Provide EXcel file name for InfileC HLS_tab function argument  
  Week_ending_prep <- read_excel(here("Input_files",WeekA),sheet = 1,range = "C7:D8",skip = 1,na = "")
  Week_ending_prep
  
  
  rm(Tab10202in, Tab1TABLE0202)
  
  Week_ending <-  Week_ending_prep %>% 
    rename(exclude = 1,Week_ending =2) %>% 
    # Turn  POSIXct date into date format in R
    mutate(week_ending = as.Date(Week_ending)) %>% 
    select(week_ending)
  Week_ending
  
  # Now we merge it with original table
  Tab0103OUTPUT <- cbind.data.frame(Tab0103,Network_name_prep2,Week_ending)
  Tab0103OUTPUT
  
  # just keep relevant dataframes
  rm(list=ls()[! ls() %in% c("Tab0103","Tab0103OUTPUT")])
  
  # Output HLS_tab output 
  write_xlsx(Tab0103OUTPUT,here("Output_files",'High_Level_Summary_tab_0103.xlsx'))
  
  # HLS_tab("ME1 20220612.xlsx","ME1 20220612.xlsx","ME1 20220612.xlsx")
  
  
}            


# TAB02 UH
# Part 02-03
UH_diagnostics_report <-function(DataB,NetworkB,WeekB) {
  
  # TAB 02/03: Urgent Histopathology
  #[..........................]
  
  TAB2input <- read_excel(here("Input_files",DataB),sheet = 2,range = "C4:H254",skip = 1,na = "") # Specify NA values
  
  TAB2input_subset <- TAB2input[ , c("Speciality")]    
  
  # …and then we can apply the complete cases function to exclude all rows of our original data based on this subset:
  TAB2notna <- TAB2input[complete.cases(TAB2input_subset),]
  
  # Replicate calculations to obtain the share of "Reported within 7 days" and *Reported beyond 7 days" as in the templae
  # Total reported
  TAB2calc_prep <- TAB2notna %>% 
    mutate(
      Total_Rported = `Reported beyond 7 days`+ `Reported within 7 days`,
      Reported_within_7D = (`Reported within 7 days`/ Total_Rported),
      Reported_beyond_7D = (`Reported beyond 7 days`/ Total_Rported)
    )
  
  
  Tab0203 <-TAB2calc_prep %>% select("Speciality","site", "Reported within 7 days" ,"Reported beyond 7 days",
                                     "Total Unreported",  "Total_Rported" , "Reported_within_7D","Reported_beyond_7D")
  
  rm(TAB2calc_prep,TAB2notna)
  
  # Add network name column to output table 
  
  # B. Provide EXcel file name for InfileB HLS_tab function argument  
  
  Network_name_prep <- read_excel(here("Input_files",             
                                       NetworkB),sheet = 1,range = "C3:D4",skip = 1,na = "")
  
  Network_name_prep1 <-  Network_name_prep %>% select(starts_with("...2")) 
  Network_name_prep2 <- Network_name_prep %>%  select(Network_name = "...2") 
  
  # 3 Add week ending column to output table
  
  # C. Provide EXcel file name for InfileC HLS_tab function argument 
  Week_ending_prep <- read_excel(here("Input_files",             
                                      WeekB),
                                 sheet = 1,range = "C7:D8",skip = 1,na = "")
  
  
  # Format the date and column name to merge it with original table  
  Week_ending <-  Week_ending_prep %>% 
    rename(exclude = 1,Week_ending =2) %>% 
    # Turn  POSIXct date into date format in R
    mutate(week_ending = as.Date(Week_ending)) %>% 
    select(week_ending)
  Week_ending
  
  # Now we merge new columns with initial table
  Tab0203OUTPUT<- cbind.data.frame(Tab0203,Network_name_prep2,Week_ending)
  Tab0203OUTPUT
  
  # just keep relevant dataframes
  rm(list=ls()[! ls() %in% c("Tab0203OUTPUT","Tab0203","Network_name_prep2","Week_ending")])
  
  # Output HLS_tab output 
  write_xlsx(Tab0203OUTPUT,here("Output_files",'Urgent_Histopatology_tab_0203.xlsx'))
  
  
}



# TAB03 RH
# Part 03-03
RH_diagnostics_report <-function(DataC,NetworkC,WeekC) {            
  
  # TAB 03/03: Routine Histopathology
  #[..........................]
  
  # 1 BUILD MAIN TABLE
  
  # A. Provide EXcel file name for InfileA HLS_tab function argument 
  TAB3input <- read_excel(here("Input_files",DataC),sheet = 3,range = "C4:H254",skip = 1,na = "") # Specify NA values
  TAB3input
  
  TAB3input_subset <- TAB3input[ , c("Speciality")]      
  
  # …and then we can apply the complete cases function to exclude all rows of our original data based on this subset:
  TAB3notna <- TAB3input[complete.cases(TAB3input_subset),]
  
  # Replicate calculations to obtain the share of "Reported within 7 days" and *Reported beyond 7 days" as in the templae
  # Total reported
  TAB3calc_prep <- TAB3notna %>% 
    mutate(
      Total_Rported = `Reported beyond 10 days`+ `Reported within 10 days`,
      Reported_within_10D = (`Reported within 10 days`/ Total_Rported),
      Reported_beyond_10D = (`Reported beyond 10 days`/ Total_Rported)
    )
  
  names(TAB3calc_prep)
  
  TAB3calc <-TAB3calc_prep %>%  select("Speciality","site", "Reported within 10 days" ,"Reported beyond 10 days",
                                       "Total Unreported",  "Total_Rported" , "Reported_within_10D","Reported_beyond_10D")
  
  
  rm(TAB3calc_prep,TAB3notna)
  
  # Add network name column to output table 
  # B. Provide EXcel file name for InfileB HLS_tab function argument 
  Network_name_prep <- read_excel(here("Input_files",NetworkC),sheet = 1,range = "C3:D4",skip = 1,na = "")
  
  Network_name_prep1 <-  Network_name_prep %>%  select(starts_with("...2")) 
  Network_name_prep2 <- Network_name_prep %>%   select(Network_name = "...2") 
  
  # 3 Add week ending column to output table
  
  # C. Provide EXcel file name for InfileC HLS_tab function argument 
  Week_ending_prep <- read_excel(here("Input_files",WeekC),sheet = 1,range = "C7:D8",skip = 1,na = "")
  
  
  Week_ending <-  Week_ending_prep %>% 
    rename(exclude = 1,Week_ending =2) %>% 
    # Turn  POSIXct date into date format in R
    mutate(week_ending = as.Date(Week_ending)) %>% 
    select(week_ending)
  Week_ending
  
  # Now we merge new columns with initial table
  TABLE0303OUTPUT  <- cbind.data.frame(TAB3calc,Network_name_prep2,Week_ending)
  TABLE0303OUTPUT
  
  # just keep relevant dataframes
  rm(list=ls()[! ls() %in% c("TABLE0303OUTPUT","Network_name_prep2","Week_ending")])
  
  # Output HLS_tab output 
  write_xlsx(TABLE0303OUTPUT,here("Output_files",'Routine_Histopatology_tab_0303.xlsx'))
  
}  

# Run up to this line to load function
## RUN LIBRARIES
## HOWTO PRODUCE DIAGNOSTICS REPORT
# 1.LOAD FUNCTION
# We need three input files for this function
# infileA: ME1 20220612.xlsx
# infileB: ME1 20220612.xlsx
# infuleC: ME1 20220612.xlsx
# for each file returned input filename three times

# Select and Run Line 209 to execute Diagnostics function ##
# 2. Select and run LINE 86 to run report, Outputs will be stored in  "Output_files" folder

# FOR EACH FILE on your "Input_files" folder, run the three functions below

# 1. List files on "Input_files" folder

list.files('Input_files')

# Function section Tab 01-03
HLS_diagnostics_report("ME1_20220626.xlsx","ME1_20220626.xlsx","ME1_20220626.xlsx")
HLS_diagnostics_report("ME2_20220626.xlsx","ME2_20220626.xlsx","ME2_20220626.xlsx")
HLS_diagnostics_report("ME3_20220626.xlsx","ME3_20220626.xlsx","ME3_20220626.xlsx")
HLS_diagnostics_report("ME4_20220626.xlsx","ME4_20220626.xlsx","ME4_20220626.xlsx")
HLS_diagnostics_report("N8_20220626.xlsx","N8_20220626.xlsx","N8_20220626.xlsx")

# Function section Tab 02-03
UH_diagnostics_report("ME1_20220626.xlsx","ME1_20220626.xlsx","ME1_20220626.xlsx")
UH_diagnostics_report("ME2_20220626.xlsx","ME2_20220626.xlsx","ME2_20220626.xlsx")
UH_diagnostics_report("ME3_20220626.xlsx","ME3_20220626.xlsx","ME3_20220626.xlsx")
UH_diagnostics_report("ME4_20220626.xlsx","ME4_20220626.xlsx","ME4_20220626.xlsx")
UH_diagnostics_report("N8_20220626.xlsx","N8_20220626.xlsx","N8_20220626.xlsx")

# Function section Tab 03-03
RH_diagnostics_report("ME1_20220626.xlsx","ME1_20220626.xlsx","ME1_20220626.xlsx")
RH_diagnostics_report("ME2_20220626.xlsx","ME2_20220626.xlsx","ME2_20220626.xlsx")
RH_diagnostics_report("ME3_20220626.xlsx","ME3_20220626.xlsx","ME3_20220626.xlsx")
RH_diagnostics_report("ME4_20220626.xlsx","ME4_20220626.xlsx","ME4_20220626.xlsx")
RH_diagnostics_report("N8_20220626.xlsx","N8_20220626.xlsx","N8_20220626.xlsx")
