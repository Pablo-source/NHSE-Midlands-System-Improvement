# R Script : 01 Histopathology_weekly_report.R
# Date: 29/06/2022

# FUNCTION TO EASY LOADING DATA FROM SEVERAL .CSV FILES     
# 27/05/2022 
# R Script: RScript_Append_MRR.R
# [1] "N:/_Everyone/Mona Kochhar/Outpatients/EROC SQL/Append"

library(readxl)
library(writexl)
library(dplyr)
library(here)

# List files in a sub folder 
here()
# [1] "N:/_Everyone/Mona Kochhar/Outpatients/EROC SQL/Append"

list.files("data/SINGLEF")

# 1. Check initial Excel file data structure

clean_data <-read_excel(here("data","SINGLEF","MRR_Prov-Web-file-August-20-REVISED-8th-July-2021.xls"),sheet=1,skip=13,na="")
clean_data


# 2. REad in files and append them into a single output dataset 
here()
combined <- list.files(path = "data/SINGLEF/",
                       pattern="*.xls",
                       full.names = T) %>%
  map_df(~read_excel(.))
combined

# Load in different files excluding top 13 rows
remove_top <- list.files(path = "data/SINGLEF/",
                         pattern="*.xls",
                         full.names = T) %>%
  map_df(~read_excel(.),sheet=1,skip=13,na="") 
remove_top

# 3. Exclude rows containing any NA values
# Remove any row with NA in it
data_complete <- remove_top[complete.cases(remove_top), ] # Create new data without missing values
data_complete

#here()
#> here()
#[1] "N:/_Everyone/Mona Kochhar/Outpatients/EROC SQL/Append"

# 4. Output appended file as .xlsx  Excel file
write_xlsx(data_complete,here("data","SINGLEF","Output","MMR_appended.xlsx"))
