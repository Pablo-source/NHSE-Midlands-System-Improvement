# FUNCTION TO EASY LOADING DATA FROM SEVERAL .CSV FILES                                     ##
# 27/05/2022

# CREATE DUMMY DATA from PENGUINS DATASET #

# Function: write_csv; Package: readr                                          #
# install.packages("tidyverse",dependencies = TRUE)

# Use pacman package to load several libraries at once

# install.packages("pacman")
pacman::p_load(readr,here,dplyr,ggplot2,purrr)

#library(readr)
#library(here)
#library(dplyr)
#library(ggplot2)
#library(purrr)

# 1 Create directory to save data set backup from readr package
if(!dir.exists("data")){dir.create("data")}
write_csv(palmerpenguins::penguins,here("data","penguins.csv"))
list.files("data/",".csv")

Mypenguins <- palmerpenguins::penguins
nrow(Mypenguins)

# 2 Split penguin main dataset into three files
#Indexing data [rows,cols]
# Slice the first 100 rows into a new dataset
nrow(Mypenguins)
Peng1 <-Mypenguins[1:100,]
Peng2 <-Mypenguins[101:201,]
Peng3 <-Mypenguins[202:344,]

if(!dir.exists(".data/SINGLEF")){dir.create("data/SINGLEF")}

write_csv(Peng1,here("data/SINGLEF","penguins1.csv"))
write_csv(Peng2,here("data/SINGLEF","penguins2.csv"))
write_csv(Peng3,here("data/SINGLEF","penguins3.csv"))

# 01 FINAL SCRIPT TO READ AND COMBINE ALL FILES IN A DIRECTORY

# Using the PURR PACKGE (FOR THE map_df() function)

files_to_comb <- list.files(path = "data/SINGLEF/",
                            pattern="*.csv",
                            full.names = T)
files_to_comb

combined_DATA <- list.files(path = "data/SINGLEF/",
                            pattern="*.csv",
                            full.names = T) %>%
                            map_df(~read_csv(.))

# This would be the content of the whole function
# 1. List all files in any given directory
# (Run this first of step 2)
ALLFIES <-list.files("data/SINGLEF/",".csv")
ALLFIES

NFILES <-length(list.files("data/SINGLEF/",".csv"))
NFILES

# 2 Load individual files on your workspace 
# myfile_Name <-c("penguins1.csv","penguins2.csv","penguins3.csv")
for(i in 1:NFILES) {  
  assign(ALLFIES[i],
     read_csv(paste0("data/SINGLEF/",ALLFIES[i]))
  )
}

rm(combined_DATA.csv)

# 3. Then we combine them using list.files and map_df functions
combined_DATA <- list.files(path = "data/SINGLEF/",
                            pattern="*.csv",
                            full.names = T) %>%
                            map_df(~read_csv(.))

# 4 Finally we write out combined file
write_csv(combined_DATA,here("data/SINGLEF","combined_DATA.csv"))

# JUST FOR TESTING PURPOSES. (REMOVE APPENDEN FILE)
file.remove('data/SINGLEF/combined_DATA.csv')


