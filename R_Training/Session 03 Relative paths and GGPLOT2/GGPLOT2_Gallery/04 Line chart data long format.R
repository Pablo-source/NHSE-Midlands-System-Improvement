# 04 Line chart data long format.R  

## AIM
# 1. Import data from Excel file
# 2. Deal with 2016 JAN date format, converting it to standard R format 

# Example on how to import a specific table from this Excel file below:
# UK Goods trade OBR March 2022 report.xlsx 

# Import table starting in line 26 that populates "B26:F98" range of cells 
library(tidyverse)
library(readxl)
library(here)
library(janitor)

list.files (path = "./data" ,pattern = "xlsx$")
excel_sheets("./data/UK Goods trade OBR March 2022 report.xlsx")

Goods_trade <- read_excel(here("data","UK Goods trade OBR March 2022 report.xlsx"),
                          sheet ="C2.H", range = "B26:F98", skip = 25, na = "") %>% 
                          clean_names() %>% 
                          select(Date = x1, eu_exports, non_eu_exports, eu_imports, non_eu_imports)
Goods_trade

# Pivot longer
Goodslg <- Goods_trade %>% pivot_longer(names_to = "Trade", cols = 2:ncol(Goods_trade))
Goodslg

names(Goodslg)
# [1] "Date"  "Trade" "value"

# Plot
PLOT01 <-ggplot(data = Goodslg, aes( x = Date, y = value, group = Trade, colour = Trade)) + 
         geom_line() +
         labs(title ="UK Goods trade OBR March 2022 report")
PLOT01

ggsave(paste0("UK Goods trade OBR March 2022 report",".jpeg"),width = 30, height = 20, dpi = 150, units = "cm")

# First we need to fix the date format
head(Goodslg)
names(Goodslg)
# [1] "Date"  "Trade" "value"
# A tibble: 6 × 3
#Date     Trade          value
#<chr>    <chr>          <dbl>
#  1 2016 JAN eu_exports      93.5
#  2 2016 JAN non_eu_exports  87.8


# Transform Date varianle into the standard R date format 
# Important, to get the right date format, w need to add a 01 to the original date
date <- "2016 JAN"
daten <-paste0("2016","/","JAN","/","01")
daten
datef <- as.Date(daten, format = "%Y/%b/%d")
datef
str(datef)
# Date[1:1], format: "2016-01-01"

# > Dataf
# A tibble: 288 × 4
# Date     Trade          value Year 
# <chr>    <chr>          <dbl> <chr>
# 1 2016 JAN eu_exports      93.5 2016 
# 2 2016 JAN non_eu_exports  87.8 2016 
# 3 2016 JAN eu_imports      93.0 2016 
# 4 2016 JAN non_eu_imports  95.7 2016 
# 5 2016 FEB eu_exports      94.1 2016 
# 6 2016 FEB non_eu_exports  88.7 2016 
# 7 2016 FEB eu_imports      93.3 2016 
# 8 2016 FEB non_eu_imports  96.4 2016 
# 9 2016 MAR eu_exports      94.0 2016 
# 10 2016 MAR non_eu_exports  88.9 2016 

# FIX data variable format from previous data set 
# Save previous data set as Dataf and implement fix for date format
Dataf <- Goodslg
head(Dataf)

# We use substring(variable,start,end) to extract each date part from original 
# variable
Dataf <- Goodslg %>% 
         select(Date, Trade, value) %>% 
         mutate(
                Year = substring(Date,1,4),
                Month = substring(Date,6,8),
                Day = 01,
                date = paste0(Year,"/",Month,"/",Day)
              ) %>% 
        mutate(datef = as.Date(date, format = "%Y/%b/%d"))
        
Dataf

# Now the data is in the right format for us to plot it
Plot_data <- Dataf %>% 
             select(datef, Trade, value) 
Plot_data

# > Plot_data
# A tibble: 288 × 3
# datef      Trade          value
# <date>     <chr>          <dbl>
# 1 2016-01-01 eu_exports      93.5
# 2 2016-01-01 non_eu_exports  87.8
# 3 2016-01-01 eu_imports      93.0
# 4 2016-01-01 non_eu_imports  95.7
# 5 2016-02-01 eu_exports      94.1

# Second plot with the right date format
PLOT02 <-ggplot(data = Plot_data, aes( x = datef, y = value, group = Trade, colour = Trade)) + 
  geom_line() +
labs(title ="UK Goods trade OBR March 2022 report",
     subtitle = "Multiple colour line chart formatted date variables")
PLOT02

ggsave(paste0("UK Goods trade OBR March 2022 report date formatted",".jpeg"),width = 30, height = 20, dpi = 150, units = "cm")
