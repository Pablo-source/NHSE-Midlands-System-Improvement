## 01 Get Excel data into R
library(readxl)
library(here)
library(dplyr)
library(ggplot2)
library(janitor)

# INPUT FILE: RTT DATA DOWNLOADED FROM NHS WEBSITE
# https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2022-23/#Jun22
# England-level time series
# "RTT Overview Timeseries Jun 22(XLS,77k)68395
# https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/08/RTT-Overview-Timeseries-Jun22-XLS-77K-68395.xlsx


# Inspect element and copy OUTHER HTML
# <a href="https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/08/RTT-Overview-Timeseries-Jun22-XLS-77K-68395.xlsx" class="xls-link">RTT Overview Timeseries Jun22 (XLS, 77K) 68395</a>

# This is how we know the file name and path to its URL

# 1 List files on your data directory 
list.files(here("Data"))
[1] "RTT-Overview-Timeseries-Jun22-XLS-77K-68395.xlsx"

# 2 list sheet names 
excel_sheets(here("Data", "RTT-Overview-Timeseries-Jun22-XLS-77K-68395.xlsx"))


# 3 Import initial data and check for empty rows 
datan <- read_excel(here("Data", "RTT-Overview-Timeseries-Jun22-XLS-77K-68395.xlsx"), sheet = 1) %>% 
  clean_names()
datan

# As you can see, we have plenty of Missing rows at the top of our file

> datan
# A tibble: 210 × 28
title    referral_to_trea… x3    x4    x5    x6    x7    x8    x9    x10   x11   x12   x13   x14   x15   x16   x17   x18   x19   x20   x21   x22  
<chr>    <chr>             <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
  1 NA       NA                NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   
2 Period:  April 2007 to Ju… NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   
3 Source:  NHS England and … NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   
4 Basis:   Commissioner      NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   
5 Contact: england.rtt@nhs.… NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   
6 NA       NA                NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   
7 Year     Month             Inco… NA    NA    NA    NA    NA    NA    NA    NA    Admi… NA    NA    NA    NA    Non-… NA    NA    NA    NA    New …
8 NA       NA                Medi… 92nd… No. … % wi… No. … No. … % > … No. … Tota… Medi… 95th… No. … No. … No. … Medi… 95th… No. … No. … No. … No. …
9 2007/08  39173             -     -     -     -     -     -     -     -     -     17.4… 52+   94658 21169 2460… -     -     -     -     -     -    
  10 NA       39203             -     -     -     -     -     -     -     -     -     16.6… 52+   1149… 24605 2775… -     -     -     -     -     -    
  # … with 200 more rows, and 6 more variables: x23 <lgl>, x24 <chr>, x25 <chr>, x26 <chr>, x27 <chr>, x28 <chr>

# 4. We can clean it up by using skip function
# From the above data set, we can see that variable headings are displayed on row 8, previous rows are annotations on the Excel file for users
# We have 9 rows at the top that we would like to omit  
datan <- read_excel(here("Data", "RTT-Overview-Timeseries-Jun22-XLS-77K-68395.xlsx"), sheet = 1, skip = 9) %>% 
          clean_names()
datan

names(datan)
head(datan)

# Now we are able to see that the right variable names are starting to be displayed in our data set, although Columns One and two have no heeadings as they 
# where written on a different row in the original Excel file

# 5. We retain first two columns variables and also those related to "Admitted (unadjusted) RTT pathways, as they have 
# completed series of data

# use str() to obtain a detailed variable names list
str(datan)

# tibble [202 × 28] (S3: tbl_df/tbl/data.frame)
# $ x1                       : chr [1:202] "2007/08" NA NA NA ...
# $ x2                       : POSIXct[1:202], format: "2007-04-01" "2007-05-01" "2007-06-01" "2007-07-01" ...
# $ median_wait_weeks_12     : num [1:202] 17.5 16.7 16.4 16.4 15.6 ...
# $ x95th_percentile_weeks_13: chr [1:202] "52+" "52+" "52+" "52+" ...
# $ no_within_18_weeks_14    : num [1:202] 94658 114950 121336 123853 125334 ...
# $ no_52_weeks_15           : num [1:202] 21169 24605 25240 24672 22188 ...
# $ no_of_pathways_all_16    : num [1:202] 246013 277540 285744 297504 284512 ...

data_sub <- datan %>% 
            select(x1,x2,median_wait_weeks_12,x95th_percentile_weeks_13,x95th_percentile_weeks_13,
                   no_within_18_weeks_14,no_52_weeks_15,
                   no_of_pathways_all_16)
data_sub

# Now we can rename our vars with meaninful names
data_rename <- data_sub %>% 
               select(
                 FY = x1,
                 DATE = x2,
                 median_waitw12 = median_wait_weeks_12,
                 perc_95_w13 = x95th_percentile_weeks_13,
                 Adm_18w = no_within_18_weeks_14,
                 Adm_52w = no_52_weeks_15,
                 Adm_allpath = no_of_pathways_all_16)
data_rename

# we use select to rename variables and also subset for the set of variables we will use in the Shiny dashboard

# 6. Finally we format variables for the right data type
names(data_rename)

data_shiny <- data_rename %>% 
              mutate(
                Date = as.Date(DATE),
                median_waitw12 = as.integer(median_waitw12),
                perc95W13 = as.integer(perc_95_w13),
                adm18W = as.integer(Adm_18w),
                adm52W = as.integer(Adm_52w),
                admAll = as.integer(Adm_allpath)
              )
str(data_shiny)

rm(list=ls()[!(ls()%in%c('data_shiny'))]) 
# Now we can use this data_shiny data set to build our Shiny app 