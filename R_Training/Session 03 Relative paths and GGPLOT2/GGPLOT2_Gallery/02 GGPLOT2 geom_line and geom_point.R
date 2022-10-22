## 02 GGPLOT2 geom_line and geom_point.R
library(readr)
library(tidyverse)

BOE_rate <- read_csv("Quoted Interest Rates New Vis_data.csv")

str(BOE_rate)

spec_tbl_df [381 × 4] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
$ Month of Date: chr [1:381] "March 2012" "April 2012" "May 2012" "June 2012" ...
$ Name         : chr [1:381] "Bank rate" "Bank rate" "Bank rate" "Bank rate" ...
$ Max. Value   : num [1:381] 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
$ Value (copy) : num [1:381] 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...

names(BOE_rate)

Rates <- BOE_rate %>% select(Date = 'Month of Date',
                             Name,
                             Rate = 'Max. Value')
Rates

# %b abbreviated month Jan
# %B unabbreviated month	 January
# %Y Four digits YEar 2013	 January

# Prepending 1 to the characters worked! as.Date("1 January 2008", "%d %B %Y")
Rates_date <- Rates %>% 
                   mutate(DATED = paste0(1," ",Date)) 
Rates_date

# Format date
Rates_fmtd <- Rates_date %>% 
              mutate(
                DATE = as.Date(DATED,format = "%d %B %Y")
              )
Rates_fmtd

# Remote extra data sets
rm(list=ls()[!(ls()%in%c('Rates_fmtd'))])
str(Rates_fmtd)

## Subset first five rows

# Use top_n(number_of_rows) to subset top_n rows
# https://dplyr.tidyverse.org/reference/top_n.html
# top_n(2) # Two highest values
# tops_n(-2) # Two lowest values

# 
# https://dplyr.tidyverse.org/reference/slice.html
# Get first n rows based on existing order
# slice_head(n = 5)
# Get last n rows based on existing order
# slice_tail(n = 5)

Total_months <- nrow(Rates_fmtd)
Total_months
# [1] 381

# Check min and max date values
Min_Dates  <-min(Rates_fmtd$DATE)
Min_Dates
# [1] "2012-03-01"

Max_Dates <-max(Rates_fmtd$DATE)
Max_Dates
#[1] "2013-10-01"

# Test to plot just the first 121 data points
Rates_prd <- Rates_fmtd %>% 
              select (DATE,Rate) %>% 
              slice_head(n = 127)
Rates_prd

Min_Dates_prd   <-min(Rates_prd$DATE)
Min_Dates_prd
# [1] "2012-03-01"

Max_Dates_prd <-max(Rates_prd$DATE)
Max_Dates_prd
# [1] "2022-09-01"
# PLot it 

Interestr <-ggplot(Rates_prd, aes( x = DATE, y = Rate)) +
            geom_line(colour = "deepskyblue") +
            geom_point(colour = "deepskyblue") +
            labs(title ="BoE Interest rate 1st March to 1st Sep 2022")
Interestr

#ggsave(paste0("Geom_line_plot_chart",".jpeg"),width = 30, height = 20, dpi = 150, units = "cm")

# INCLUDE LATESTE DATE AVAILABLE LABEL IN THE PLOT

# Filter data to display latest date available
# install.packages("ggrepel")
library(ggrepel)

Max_Dates_prd <-max(Rates_prd$DATE)
Max_Dates_prd

 data_ends <- Rates_fmtd %>% filter(DATE == Max_Dates_prd &
                                      Name =="Bank rate") %>% 
                             select(Rate,DATE)
 data_ends
 
# Include latest data point into the plot
 Interestr + geom_text_repel(aes(label = Rate), data = data_ends,fontface ="plain", color = "black", size = 3)
 Interestr
 
 ggsave(paste0("Geom_line_plot_chart_latest",".jpeg"),width = 30, height = 20, dpi = 150, units = "cm")
 
 