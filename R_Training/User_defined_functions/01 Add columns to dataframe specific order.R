# 01 Add Columns In Excel
# Date 11/04/2022

# Using DPLYR package

# FUNCTION WE WILL EXPLORE IN THIS SCRIPT
# LIBRARY (dplyr)
# add_column(Col_Name_new  = "column_content", .after ="RAS")
# add_column(Col_Name_before  = "column_content", .before ="RC9")

Date <-as.Date(c('2020-03-11','2020-03-12','2020-03-13','2020-03-14','2020-03-15'),"%Y-%m-%d")
RAS <-c(123,212,214,147,195)
R1A <-c(24,54,87,46,89)
RC9 <-c(24,54,87,46,89)
R1H <-c(87,46,75,95,12)


Mydata <- cbind.data.frame(Date,RAS,R1A,RC9,R1H)
Mydata

# Add new column in DPLYR using Column_Before
library(tidyverse)

# 2. Add an empty column (By default it will be added to the end)
Newcol <- Mydata %>%
          add_column(Empty = NA)
Newcol

# 2. Add an empty column after RAS
Newcol_after <- Mydata %>%
                add_column(Col_Name_new  = "column_content", .after ="RAS")
Newcol_after


# 3. Add an empty column before RC9
Newcol_before <- Mydata %>%
                 add_column(Col_Name_before  = "column_content", .before ="RC9")
Newcol_before

# Add Columns from One Dataframe to Another Dataframe
# Read data from the .xlsx files:
dataf <- read_excel('./SimData/add_column.xlsx')
dataf2 <- read_excel('./SimData/add_column2.xlsx')

# Add the columns from the second dataframe to the first
dataf3 <- cbind(dataf, dataf2[c("Anx1", "Anx2", "Anx3")])

