
# Reference
# https://cran.r-project.org/web/packages/here/vignettes/here.html

# Specific uses of HERE library

install.packages("here")
library(here)

# 1. Define relative path to Project directory
here::i_am("RmdFolder/20220428_AW_Health_Inequalities.Rmd")

# 2. Create folder structure making use of HERE package
project_setup <-function(){
  if(!dir.exists("Data")){dir.create(here::here("Data"))}
  if(!dir.exists("Test")){dir.create(here::here("Test"))}
  if(!dir.exists("Report")){dir.create(here::here("Report"))}
  if(!dir.exists("Model")){dir.create(here::here("Model"))}
}
# Execute the function
project_setup()

# 3. Create relative paths using Here package
data_path <- here("data")
here(data_path)
#> [1] "/tmp/RtmpnoXhd8/Rinst301434451937/here/demo-project/data"
here(data_path, "penguins.csv")

 
