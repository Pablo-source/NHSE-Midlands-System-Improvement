# 03 Line chart data long format.R  

# Example on how to import a specific table from this Excel file below:
# UK Goods trade OBR March 2022 report.xlsx 
#
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
         labs(title ="UK Goods trade OBR March 2022 report",
              subtitle = "Multiple colour line chart by data series")
PLOT01

ggsave(paste0("UK Goods trade OBR March 2022 report",".jpeg"),width = 30, height = 20, dpi = 150, units = "cm")

