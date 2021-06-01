library(RSocrata)
library(tidyverse)

#import cases from CDC COVID data: https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data-with-Ge/n8mc-b4w4

CDC_Cases <- read.socrata("https://data.cdc.gov/resource/n8mc-b4w4.json") %>% 
             select(case_month, sex) %>% 
             rename(date = case_month) %>% 
             group_by(date, sex) %>% 
             summarize (cases_monthly = n())