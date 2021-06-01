library(tidyverse)
library(rlist)
library(tidycensus)
library(lubridate)
library(zoo) # moving averages 

# Double-check you are in the appropriate top working directory (e.g. "Tracker Data") before running this script
# you need to have run both Step1a and Step1b code and placed your datasets in the appropriate directories for this code to work.

validated<-read.csv(file="Data/Validated Tracker Data/RawTrackerData.csv") %>% select(-X) %>% mutate(date=ymd(date) )
unvalidated<-read.csv(file="Data/Unvalidated Tracker Data/RawTrackerData.csv")  %>% select(-X)%>% mutate(date=ymd(date) )
 

## Anti_join the unvalidated to the validated to get the rows that are not in the validated dataset
        anti_join_unvalidated<-anti_join(unvalidated,validated, by = c("state", "sex", "date"))
        

### Now rowbind the anti-join with the validated data
        combined_val_unval<-rbind(anti_join_unvalidated,validated)
        
### Pull in the census denominators for each state and sex group
census_api_key(my_census_key)

#vars <- load_variables(2019, "acs5")  #This is loading the variables for the 2015-2019 5-year ACS estimates
ACS_dat<- get_acs(geography = "State",
                  variables = c('B01001_001', #total pop
                                'B01001_002', #total pop men
                                'B01001_026'  #total pop women
                  ),
                  year=2019) %>% 
        rename(state=NAME, population=estimate) %>% 
        mutate(sex=case_when(variable=="B01001_001"~ "total",
                             variable=="B01001_002"~ "men",
                             variable=="B01001_026"~ "women"))%>% 
        select(state, population, sex) 

#merge the population denominators with the COVID data
mergeddata_long<-merge(combined_val_unval,ACS_dat, by=c("state", "sex"), all.x=TRUE ) %>% 
        mutate(
                cases = replace(cases, which(cases<0), NA),  # REPLACING THE NEGATIVES WITH NA
                deaths = replace(deaths, which(deaths<0), NA),# REPLACING THE NEGATIVES WITH NA
                cases_oneweek = replace(cases_oneweek, which(cases_oneweek<0), NA),# REPLACING THE NEGATIVES WITH NA
                deaths_oneweek = replace(deaths_oneweek, which(deaths_oneweek<0), NA),# REPLACING THE NEGATIVES WITH NA 
                
                
               cases_rate=cases/population*100000,
               deaths_rate=deaths/population*100000,
               cases_oneweek_rate=cases_oneweek/population*100000,
               deaths_oneweek_rate=deaths_oneweek/population*100000) %>% 
        dplyr::arrange(desc(date)) %>% 
        dplyr::group_by(state,sex) %>% 
        mutate(cases_3wkrolling = zoo::rollmean(cases, k = 3, fill = NA),
               deaths_3wkrolling = zoo::rollmean(deaths, k = 3, fill = NA),
               cases_oneweek_3wkrolling = zoo::rollmean(cases_oneweek, k = 3, fill = NA),
               deaths_oneweek_3wkrolling = zoo::rollmean(deaths_oneweek, k = 3, fill = NA),
               cases_rate_3wkrolling = zoo::rollmean(cases_rate, k = 3, fill = NA),
               deaths_rate_3wkrolling = zoo::rollmean(deaths_rate, k = 3, fill = NA),
               cases_oneweek_rate_3wkrolling = zoo::rollmean(cases_oneweek_rate, k = 3, fill = NA),
               deaths_oneweek_rate_3wkrolling = zoo::rollmean(deaths_oneweek_rate, k = 3, fill = NA))

#Making a wide dataset so that I can do IRR and IRD calculations
mergeddata_wide<-mergeddata_long %>% 
        select(state, sex, date, cases_rate, deaths_rate,cases_oneweek_rate, deaths_oneweek_rate,deaths_rate_3wkrolling,deaths_oneweek_rate_3wkrolling) %>% 
        pivot_wider(names_from = sex, values_from =c(cases_rate, deaths_rate,cases_oneweek_rate,deaths_oneweek_rate,deaths_rate_3wkrolling,deaths_oneweek_rate_3wkrolling)) %>% 
        mutate(cases_rate_IRR = cases_rate_men/cases_rate_women, 
               cases_rate_IRD = cases_rate_men-cases_rate_women,
               deaths_rate_IRR = deaths_rate_men/deaths_rate_women,
               deaths_rate_IRD = deaths_rate_men-deaths_rate_women,
               cases_oneweek_rate_IRR = cases_oneweek_rate_men/cases_oneweek_rate_women,
               cases_oneweek_rate_IRD = cases_oneweek_rate_men-cases_oneweek_rate_women,
               deaths_oneweek_rate_IRR = deaths_oneweek_rate_men/deaths_oneweek_rate_women,
               deaths_oneweek_rate_IRD = deaths_oneweek_rate_men-deaths_oneweek_rate_women,
               
               deaths_rate_3wkrolling_IRR = deaths_rate_3wkrolling_men/deaths_rate_3wkrolling_women, 
               deaths_rate_3wkrolling_IRD = deaths_rate_3wkrolling_men-deaths_rate_3wkrolling_women,
               deaths_oneweek_rate_3wkrolling_IRR = deaths_oneweek_rate_3wkrolling_men/deaths_oneweek_rate_3wkrolling_women,
               deaths_oneweek_rate_3wkrolling_IRD = deaths_oneweek_rate_3wkrolling_men-deaths_oneweek_rate_3wkrolling_women
               ) %>% 
        select (state, date, cases_rate_IRR, cases_rate_IRD,deaths_rate_IRR, deaths_rate_IRD,cases_oneweek_rate_IRR, 
                cases_oneweek_rate_IRD, deaths_oneweek_rate_IRR, deaths_oneweek_rate_IRD,deaths_rate_3wkrolling_IRR,deaths_rate_3wkrolling_IRD,
                deaths_oneweek_rate_3wkrolling_IRR,deaths_oneweek_rate_3wkrolling_IRD)

# save files
save(mergeddata_long,file="Data/Combined Validated and Unvalidated Tracker Data/tracker.long.RData")
save(mergeddata_wide,file="Data/Combined Validated and Unvalidated Tracker Data/tracker.wide.RData")
