library(tidyverse)
library(googlesheets4)
library(rlist)
library(tidycensus)
library(zoo) # moving averages 

# Double-check you are in the appropriate top working directory (e.g. "Tracker Data") before running this script

# NOTE: This is the view-only version of the Google Sheet, up to date as of May 24, 2021.
# Contact the authorship team for the most up-to-date dataset.

sheetinfo<-gs4_get("https://docs.google.com/spreadsheets/d/1h-17aeB69PnSZozCsOaY7nzDC7qX3S7Jo97SQ1xvIic/edit#gid=0") #This imports the information about to google sheet to get the number of tabs
1  #this selects the "yes" option for caching the OAuth access credentials (not used for view-only data?)
sheetnames<-sheetinfo[[6]][,1] %>% filter(stringr::str_count(name)==10) #this excludes the tabs in the google spreadsheet that were not dates (i.e. have ten characters)
nweeks<-length(sheetnames$name)   #this creates a variable called nweeks that is the number of date tabs in the google spreadsheet

#nweeks<-32 ## remove this later and use the above!

#This code pulls in each tab of the google sheet into a list called weeklydata. R will not pull in all the weeks in one go (it will get timed out), so We will do it in two batches.
weeklydata<-vector(mode="list") #creates an empty list to put each weekly sheet into in the loop below

#batch 1
#This also transfers the zeros into NAs because they represent missing data not actual zeros. And they mess up the weekly calculation!
for(i in 1:28){
  weeklydata[[i]]<-read_sheet("https://docs.google.com/spreadsheets/d/1h-17aeB69PnSZozCsOaY7nzDC7qX3S7Jo97SQ1xvIic/edit#gid=0", 
                              sheet=i, range="A4:K56",col_names = c("state","date","cases_total","cases_men"	,
                                                                    "cases_men_percent",	"cases_women" ,	
                                                                    "cases_women_percent","deaths_total",	
                                                                    "deaths_men",	"deaths_men_percent",	"deaths_women")) %>% 
                              mutate(cases_total = case_when(cases_total == 0 ~ NA_real_,TRUE ~ as.numeric(cases_total)),
                                     cases_women = case_when(cases_women == 0 ~ NA_real_,TRUE ~ as.numeric(cases_women)),
                                     cases_men = case_when(cases_men == 0 ~ NA_real_,TRUE ~ as.numeric(cases_men)),
                                     deaths_total = case_when(deaths_total == 0 ~ NA_real_,TRUE ~ as.numeric(deaths_total)),
                                     deaths_women = case_when(deaths_women == 0 ~ NA_real_,TRUE ~ as.numeric(deaths_women)),
                                     deaths_men = case_when(deaths_men == 0 ~ NA_real_,TRUE ~ as.numeric(deaths_men))
                              ) 
                          }

#batch 2
for(i in 29:nweeks){
  weeklydata[[i]]<-read_sheet("https://docs.google.com/spreadsheets/d/1h-17aeB69PnSZozCsOaY7nzDC7qX3S7Jo97SQ1xvIic/edit#gid=0", 
                              sheet=i, range="A4:K56",col_names = c("state","date","cases_total","cases_men"	,"cases_men_percent",
                                                                    "cases_women" ,	"cases_women_percent","deaths_total",	"deaths_men",	
                                                                    "deaths_men_percent",	"deaths_women"))  %>% 
                              mutate(cases_total = case_when(cases_total == 0 ~ NA_real_,TRUE ~ as.numeric(cases_total)),
                                     cases_women = case_when(cases_women == 0 ~ NA_real_,TRUE ~ as.numeric(cases_women)),
                                     cases_men = case_when(cases_men == 0 ~ NA_real_,TRUE ~ as.numeric(cases_men)),
                                     deaths_total = case_when(deaths_total == 0 ~ NA_real_,TRUE ~ as.numeric(deaths_total)),
                                     deaths_women = case_when(deaths_women == 0 ~ NA_real_,TRUE ~ as.numeric(deaths_women)),
                                     deaths_men = case_when(deaths_men == 0 ~ NA_real_,TRUE ~ as.numeric(deaths_men)))
                          }

# this code creates the weekly numbers of cases and deaths for total, men and women by subtracting each cumulative total from the previous weeks cumulative total
# I am ROUNDING the case and death counts to whole numbers. The reason there are fractions in the dataset is because in some instances the 
# count was derived from a percentage i.e., the percent of cases among women was multiplied by the total cases in the state to get the number of cases among women
# I am rounding to avoid some negative weekly counts that were resulting for strange decimals and because you cannot really have a fraction of a case/death

for (i in 2:nweeks){
  weeklydata[[i]]$deaths_total_oneweek <- round(weeklydata[[i]]$deaths_total) - round(weeklydata[[i-1]]$deaths_total)
  weeklydata[[i]]$deaths_men_oneweek <- round(weeklydata[[i]]$deaths_men) - round(weeklydata[[i-1]]$deaths_men)
  weeklydata[[i]]$deaths_women_oneweek <- round(weeklydata[[i]]$deaths_women) - round(weeklydata[[i-1]]$deaths_women)
  
  weeklydata[[i]]$cases_total_oneweek <- round(weeklydata[[i]]$cases_total) - round(weeklydata[[i-1]]$cases_total)
  weeklydata[[i]]$cases_men_oneweek <- round(weeklydata[[i]]$cases_men) - round(weeklydata[[i-1]]$cases_men)
  weeklydata[[i]]$cases_women_oneweek <- round(weeklydata[[i]]$cases_women) - round(weeklydata[[i-1]]$cases_women)
  
}

weeklydata<-weeklydata[-1]  #remove week1 because we cannot get a weekly total for that week (nothing to subtract from it)
combinedweeks = do.call(rbind, weeklydata) # combine all the weekly sheets in the list into one dataframe

#create the long dataframe

women<-combinedweeks %>% 
  select(state, date, cases_women, deaths_women, deaths_women_oneweek, cases_women_oneweek) %>% 
  rename(cases=cases_women, deaths=deaths_women, deaths_oneweek=deaths_women_oneweek, cases_oneweek=cases_women_oneweek) %>% 
  mutate (sex="women")

men<-combinedweeks %>% 
  select(state, date, cases_men, deaths_men, deaths_men_oneweek, cases_men_oneweek) %>% 
  rename(cases=cases_men, deaths=deaths_men, deaths_oneweek=deaths_men_oneweek, cases_oneweek=cases_men_oneweek) %>% 
  mutate (sex="men")

total<-combinedweeks %>% 
  select(state, date, cases_total, deaths_total, deaths_total_oneweek, cases_total_oneweek) %>% 
  rename(cases=cases_total, deaths=deaths_total, deaths_oneweek=deaths_total_oneweek, cases_oneweek=cases_total_oneweek) %>% 
  mutate (sex="total")

#combine the men, women and total into one dataframe
RawTrackerData<-rbind(total,men, women)

write.csv(RawTrackerData,file="Data/Unvalidated Tracker Data/RawTrackerData.csv")

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
mergeddata_long<-merge(RawTrackerData,ACS_dat, by=c("state", "sex"), all.x=TRUE ) %>% 
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
  select(state, sex, date, cases_rate, deaths_rate,cases_oneweek_rate, deaths_oneweek_rate) %>% 
  pivot_wider(names_from = sex, values_from =c(cases_rate, deaths_rate,cases_oneweek_rate,deaths_oneweek_rate)) %>% 
  mutate(cases_rate_IRR = cases_rate_men/cases_rate_women, 
         cases_rate_IRD = cases_rate_men-cases_rate_women,
         deaths_rate_IRR = deaths_rate_men/deaths_rate_women,
         deaths_rate_IRD = deaths_rate_men-deaths_rate_women,
         cases_oneweek_rate_IRR = cases_oneweek_rate_men/cases_oneweek_rate_women,
         cases_oneweek_rate_IRD = cases_oneweek_rate_men-cases_oneweek_rate_women,
         deaths_oneweek_rate_IRR = deaths_oneweek_rate_men/deaths_oneweek_rate_women,
         deaths_oneweek_rate_IRD = deaths_oneweek_rate_men-deaths_oneweek_rate_women) %>% 
  select (state, date, cases_rate_IRR, cases_rate_IRD,deaths_rate_IRR, deaths_rate_IRD,cases_oneweek_rate_IRR, cases_oneweek_rate_IRD, deaths_oneweek_rate_IRR, deaths_oneweek_rate_IRD)

# save files
save(mergeddata_long,file="Data/Unvalidated Tracker Data/tracker.long.RData")
save(mergeddata_wide,file="Data/Unvalidated Tracker Data/tracker.wide.RData")
