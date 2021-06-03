rm(list=ls())
library(tidycensus)
library(writexl)
library(tidyverse)
library(stringr)
library(RSocrata)

# Double-check you are in the appropriate top working directory (e.g. "Tracker Data") before running this script

#### Import CDC Data from: https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Sex-Age-and-S/9bhg-hcku/data####
CDC_Deaths <- read.socrata("https://data.cdc.gov/resource/9bhg-hcku.json")

CDC_Deaths_a <- mutate(CDC_Deaths,  
                       wave = case_when(
                          start_date == "2020-01-01" ~ "Wave1",
                          start_date == "2020-02-01" ~ "Wave1",
                          start_date == "2020-03-01" ~ "Wave1",
                          start_date == "2020-04-01" ~ "Wave2",
                          start_date == "2020-05-01" ~ "Wave2",
                          start_date == "2020-06-01" ~ "Wave2",
                          start_date == "2020-07-01" ~ "Wave2",
                          start_date == "2020-08-01" ~ "Wave2",
                          start_date == "2020-09-01" ~ "Wave2",
                          start_date == "2020-10-01" ~ "Wave3",
                          start_date == "2020-11-01" ~ "Wave3",
                          start_date == "2020-12-01" ~ "Wave3",
                          start_date == "2021-01-01" ~ "Wave3",
                          start_date == "2021-02-01" ~ "Wave3",
                          start_date == "2021-03-01" ~ "Wave3"),
                        covid_deaths_num=as.numeric(covid_19_deaths)) %>% 
                        filter(group=="By Month" & sex!="All Sexes" & age_group=="All Ages" & state=="United States") %>% 
                        dplyr::select(state,sex,age_group,group,start_date,covid_deaths_num) %>% rename (month=start_date)

##### Import and formulate cumulative counts

#for males
m<-CDC_Deaths_a %>% dplyr::filter(sex=="Male")
m$covid_deaths_cumulative<-NA
n_m <- nrow(m)
m$covid_deaths_cumulative[1] <- m$covid_deaths_num[1]
for(i in 2:n_m){
  m$covid_deaths_cumulative[i] <- m$covid_deaths_cumulative[i-1] + m$covid_deaths_num[i]
}


#for females
f<-CDC_Deaths_a %>% dplyr::filter(sex=="Female")
f$covid_deaths_cumulative<-NA
n_f <- nrow(f)
f$covid_deaths_cumulative[1] <- f$covid_deaths_num[1]
for(i in 2:n_f){
  f$covid_deaths_cumulative[i] <- f$covid_deaths_cumulative[i-1] + f$covid_deaths_num[i]
}

CDC_USA_cumulative_monthly<-rbind(m,f)

###Get USA Population Demoninators from 2018 5-Year ACS###
census_api_key(my_census_key)
US_ACS<- get_acs(geography = "us",
                    variables = c("B01001_002","B01001_026"),year=2019) %>% #total for male, total for female in the USA
                    mutate(sex = case_when(variable == "B01001_002"~"Male",
                                           variable =="B01001_026" ~ "Female")) %>% 
                    rename (state = NAME, pop=estimate) %>% select(state, sex, pop)

NCovid_ACS_USADenoms<-merge(CDC_USA_cumulative_monthly,US_ACS,by=c("state",'sex'),all.x=TRUE)

USA_CrudeRate<-NCovid_ACS_USADenoms %>% 
                mutate(Mortality_CrudeRate_USA= covid_deaths_num/pop*100000,
                       Mortality_CrudeRate_USA_cumulative= covid_deaths_cumulative/pop*100000) %>% 
                select(state, month, sex, covid_deaths_num, covid_deaths_cumulative,pop,Mortality_CrudeRate_USA,Mortality_CrudeRate_USA_cumulative)
                                    
save(USA_CrudeRate, file="GenderSci_CovidMortalityProject/Tracker Analysis/Data/USA data/USA_CrudeRate.RData")

USA_RR_RD <- USA_CrudeRate %>% select(month,sex,Mortality_CrudeRate_USA) %>% 
group_by (month) %>% 
  summarize(RateRatio = (Mortality_CrudeRate_USA[sex == "Male"] / Mortality_CrudeRate_USA[sex == "Female"]),
            RateDiff = (Mortality_CrudeRate_USA[sex == "Male"] - Mortality_CrudeRate_USA[sex == "Female"]))
save(USA_RR_RD, file="GenderSci_CovidMortalityProject/Tracker Analysis/Data/USA data/USA_RR_RD.RData")


