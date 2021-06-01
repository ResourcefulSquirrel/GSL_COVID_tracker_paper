

## THIS CODE CREATES A DATASET THAT HAS THE US RATES FOR MEN AND WOMEN IN THE US BY MONTH

rm(list=ls())
library(tidycensus)
library("writexl")
library(tidyr)
library(tidyverse)
library(tigris)
library(sp)
library(stringr)
library(haven)
library(reshape2)
library(rstudioapi)

#  IF USING MY PC
setwd("C:/Users/tar344/Box/")

# IF USING MY MAC
# setwd("/Users/tamararushovich/Box/")


### CREATE USA DATASET (JUST SEX/GENDER)

#### PULL IN THE CDC DATA FROM THE API: https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Sex-Age-and-S/9bhg-hcku/data####
library(RSocrata)
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

##### Import and formate cumulative counts

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
census_api_key("026c1ea35d1fbc9966cd516746860acbf07bead8")
#v12 <- load_variables(2019, "acs5")
US_ACS<- get_acs(geography = "us",
                    variables = c("B01001_002","B01001_026"),year=2019) %>% #total for male, total for female in the USA
                    mutate(sex = case_when(variable == "B01001_002"~"Male",
                                           variable =="B01001_026" ~ "Female")) %>% 
                    rename (state = NAME, pop=estimate) %>% select(state, sex, pop)

##MERGE US DEATHS AND POPS TOGETHER
NCovid_ACS_USADenoms<-merge(CDC_USA_cumulative_monthly,US_ACS,by=c("state",'sex'),all.x=TRUE)

### CALCULATE POP RATES#####
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



#Calculate at 2015-2019 mortality rates
# CDC_age_sex_excess<-read.socrata("https://data.cdc.gov/resource/m74n-4hbs.json") 
# 
# USAMortalityRate_1519<-CDC_age_sex_excess%>% 
#                     filter(time_period =="2015-2019" & raceethnicity=="All Race/Ethnicity Groups"& agegroup=="All Ages" & 
#                             sex!="All Sexes") %>% 
#                       mutate(ndeaths=as.numeric(deaths_unweighted)) %>% 
#                       select(mmwryear,mmwrweek,sex,ndeaths) %>% 
#                       group_by(mmwrweek,sex) %>%
#                       dplyr::summarise(five_yr_avg_deaths=sum(ndeaths)/5) %>% 
#                       ungroup() %>% 
#                       mutate(timepd="2015-2019", sex = case_when(sex=="Female (F)"~"Female", 
#                                                                  sex=="Male (M)"~"Male")) %>% 
#   
#                       merge(US_ACS,by=c('sex'),all.x=TRUE)%>% 
#                       mutate(AverageDeathRate_1519= five_yr_avg_deaths/pop*100000,
#                              weekdate = as.Date(paste(2020, mmwrweek, 1, sep="-"), "%Y-%U-%u"))
# 
# save(USAMortalityRate_1519, file="/Users/tamararushovich/Box/GenderSci_CovidMortalityProject/Tracker Analysis/Data/USA data/USAMortalityRate_1519.RData")
# 

# 
# #################### CREATE THE USA DATASET WITH AGE AND STATE BY MONTH
# 
# ### AGE ADJUSTING USING THE 2000 US POP standard and the DIRECT age adjustment method####
# 
# rm(list=ls())
# library(tidycensus)
# library("writexl")
# library(tidyr)
# library(tidyverse)
# library(tigris)
# library(sp)
# library(stringr)
# library(haven)
# library(reshape2)
# library(rstudioapi)
# 
# #  IF USING MY PC
# setwd("C:/Users/tar344/Box/")
# 
# # IF USING MY MAC
# # setwd("/Users/tamararushovich/Box/")
# 
# 
# #### PULL IN THE CDC DATA FROM THE API: https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Sex-Age-and-S/9bhg-hcku/data####
# library(RSocrata)
# CDC_State_Deaths <- read.socrata("https://data.cdc.gov/resource/9bhg-hcku.json")
# 
# ##ReAlign CDC age groups (combine 'under one year' and 1-4)
# CDC_State_Deaths <- mutate(CDC_State_Deaths, agecat = case_when(
#   age_group == "Under 1 year" ~ "0-4 years",
#   age_group == "1-4 years" ~ "0-4 years",
#   age_group == "5-14 years" ~ "5-14 years",
#   age_group == "15-24 years" ~ "15-24 years",
#   age_group == "25-34 years" ~ "25-34 years",
#   age_group == "35-44 years" ~ "35-44 years",
#   age_group == "45-54 years" ~ "45-54 years",
#   age_group == "55-64 years" ~ "55-64 years",
#   age_group == "65-74 years" ~ "65-74 years",
#   age_group == "75-84 years" ~ "75-84 years",
#   age_group == "85 years and over" ~ "85 years and over",
#   age_group == "All Ages" ~ "All Ages",
#   age_group == "Female, all ages" ~ "All Ages", 
#   age_group == "Male, all ages" ~ "All Ages",
#   age_group == "All ages" ~ "All Ages",
#   TRUE ~ NA_character_))
# 
# 
# # remove totals and unknown  
# CDC_State_Deaths <- CDC_State_Deaths %>% dplyr::select(start_date:covid_19_deaths,agecat) %>%
#                                          filter(is.na(agecat)==FALSE) %>% 
#                                          mutate(state2=ifelse(state=="New York City","New York",state)) #add NYC+NY State
# 
# #make the NA to be zero  (NA MEANS CDC SUPPRESSED DATA WHEN IT WAS 1-9 deaths)
# CDC_State_Deaths$covid_19_deaths[which(is.na(CDC_State_Deaths$covid_19_deaths))]<-0
# 
# CDC_State_Deaths<-CDC_State_Deaths %>% group_by(start_date,end_date,group,state2,sex,agecat) %>% 
#                                             summarize(ncoviddeaths=sum(as.numeric(covid_19_deaths))) %>% 
#                                        rename(state=state2)
# 
# 
# 
# ###Get State Population Denominators from 2019 5-Year ACS for age and sex###
# 
# census_api_key("026c1ea35d1fbc9966cd516746860acbf07bead8")
# 
# data(fips_codes)
# v12 <- load_variables(2019, "acs5") %>% separate(label, c("type", "group","sex","age_group"), "!!") %>% 
#                                         filter(stringr::str_detect(name, 'B01001_') ) %>% 
#                                         mutate(sex=case_when(sex=="Male:"~"Male",
#                                                              sex=="Female:"~"Female",
#                                                              is.na(sex) ~ "All Sexes"),
#                                              agecat=case_when( age_group == "Under 5 years" ~ "0-4 years",
#                                                                  age_group == "5 to 9 years" ~ "5-14 years",
#                                                                  age_group == "10 to 14 years" ~ "5-14 years",
#                                                                  age_group == "15 to 17 years" ~ "15-24 years",
#                                                                  age_group == "18 and 19 years" ~ "15-24 years",
#                                                                  age_group == "20 years" ~ "15-24 years",
#                                                                  age_group == "21 years" ~ "15-24 years",
#                                                                  age_group == "22 to 24 years" ~ "15-24 years",
#                                                                  age_group == "25 to 29 years" ~ "25-34 years",
#                                                                  age_group == "30 to 34 years" ~ "25-34 years",
#                                                                  age_group == "35 to 39 years" ~ "35-44 years",
#                                                                  age_group == "40 to 44 years" ~ "35-44 years",
#                                                                  age_group == "45 to 49 years" ~ "45-54 years",
#                                                                  age_group == "50 to 54 years" ~ "45-54 years",
#                                                                  age_group == "55 to 59 years" ~ "55-64 years",
#                                                                  age_group == "60 and 61 years" ~ "55-64 years",
#                                                                  age_group == "62 to 64 years" ~ "55-64 years",
#                                                                  age_group == "65 and 66 years" ~ "65-74 years",
#                                                                  age_group == "67 to 69 years" ~ "65-74 years",
#                                                                  age_group == "70 to 74 years" ~ "65-74 years",
#                                                                  age_group == "75 to 79 years" ~ "75-84 years",
#                                                                  age_group == "80 to 84 years" ~ "75-84 years",
#                                                                  age_group == "85 years and over" ~ "85 years and over",
#                                                                  is.na(age_group) ~ "All Ages")) %>% 
#                                                 rename(variable=name) %>% 
#                                                 select(variable,agecat,sex)
# 
# State_ACS<- get_acs(geography = "state",
#                     variables = c(## total pop
#                       paste0('B01001_',str_pad(as.character(c(1:49)),width=3,side='left',pad='0'))
#                     ),year=2019) %>% 
#             merge(v12,by="variable") %>% select(NAME, estimate, agecat, sex) %>% rename(state=NAME) %>% 
#             group_by(state,sex,agecat) %>% summarize(pop=sum(estimate))
# 
# USA_ACS<- get_acs(geography = "us",
#                     variables = c(## total pop
#                       paste0('B01001_',str_pad(as.character(c(1:49)),width=3,side='left',pad='0'))
#                     ),year=2019) %>% 
#   merge(v12,by="variable") %>% select(NAME, estimate, agecat, sex) %>% rename(state=NAME) %>% 
#   group_by(state,sex,agecat) %>% summarize(pop=sum(estimate))
# 
# Denoms<-rbind(State_ACS,USA_ACS) %>% 
#                       pivot_wider(names_from=sex,values_from=pop) %>%
#                       mutate (`All Sexes` = Female+Male) %>% 
#                       pivot_longer(cols=c("All Sexes","Female", "Male"), names_to="sex", values_to = "pop")
# 
# 
# ##MERGE DEATHS AND POPS TOGETHER
# NCovid_ACSStateDenoms<-merge(CDC_State_Deaths,Denoms,by=c('state','sex','agecat'),all.x=TRUE)
# #make the NA to be zero
# NCovid_ACSStateDenoms$ncoviddeaths[which(is.na(NCovid_ACSStateDenoms$ncoviddeaths))]<-0
# 
# CDC_Covid_Data_State_with_ACSDenoms_long<-NCovid_ACSStateDenoms 
# 
# CDC_Covid_Data_State_with_ACSDenoms_wide<-CDC_Covid_Data_State_with_ACSDenoms_long %>% 
#                                           pivot_wider(names_from = sex, values_from = c("pop", "ncoviddeaths"))
# 
# 
# #MAKE THE WIDE BACK LONG TO USE THE TOTAL POP! 
# 
# 
# #SAVE Final Dataset
# save(CDC_Covid_Data_State_with_ACSDenoms_long, file="GenderSci_CovidMortalityProject/Tracker Analysis/Data/USA data/CDC_Covid_Data_State_with_ACSDenoms_sex_age_long.RData") 
# save(CDC_Covid_Data_State_with_ACSDenoms_wide, file="GenderSci_CovidMortalityProject/Tracker Analysis/Data/USA data/CDC_Covid_Data_State_with_ACSDenoms_sex_age_wide.RData") 


























#### ARCHIVED/OLD STUFF####



# ## MAKE PLOTS
# ggplot(USA_CrudeRate,aes(x=month,y=Mortality_CrudeRate_USA,group=sex,color=sex))+
#         geom_line()+ theme(legend.position = "bottom")+ scale_color_hue(direction = -1)
# ggsave("/Users/tamararushovich/Box/GenderSci_CovidMortalityProject/Tracker Analysis/figures/USAplots/USA_Mortality_CrudeRate.png", width = 30, height = 20, units = "cm")
# 
# ggplot(USA_RR_RD,aes(x=month,y=RateRatio))+
#   geom_line()+ theme(legend.position = "bottom")+
#   geom_hline(yintercept=1, linetype="dashed", color = "blue")
# ggsave("/Users/tamararushovich/Box/GenderSci_CovidMortalityProject/Tracker Analysis/figures/USAplots/USA_Mortality_RR.png", width = 30, height = 20, units = "cm")
# 
# ggplot(USA_RR_RD,aes(x=month,y=RateDiff))+
#   geom_line()+ theme(legend.position = "bottom")+
#   geom_hline(yintercept=1, linetype="dashed", color = "purple")
# ggsave("/Users/tamararushovich/Box/GenderSci_CovidMortalityProject/Tracker Analysis/figures/USAplots/USA_Mortality_RD.png", width = 30, height = 20, units = "cm")
# 








############################ if you want to do by age as well############################ 
##ReAlign CDC Deaths (combine 'under one year' and 1-4)
# CDC_Deaths_a <- mutate(CDC_Deaths,  agecat = case_when(
#                                             age_group == "Under 1 year" ~ "0-4 years",
#                                             age_group == "1-4 years" ~ "0-4 years",
#                                             age_group == "5-14 years" ~ "5-14 years",
#                                             age_group == "15-24 years" ~ "15-24 years",
#                                             age_group == "25-34 years" ~ "25-34 years",
#                                             age_group == "35-44 years" ~ "35-44 years",
#                                             age_group == "45-54 years" ~ "45-54 years",
#                                             age_group == "55-64 years" ~ "55-64 years",
#                                             age_group == "65-74 years" ~ "65-74 years",
#                                             age_group == "75-84 years" ~ "75-84 years",
#                                             age_group == "85 years and over" ~ "85 years and over",
#                                             age_group == "All Ages" ~ "All Ages",
#                                             age_group == "Female, all ages" ~ "All Ages", 
#                                             age_group == "Male, all ages" ~ "All Ages",
#                                             age_group == "All ages" ~ "All Ages",
#                                             TRUE ~ NA_character_),
#                                   wave = case_when(
#                                             start_date == "2020-01-01" ~ "Wave1",
#                                             start_date == "2020-02-01" ~ "Wave1",
#                                             start_date == "2020-03-01" ~ "Wave1",
#                                             start_date == "2020-04-01" ~ "Wave2",
#                                             start_date == "2020-05-01" ~ "Wave2",
#                                             start_date == "2020-06-01" ~ "Wave2",
#                                             start_date == "2020-07-01" ~ "Wave2",
#                                             start_date == "2020-08-01" ~ "Wave2",
#                                             start_date == "2020-09-01" ~ "Wave2",
#                                             start_date == "2020-10-01" ~ "Wave3",
#                                             start_date == "2020-11-01" ~ "Wave3",
#                                             start_date == "2020-12-01" ~ "Wave3",
#                                             start_date == "2021-01-01" ~ "Wave3",
#                                             start_date == "2021-02-01" ~ "Wave3",
#                                             start_date == "2021-03-01" ~ "Wave3"),
#                                     covid_deaths_num=as.numeric(covid_19_deaths)) %>% 
#   filter(group=="By Month" & sex!="All Sexes" & !is.na(agecat) & state=="United States") %>% 
#   select(state,sex,agecat,group,start_date,covid_deaths_num)
# ## vnum is a variable that tells us the sex and age group the row represents ##
# US_ACS$vnum<-as.numeric(substr(US_ACS$variable,nchar(US_ACS$variable)-2,nchar(US_ACS$variable)))
# 
# US_ACSa<-US_ACS %>% mutate(agecat = case_when(
#                                       vnum == 2 ~ 'All Ages',
#                                       vnum == 3 ~ '0-4 years',
#                                       vnum == 4 ~ '5-14 years',
#                                       vnum == 5 ~ '5-14 years',
#                                       vnum == 6 ~ '15-24 years',
#                                       vnum == 7 ~ '15-24 years',
#                                       vnum == 8 ~ '15-24 years',
#                                       vnum == 9 ~ '15-24 years',
#                                       vnum == 10 ~ '15-24 years',
#                                       vnum == 11 ~ '25-34 years',
#                                       vnum == 12 ~ '25-34 years',
#                                       vnum == 13 ~ '35-44 years',
#                                       vnum == 14 ~ '35-44 years',
#                                       vnum == 15 ~ '45-54 years',
#                                       vnum == 16 ~ '45-54 years',
#                                       vnum == 17 ~ '55-64 years',
#                                       vnum == 18 ~ '55-64 years',
#                                       vnum == 19~ '55-64 years',
#                                       vnum == 20 ~ '65-74 years',
#                                       vnum == 21 ~ '65-74 years',
#                                       vnum == 22 ~ '65-74 years',
#                                       vnum == 23 ~ '75-84 years',
#                                       vnum == 24 ~ '75-84 years',
#                                       vnum == 25 ~ '85 years and over',
#                                       vnum == 26 ~ 'All Ages',
#                                       vnum == 27 ~ '0-4 years',
#                                       vnum == 28 ~ '5-14 years',
#                                       vnum == 29 ~ '5-14 years',
#                                       vnum == 30 ~ '15-24 years',
#                                       vnum == 31 ~ '15-24 years',
#                                       vnum == 32 ~ '15-24 years',
#                                       vnum == 33 ~ '15-24 years',
#                                       vnum == 34 ~ '15-24 years',
#                                       vnum == 35 ~ '25-34 years',
#                                       vnum == 36 ~ '25-34 years',
#                                       vnum == 37 ~ '35-44 years',
#                                       vnum == 38 ~ '35-44 years',
#                                       vnum == 39 ~ '45-54 years',
#                                       vnum == 40 ~ '45-54 years',
#                                       vnum == 41 ~ '55-64 years',
#                                       vnum == 42 ~ '55-64 years',
#                                       vnum == 43 ~ '55-64 years',
#                                       vnum == 44 ~ '65-74 years',
#                                       vnum == 45 ~ '65-74 years',
#                                       vnum == 46 ~ '65-74 years',
#                                       vnum == 47 ~ '75-84 years',
#                                       vnum == 48 ~ '75-84 years',
#                                       vnum == 49 ~ '85 years and over'),
#                            sex=case_when(vnum<26~ "Male",TRUE~"Female"))%>%
#             select(NAME, estimate, sex, agecat) %>% 
#             group_by(NAME,agecat) %>% summarize(pop=sum(estimate)) %>% mutate(sex="All Sexes")


# ############################ STOPPED HERE #############################################################
# 
# 
# #########################################################################################
# #RATES FOR STATES
# 
# 
# ###Get USA Population Demoninators from 2018 5-Year ACS###
# 
# 
# STATE_ACS<- get_acs(geography='state',
#                    variables = c(## total pop
#                      paste0('B01001_',str_pad(as.character(c(2:49)),width=3,side='left',pad='0'))
#                     ),year=2018)
# 
# ## organize (some of the steps are not totally necessary, but I was adapting anothe code and just left them in##
# STATE_ACS<-as.data.frame(STATE_ACS)
# 
# ## racevar is a variable that tells us what racegroup the row represents (not actually needed for this analysis as we're just doing gender)##
# STATE_ACS$racevar<-substr(STATE_ACS$variable, 1, 7)
# ## vnum is a variable that tells us the sex and age group the row represents ##
# STATE_ACS$vnum<-as.numeric(substr(STATE_ACS$variable,nchar(STATE_ACS$variable)-2,nchar(STATE_ACS$variable)))
# 
# ## create a dataset with proper race and sex variables and an age group variable with consistent groupings (called rxsxa) based on the racevar and vnum variables ##
# ## age ranges ##
# agecat<-c('All Ages','0-4 years','5-14 years','15-24 years','25-34 years','35-44 years','45-54 years','55-64 years','65-74 years','75-84 years','85 years and over')
# #this is code that realigns the age groups
# rxsxa<-STATE_ACS[!duplicated(STATE_ACS[,c('racevar','vnum')]),c('racevar','vnum')]
# rxsxa<-rxsxa[order(rxsxa$racevar,rxsxa$vnum),]
# 
# realign_fineage<-c(1,2,3,3,4,4,4,4,4,5,5,6,6,7,7,8,8,8,9,9,9,10,10,11)
# rxsxa$age<-c(rep(realign_fineage,2),rep(1:length(agecat),2*(length(unique(rxsxa$racevar))-1)))
# rxsxa$sex<-c(rep(c('m','f'),each=length(realign_fineage)),rep(rep(c('m','f'),each=length(agecat)),length(unique(rxsxa$racevar))-1))
# 
# rxsxa<-merge(rxsxa,data.frame('age'=1:length(agecat),'agecat'=agecat),by='age')
# rxsxa<-merge(rxsxa,data.frame(
#   'race'=c('total','white','black','am_indian','asian','pac_islander','hispanic'),
#   'racevar'=paste0('B01001',c('_','H','B','C','D','E','I'))),by='racevar')
# 
# ## merge this new dataset into CHI_ACS to add proper race, age, sex variables ##
# STATE_ACS<-merge(STATE_ACS,rxsxa,by=c('racevar','vnum'))
# 
# 
# STATE_ACS <- mutate(STATE_ACS, sex = case_when(
#   sex == "m" ~ "Male",
#   sex == "f" ~ "Female",
#   TRUE ~ NA_character_))
# 
# STATE_ACS <- STATE_ACS%>%
#   select(NAME, estimate, sex, agecat)
# 
# colnames(STATE_ACS)[1] <- "state"
# colnames(STATE_ACS)[2] <- "state_ACS_pop"
# 
# st2<-aggregate(state_ACS_pop~state+sex+agecat,data=STATE_ACS,
#               FUN=sum,na.rm=T)
# st2 <-subset(st2, agecat != "All Ages")
# 
# 
# ##MERGE STATE DEATHS AND POPS TOGETHER
# NCovid_ACS_STATEDenoms<-merge(st2,StateDeaths,by=c('state','sex','agecat'),all.x=TRUE)
# #make the NA to be zero
# NCovid_ACS_STATEDenoms$n_state_coviddeaths[which(is.na(NCovid_ACS_STATEDenoms$n_state_coviddeaths))]<-0
# 
# ### CALCULATE POP RATES#####
# NCovid_ACS_STATEDenoms$STATE_crude_rate<-NCovid_ACS_STATEDenoms$n_state_coviddeaths/NCovid_ACS_STATEDenoms$state_ACS_pop
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###Create dataset with the 2000 standard population
# #agecat<-c('0-4 years','5-14 years','15-24 years','25-34 years','35-44 years','45-54 years','55-64 years','65-74 years','75-84 years','85 years and over')
# #pop2000<-c(18987000,39977000,38077000,37233000,44659000,37030000,23961000,18136000,12315000,4259000)
# #Weight2000<-c(0.069135,0.145565,0.138646,0.135573,0.162613,0.134834,0.082747,0.066037,0.044842,0.015508)
# #stddata <- data.frame(agecat, pop2000, Weight2000)
# 
# 
# ##### CALCULATE THE AGE ADJUSTED RATE
# # USA Rates dataset: NCovid_ACS_USADenoms
# # STATE DEATHS dataset: NCovid_ACS_STATEDenoms
# 
# ## Function to do one state for male and female
#   #following this method: https://www.dartmouthatlas.org/wp-content/uploads/indirect_adjustment.pdf and https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3406211/#:~:text=In%20indirect%20age%2Dadjustment%2C%20a,(SMR)%20(5).
# 
#  #confidence interval reference: https://www.cdc.gov/nchs/data/statnt/statnt06rv.pdf
# State_sex_age_adjust_Rate<- function(State_name) {
#   
#   #MALE
#   State_Sex <-subset(NCovid_ACS_STATEDenoms, state == "New York" & sex == "Male")
#   USA_Sex<-subset(NCovid_ACS_USADenoms, sex=="Male")
#   State_Sex2<-merge(State_Sex,USA_Sex,by=c('agecat'))
#   
#   State_Sex3 <- mutate(State_Sex2, expecteddeaths = state_ACS_pop*USA_crude_rate)
#   State_Sex3 <- mutate(State_Sex3, state_var_m = ((USA_crude_rate*(1-USA_crude_rate))/state_ACS_pop))
#   State_Sex3 <- mutate(State_Sex3, state_weight_m = (state_ACS_pop/sum(State_Sex3$state_ACS_pop)))
#   State_Sex3 <- mutate(State_Sex3, state_weight2_var_m = (state_var_m*(state_weight_m^2)))
#   
#   Standard_Error_M<- sqrt(sum(State_Sex3$state_weight2_var_m))*100000
#   
#   
#   
#   TOT_expected_Deaths_State_M<-sum(State_Sex3$expecteddeaths)
#   TOT_observed_Deaths_State_M<-sum(State_Sex3$n_state_coviddeaths)
#   TOT_US_DEATHS_M<-sum(State_Sex3$n_USA_coviddeaths)
#   TOT_US_POP_M<-sum(State_Sex3$USA_ACS_pop)
#   USA_CRUDE_RATE_M<-TOT_US_DEATHS_M/TOT_US_POP_M
#   SMR_M<-TOT_observed_Deaths_State_M/TOT_expected_Deaths_State_M
#   AMR_M<-(SMR_M*USA_CRUDE_RATE_M)*100000  ##Adjusted Mortality Ratio for M in 
#   
#   lower_CI_M<-(AMR_M-(1.96*Standard_Error_M))
#   upper_CI_M<-(AMR_M+(1.96*Standard_Error_M))
#   
#   #next multip SMR by overal mort ratio
#   
#   #FEMALE
#   State_Sex <-subset(NCovid_ACS_STATEDenoms, state == State_name & sex == "Female")
#   USA_Sex<-subset(NCovid_ACS_USADenoms, sex=="Female")
#   State_Sex2<-merge(State_Sex,USA_Sex,by=c('agecat'))
#   
#   State_Sex3 <- mutate(State_Sex2, expecteddeaths = state_ACS_pop*USA_crude_rate)
#   State_Sex3 <- mutate(State_Sex3, state_var_f = ((USA_crude_rate*(1-USA_crude_rate))/state_ACS_pop))
#   State_Sex3 <- mutate(State_Sex3, state_weight_f = (state_ACS_pop/sum(State_Sex3$state_ACS_pop)))
#   State_Sex3 <- mutate(State_Sex3, state_weight2_var_f = (state_var_f*(state_weight_f^2)))
#   
#   Standard_Error_F<- sqrt(sum(State_Sex3$state_weight2_var_f))*100000
#   
#   TOT_expected_Deaths_State_F<-sum(State_Sex3$expecteddeaths)
#   TOT_observed_Deaths_State_F<-sum(State_Sex3$n_state_coviddeaths)
#   TOT_US_DEATHS_F<-sum(State_Sex3$n_USA_coviddeaths)
#   TOT_US_POP_F<-sum(State_Sex3$USA_ACS_pop)
#   USA_CRUDE_RATE_F<-TOT_US_DEATHS_F/TOT_US_POP_F
#   SMR_F<-TOT_observed_Deaths_State_F/TOT_expected_Deaths_State_F
#   AMR_F<-(SMR_F*USA_CRUDE_RATE_F)*100000
#   
#   lower_CI_F<-(AMR_F-(1.96*Standard_Error_F))
#   upper_CI_F<-(AMR_F+(1.96*Standard_Error_F))
#   
#   
#   one<-c(State_name)
#   two<-(AMR_M)
#   three<-c(AMR_F)
#   
#   
#   State_name<-data.frame(one, TOT_observed_Deaths_State_M,two,lower_CI_M, upper_CI_M, TOT_observed_Deaths_State_F,three,lower_CI_F,upper_CI_F)
#   colnames(State_name) <- c("State", "N_COVID_DEATHS_M","Age_adjusted_rate_Male","lower_CI_M","upper_CI_M", "N_COVID_DEATHS_F", "Age_Adjusted_Rate_Female","lower_CI_F","upper_CI_F")
#   
#   return(data.frame(State_name))
# }
# 
# #example for just one state:
# State_sex_age_adjust_Rate("Arizona")
# 
# #create list of states
# States<-aggregate(state_ACS_pop~state,data=STATE_ACS,
#                   FUN=sum,na.rm=T)
# State_names<-data.frame(States$state)
# 
# ### Run the list of states through the function (to do all states at once)
# full<-data.frame(apply(State_names,MARGIN=1,FUN=State_sex_age_adjust_Rate))
# 
# #full <- subset(full, select = -collapse)
# 
# ## reformat the dataset so it is long not wide
# 
# full2<-as.data.frame(matrix(unlist(full),ncol=9, byrow=TRUE)) ### WHY ARE THE STATE NAMES GETTING DROPPED IN THE LAST STEP ???
# 
# full12 <- data.frame(State_names, full2$V2, full2$V3,full2$V4,full2$V5,full2$V6,full2$V7, full2$V8, full2$V9)
# colnames(full12) <- c("State", "N_COVID_DEATHS_M","Age_adjusted_rate_Male","lower_CI_M","upper_CI_M", "N_COVID_DEATHS_F", "Age_Adjusted_Rate_Female","lower_CI_F","upper_CI_F")
# 
# write_xlsx(full12, "INDIRECT_Age_adjusted_mort_rates_6.8.2020.xlsx")




