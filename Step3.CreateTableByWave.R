library(tidyverse)
library(readxl) 
library(magrittr)
library(googlesheets4)
library(rlist)
library(stringr)
library(tidycensus)
library(lubridate)
library(zoo) # moving averages 

# Double-check you are in the appropriate top working directory (e.g. "Tracker Data") before running this script
# you need to have run all of the Step1a-d code code and placed your datasets in the appropriate directories for this code to work.

# Load wide and long data  (Created by "Step1.CreateTrackerDataset.R", which can be re-run to update)
load("Data/Combined Validated and Unvalidated Tracker Data/tracker.long.RData")
load("Data/Combined Validated and Unvalidated Tracker Data/tracker.wide.RData")

#create the cumulative wave dataset
mergeddata_long$date<-as.Date(mergeddata_long$date)


#This is using wave 1 as Jan-may 2020, wave 2 as june-aug 2021 and wave 3 as sept-march 2021)
# citation that support wave cut-points
  #ending end of may: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7543530/ This study used June 2 as the end of the first wave in NYC
  #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7543530/ This study used June 2 as the end of the first wave in NYC

#assigns a wave to each weekly value, sums the weekly cases, deaths over the wave and then calculates the crude rate, and 95%CI bounds. 

Wave1<-mergeddata_long %>% filter (sex != "total") %>% 
                              filter(date==as.Date("2020-04-27") | date == as.Date("2020-05-25")) %>% 
                              
                              select(state,sex,date,cases, deaths, population) %>% 
                              group_by (state,sex) %>% 
                              summarize(cases_w = (cases[date == as.Date("2020-05-25")] - cases[date == as.Date("2020-04-27")]),
                                        deaths_w = (deaths[date == as.Date("2020-05-25")] - deaths[date == as.Date("2020-04-27")]),
                                        population = population) %>% 
                              mutate (wave = "wave1") %>% 
                              distinct() %>% 
                              ungroup()


Wave2<-mergeddata_long %>% filter (sex != "total") %>% 
                          filter(date==as.Date("2020-06-01") | date == as.Date("2020-08-31")) %>% 
                          
                          select(state,sex,date,cases, deaths, population) %>% 
                          group_by (state,sex) %>% 
                          summarize(cases_w = (cases[date == as.Date("2020-08-31")] - cases[date == as.Date("2020-06-01")]),
                                    deaths_w = (deaths[date == as.Date("2020-08-31")] - deaths[date == as.Date("2020-06-01")]),
                                    population = population) %>% 
                          mutate (wave = "wave2") %>% 
                          distinct() %>% 
                          ungroup()
                                     


Wave3<-mergeddata_long %>% filter (sex != "total") %>% 
                            filter(date==as.Date("2020-09-07") | date == as.Date("2021-05-10")) %>% 
                            
                            select(state,sex,date,cases, deaths, population) %>% 
                            group_by (state,sex) %>% 
                            summarize(cases_w = (cases[date == as.Date("2021-05-10")] - cases[date == as.Date("2020-09-07")]),
                                      deaths_w = (deaths[date == as.Date("2021-05-10")] - deaths[date == as.Date("2020-09-07")]),
                                      population = population) %>% 
                            mutate (wave = "wave3") %>% 
                            distinct() %>% 
                            ungroup()
#Adding in the cumulative total across ALL waves
CumulativeTotal<-mergeddata_long %>% filter (sex != "total") %>% 
                            filter(date == as.Date("2021-05-10")) %>% 
                            select(state,sex,cases, deaths, population) %>% 
                            rename(cases_w=cases, deaths_w=deaths)%>% 
                            mutate (wave = "cumulativetotal")
                                     
                                     
cumulative.waves.crude.rates<-bind_rows(Wave1,Wave2,Wave3,CumulativeTotal) %>% 
  mutate(#denomtime = case_when(
    #wave = "wave1" ~ 104*denom/365.25, #FIX THIS WHEN THE WAVES ARE DETERMINED
    #wave = "wave2" ~  104*denom/365.25, #FIX THIS WHEN THE WAVES ARE DETERMINED
    #wave = "wave3" ~ 104*denom/365.25, #FIX THIS WHEN THE WAVES ARE DETERMINED
    denomtime = population,
    caseRate = cases_w/denomtime,
    deathRate = deaths_w/denomtime,
    varCaseRate = cases_w/denomtime^2,
    varDeathRate = deaths_w/denomtime^2,
    caserate.lo95 = caseRate - 1.96*sqrt(varCaseRate),
    caserate.hi95 = caseRate + 1.96*sqrt(varCaseRate),
    deathrate.lo95 = deathRate - 1.96*sqrt(varDeathRate),
    deathrate.hi95 = deathRate + 1.96*sqrt(varDeathRate)) %>% select(!population) 


#Save as CSV and Rdataset
write.csv(cumulative.waves.crude.rates,'Data/WaveData/cumulative.waves.crude.rates.csv')
save(cumulative.waves.crude.rates,file='Data/WaveData/cumulative.waves.crude.rates.RData')

cumulative.waves.IRR.IRD <- cumulative.waves.crude.rates %>% select (-c("caserate.lo95","caserate.hi95","deathrate.lo95","deathrate.hi95")) %>% 
                                            pivot_wider(names_from = c("sex","wave"), 
                                            values_from = c("cases_w", "deaths_w", "denomtime", "caseRate", "deathRate", "varCaseRate", "varDeathRate"),names_sep = "_") %>% 
                                            mutate(
                                             CaseIRR_W1 = caseRate_men_wave1/caseRate_women_wave1,
                                             CaseIRR_W2 = caseRate_men_wave2/caseRate_women_wave2,
                                             CaseIRR_W3 = caseRate_men_wave3/caseRate_women_wave3,
                                             CaseIRR_CTot = caseRate_men_cumulativetotal/caseRate_women_cumulativetotal,
                                             
                                             CaseIRD_W1 = caseRate_men_wave1-caseRate_women_wave1,
                                             CaseIRD_W2 = caseRate_men_wave2-caseRate_women_wave2,
                                             CaseIRD_W3 = caseRate_men_wave3-caseRate_women_wave3,
                                             CaseIRD_CTot = caseRate_men_cumulativetotal-caseRate_women_cumulativetotal,
                                             
                                             DeathIRR_W1 = deathRate_men_wave1/deathRate_women_wave1,
                                             DeathIRR_W2 = deathRate_men_wave2/deathRate_women_wave2,
                                             DeathIRR_W3 = deathRate_men_wave3/deathRate_women_wave3,
                                             DeathIRR_CTot = deathRate_men_cumulativetotal/deathRate_women_cumulativetotal,
                                             
                                             DeathIRD_W1 = deathRate_men_wave1-deathRate_women_wave1,
                                             DeathIRD_W2 = deathRate_men_wave2-deathRate_women_wave2,
                                             DeathIRD_W3 = deathRate_men_wave3-deathRate_women_wave3,
                                             DeathIRD_CTot = deathRate_men_cumulativetotal-deathRate_women_cumulativetotal,
                                               
                                             varCaselogIRR_W1 = (varCaseRate_men_wave1 /caseRate_men_wave1^2) +(varCaseRate_women_wave1/caseRate_women_wave1^2) ,
                                             varCaselogIRR_W2 = (varCaseRate_men_wave2 /caseRate_men_wave2^2) +(varCaseRate_women_wave2/caseRate_women_wave2^2) ,
                                             varCaselogIRR_W3 = (varCaseRate_men_wave3 /caseRate_men_wave3^2) +(varCaseRate_women_wave3/caseRate_women_wave3^2) ,
                                             varCaselogIRR_CTot = (varCaseRate_men_cumulativetotal/caseRate_men_cumulativetotal^2)+(varCaseRate_women_cumulativetotal/caseRate_women_cumulativetotal^2) ,
                                             
                                             varDeathlogIRR_W1 = (varDeathRate_men_wave1/deathRate_men_wave1^2)+(varDeathRate_women_wave1/deathRate_women_wave1^2) ,
                                             varDeathlogIRR_W2 = (varDeathRate_men_wave2/deathRate_men_wave2^2)+(varDeathRate_women_wave2/deathRate_women_wave2^2) ,
                                             varDeathlogIRR_W3 = (varDeathRate_men_wave3/deathRate_men_wave3^2)+(varDeathRate_women_wave3/deathRate_women_wave3^2) ,
                                             varDeathlogIRR_CTot = (varDeathRate_men_cumulativetotal/deathRate_men_cumulativetotal^2)+(varDeathRate_women_cumulativetotal/deathRate_women_cumulativetotal^2) ,
                                             
                                             
                                             varCaseIRD_W1 = varCaseRate_men_wave1+varCaseRate_women_wave1,
                                             varCaseIRD_W2 = varCaseRate_men_wave2+varCaseRate_women_wave2,
                                             varCaseIRD_W3 = varCaseRate_men_wave3+varCaseRate_women_wave3,
                                             varCaseIRD_CTot = varCaseRate_men_cumulativetotal+varCaseRate_women_cumulativetotal,
                                             
                                             varDeathIRD_W1 = varDeathRate_men_wave1+varDeathRate_women_wave1,
                                             varDeathIRD_W2 = varDeathRate_men_wave2+varDeathRate_women_wave2,
                                             varDeathIRD_W3 = varDeathRate_men_wave3+varDeathRate_women_wave3,
                                             varDeathIRD_CTot = varDeathRate_men_cumulativetotal+varDeathRate_women_cumulativetotal,
                                             
                                             lo95CaseIRD_W1 =    CaseIRD_W1 - 1.96*sqrt(varCaseIRD_W1), 
                                             lo95CaseIRD_W2 =    CaseIRD_W2 - 1.96*sqrt(varCaseIRD_W2) ,
                                             lo95CaseIRD_W3 =    CaseIRD_W3 - 1.96*sqrt(varCaseIRD_W3) ,
                                             lo95CaseIRD_CTot =  CaseIRD_CTot - 1.96*sqrt(varCaseIRD_CTot),
                                             lo95DeathIRD_W1 =  DeathIRD_W1 - 1.96*sqrt(varDeathIRD_W1),
                                             lo95DeathIRD_W2 =  DeathIRD_W2 - 1.96*sqrt(varDeathIRD_W2),
                                             lo95DeathIRD_W3 =  DeathIRD_W3 - 1.96*sqrt(varDeathIRD_W3),
                                             lo95DeathIRD_CTot =  DeathIRD_CTot - 1.96*sqrt(varDeathIRD_CTot),
                                             
                                             up95CaseIRD_W1 =    CaseIRD_W1 + 1.96*sqrt(varCaseIRD_W1) ,
                                             up95CaseIRD_W2 =    CaseIRD_W2 + 1.96*sqrt(varCaseIRD_W2) ,
                                             up95CaseIRD_W3 =    CaseIRD_W3 + 1.96*sqrt(varCaseIRD_W3) ,
                                             up95CaseIRD_CTot =  CaseIRD_CTot + 1.96*sqrt(varCaseIRD_CTot),
                                             up95DeathIRD_W1 =  DeathIRD_W1 + 1.96*sqrt(varDeathIRD_W1),
                                             up95DeathIRD_W2 =  DeathIRD_W2 + 1.96*sqrt(varDeathIRD_W2),
                                             up95DeathIRD_W3 =  DeathIRD_W3 + 1.96*sqrt(varDeathIRD_W3),
                                             up95DeathIRD_CTot =  DeathIRD_CTot + 1.96*sqrt(varDeathIRD_CTot),
                                             
                                             lo95CaseIRR_W1 =  exp(log(CaseIRR_W1) - 1.96*sqrt(varCaselogIRR_W1)) ,
                                             lo95CaseIRR_W2 =  exp(log(CaseIRR_W2) - 1.96*sqrt(varCaselogIRR_W2)) ,
                                             lo95CaseIRR_W3 =  exp(log(CaseIRR_W3) - 1.96*sqrt(varCaselogIRR_W3)) ,
                                             lo95CaseIRR_CTot =  exp(log(CaseIRR_CTot) - 1.96*sqrt(varCaselogIRR_CTot)),
                                             lo95DeathIRR_W1 =  exp(log(DeathIRR_W1) - 1.96*sqrt(varDeathlogIRR_W1)),
                                             lo95DeathIRR_W2 =  exp(log(DeathIRR_W2) - 1.96*sqrt(varDeathlogIRR_W2)),
                                             lo95DeathIRR_W3 =  exp(log(DeathIRR_W3) - 1.96*sqrt(varDeathlogIRR_W3)),      
                                             lo95DeathIRR_CTot =  exp(log(DeathIRR_CTot) - 1.96*sqrt(varDeathlogIRR_CTot)),
                                             
                                             up95CaseIRR_W1 =  exp(log(CaseIRR_W1) + 1.96*sqrt(varCaselogIRR_W1)) ,
                                             up95CaseIRR_W2 =  exp(log(CaseIRR_W2) + 1.96*sqrt(varCaselogIRR_W2)) ,
                                             up95CaseIRR_W3 =  exp(log(CaseIRR_W3) + 1.96*sqrt(varCaselogIRR_W3)) ,
                                             up95CaseIRR_CTot =  exp(log(CaseIRR_CTot) + 1.96*sqrt(varCaselogIRR_CTot)),
                                             up95DeathIRR_W1 =  exp(log(DeathIRR_W1) + 1.96*sqrt(varDeathlogIRR_W1)),
                                             up95DeathIRR_W2 =  exp(log(DeathIRR_W2) + 1.96*sqrt(varDeathlogIRR_W2)),
                                             up95DeathIRR_W3 =  exp(log(DeathIRR_W3) + 1.96*sqrt(varDeathlogIRR_W3)),
                                             up95DeathIRR_CTot =  exp(log(DeathIRR_CTot) + 1.96*sqrt(varDeathlogIRR_CTot)))%>% 
                              select(state,contains("CaseIR"), contains("DeathIR"), contains("lo95"), contains("up95"))

write.csv(cumulative.waves.IRR.IRD,'Data/WaveData/cumulative.waves.IRR.IRD.csv')
save(cumulative.waves.IRR.IRD,file='Data/WaveData/cumulative.waves.IRR.IRD.RData')



##get ranges of the waves###
temp<-cumulative.waves.IRR.IRD %>% dplyr::select(state,CaseIRR_CTot,lo95CaseIRR_CTot,up95CaseIRR_CTot ,CaseIRD_CTot,lo95CaseIRD_CTot,up95CaseIRD_CTot) %>% 
      mutate (RR.CI= str_c(round(CaseIRR_CTot,2)," (",round(lo95CaseIRR_CTot,2),", ",round(up95CaseIRR_CTot,2),")"))
  
temp2<-cumulative.waves.IRR.IRD %>% dplyr::select(state,DeathIRR_CTot,lo95DeathIRR_CTot,up95DeathIRR_CTot ,DeathIRD_CTot,lo95DeathIRD_CTot,up95DeathIRD_CTot) %>% 
  mutate (RR.CI= str_c(round(DeathIRR_CTot,2)," (",round(lo95DeathIRR_CTot,2),", ",round(up95DeathIRR_CTot,2),")"))

tempW1<-cumulative.waves.IRR.IRD %>% dplyr::select(state,DeathIRR_W1,lo95DeathIRR_W1,up95DeathIRR_W1 ,DeathIRD_W1,lo95DeathIRD_W1,up95DeathIRD_W1) %>% 
  mutate (RR.CI= str_c(round(DeathIRR_W1,2)," (",round(lo95DeathIRR_W1,2),", ",round(up95DeathIRR_W1,2),")"))

tempW2<-cumulative.waves.IRR.IRD %>% dplyr::select(state,DeathIRR_W2,lo95DeathIRR_W2,up95DeathIRR_W2 ,DeathIRD_W2,lo95DeathIRD_W2,up95DeathIRD_W2) %>% 
  mutate (RR.CI= str_c(round(DeathIRR_W2,2)," (",round(lo95DeathIRR_W2,2),", ",round(up95DeathIRR_W2,2),")"))

tempW3<-cumulative.waves.IRR.IRD %>% dplyr::select(state,DeathIRR_W3,lo95DeathIRR_W3,up95DeathIRR_W3 ,DeathIRD_W3,lo95DeathIRD_W3,up95DeathIRD_W3) %>% 
  mutate (RR.CI= str_c(round(DeathIRR_W3,2)," (",round(lo95DeathIRR_W3,2),", ",round(up95DeathIRR_W3,2),")"))

