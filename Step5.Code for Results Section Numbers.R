library(tidyverse)

# Set your working directory to "Tracker Analysis"

# Load wide and long data
load("Data/Combined Validated and Unvalidated Tracker Data/tracker.long.RData")
load("Data/Combined Validated and Unvalidated Tracker Data/tracker.wide.RData")

# Load wave data
load("Data/WaveData/cumulative.waves.IRR.IRD.RData")

### result section numbers
#how many states have data for Case and Death IRR in first and last week
RR_start<-mergeddata_wide %>% filter(date=="2020-04-27")
RR_end<-mergeddata_wide %>% filter(date=="2021-05-10")

#total numbers in the last week of data collection (5/10/2021) 
Mstart<-mergeddata_long %>% filter(date=="2020-04-27"& sex=="men")
Mend<-mergeddata_long %>% filter(date=="2021-05-10"& sex=="men")
sum(Mend$cases,na.rm=TRUE) #total cases or men cumulatively on 5/10/2021
sum(Mend$deaths,na.rm=TRUE) #total deaths or men cumulatively on 5/10/2021

Wstart<-mergeddata_long %>% filter(date=="2020-04-27"& sex=="women")
Wend<-mergeddata_long %>% filter(date=="2021-05-10"& sex=="women")
sum(Wend$cases,na.rm=TRUE)#total cases for women cumulatively on 5/10/2021
sum(Wend$deaths,na.rm=TRUE) #total deaths or women cumulatively on 5/10/2021


# weekly cases:
# num states where RR for CASES greater than 1 in first week and last week
sum(RR_start$cases_oneweek_rate_IRR>=1,na.rm=TRUE)
sum(RR_end$cases_oneweek_rate_IRR>=1,na.rm=TRUE)

sum(RR_start$cases_rate_IRR>=1,na.rm=TRUE)
sum(RR_end$cases_rate_IRR>=1,na.rm=TRUE)

# num states where RR for DEATHS greater than 1 in first week and last week 
sum(RR_start$deaths_oneweek_rate_IRR<=1,na.rm=TRUE) #to say "greater than one in all BUT X number of states)
sum(RR_end$deaths_oneweek_rate_IRR<=1,na.rm=TRUE)

sum(RR_start$deaths_rate_IRR<=1,na.rm=TRUE)
sum(RR_end$deaths_rate_IRR<=1,na.rm=TRUE)


##get ranges of the waves###
##Cumulative total
temp<-cumulative.waves.IRR.IRD %>% dplyr::select(state,CaseIRR_CTot,lo95CaseIRR_CTot,up95CaseIRR_CTot ,CaseIRD_CTot,lo95CaseIRD_CTot,up95CaseIRD_CTot) %>% 
  mutate (RR.CI= str_c(round(CaseIRR_CTot,2)," (",round(lo95CaseIRR_CTot,2),", ",round(up95CaseIRR_CTot,2),")"))

temp2<-cumulative.waves.IRR.IRD %>% dplyr::select(state,DeathIRR_CTot,lo95DeathIRR_CTot,up95DeathIRR_CTot ,DeathIRD_CTot,lo95DeathIRD_CTot,up95DeathIRD_CTot) %>% 
  mutate (RR.CI= str_c(round(DeathIRR_CTot,2)," (",round(lo95DeathIRR_CTot,2),", ",round(up95DeathIRR_CTot,2),")"))


## Each wave:
tempW1<-cumulative.waves.IRR.IRD %>% dplyr::select(state,DeathIRR_W1,lo95DeathIRR_W1,up95DeathIRR_W1 ,DeathIRD_W1,lo95DeathIRD_W1,up95DeathIRD_W1) %>% 
  mutate (RR.CI= str_c(round(DeathIRR_W1,2)," (",round(lo95DeathIRR_W1,2),", ",round(up95DeathIRR_W1,2),")"))

tempW2<-cumulative.waves.IRR.IRD %>% dplyr::select(state,DeathIRR_W2,lo95DeathIRR_W2,up95DeathIRR_W2 ,DeathIRD_W2,lo95DeathIRD_W2,up95DeathIRD_W2) %>% 
  mutate (RR.CI= str_c(round(DeathIRR_W2,2)," (",round(lo95DeathIRR_W2,2),", ",round(up95DeathIRR_W2,2),")"))

tempW3<-cumulative.waves.IRR.IRD %>% dplyr::select(state,DeathIRR_W3,lo95DeathIRR_W3,up95DeathIRR_W3 ,DeathIRD_W3,lo95DeathIRD_W3,up95DeathIRD_W3) %>% 
  mutate (RR.CI= str_c(round(DeathIRR_W3,2)," (",round(lo95DeathIRR_W3,2),", ",round(up95DeathIRR_W3,2),")"))
