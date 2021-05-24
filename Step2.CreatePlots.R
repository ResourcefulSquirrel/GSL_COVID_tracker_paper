library(tidyverse)
library(tidycensus)
library(lubridate)
library(zoo) # moving averages 

# Set your working directory to "Tracker Analysis"

# Load wide and long data
load("Data/Combined Validated and Unvalidated Tracker Data/tracker.long.RData")
load("Data/Combined Validated and Unvalidated Tracker Data/tracker.wide.RData")

# Load wave data
load("Data/WaveData/cumulative.waves.IRR.IRD.RData")

# Load US data
load("Data/USA data/USA_CrudeRate.RData")
load("Data/USA data/USA_RR_RD.RData")
load("Data/USA data/USAMortalityRate_1519.RData")


############
# Format data

#format USA data so it can be mapped with state data
USA_formatted <- USA_CrudeRate %>% rename(date=month, 
                                          deaths_oneweek = covid_deaths_num, 
                                          deaths_oneweek_rate_3wkrolling = Mortality_CrudeRate_USA, 
                                          population = pop) %>% 
                                    mutate(sex=case_when(sex=="Male" ~ "men", sex=="Female"~"women"))

USA_RR_RD_formatted <- USA_RR_RD %>% rename(date=month, 
                                            deaths_oneweek_rate_IRD = RateDiff, 
                                            deaths_oneweek_rate_IRR = RateRatio) %>% 
                                     mutate(state="United States")

USAMortalityRate_1519 <- USAMortalityRate_1519 %>% 
                         rename(date=weekdate) %>% 
                         mutate(sex=case_when(sex=="Male" ~ "men",
                                              sex=="Female"~"women"))

### ggplot of rates and counts by week

temp<-bind_rows(mergeddata_long,USA_formatted) %>% filter(sex!="total") %>% filter(state != "Puerto Rico") %>% filter(state != "US Virgin Islands")

### Reorder the state factor so U.S. is first
temp$state <- fct_relevel(as.factor(temp$state), "United States")

##############
## Figure 1: Cases (weekly and cumulative)

#cases weekly
cases_weekly <- ggplot(temp,aes(x=as.Date(date),y=cases_oneweek_rate_3wkrolling,group=sex,color=sex))+
  geom_line()+facet_wrap(~state)+ 
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")+
  theme(axis.text.x=element_text(angle=60, hjust=1), 
        strip.text.x = element_text(size = 8, margin = margin(1,1,1,1, unit = "pt")), 
        panel.spacing = unit(0, 'pt'))+
  labs(x = "Month", y = "Three Week Rolling Average Case Rate per 100,000", subtitle = "Weekly")


#Cases cumulative
cases_cumulative <- ggplot(temp,aes(x=as.Date(date),y=cases_rate,group=sex,color=sex))+
  geom_line()+facet_wrap(~state)+ 
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")+
  theme(axis.text.x=element_text(angle=60, hjust=1), 
        strip.text.x = element_text(size = 8, margin = margin(1,1,1,1, unit = "pt")),
        panel.spacing = unit(0, 'pt'))+
  labs(x = "Month", y = "Three Week Rolling Average Case Rate per 100,000", subtitle = "Cumulative")

cases_combined <- cases_weekly / cases_cumulative + plot_annotation(tag_levels = 'a') + plot_layout(guides = 'collect')

ggsave(x=cases_combined, filename = "figures/Combined_Val_Unval_data/Cases_combinedPlot.png", width = 10, height = 16)


        
##############
# Figure 2: Case Rate Ratios, Mortality Rate Ratios
temp2<-bind_rows(mergeddata_wide,USA_RR_RD_formatted)
temp2<- temp2 %>% filter(state != "Puerto Rico") %>% filter(state != "US Virgin Islands")

temp2[sapply(temp2, is.infinite)] <- NA

temp2$state <- fct_relevel(as.factor(temp2$state), "United States")

# Case rate ratio
weekly_cases_IRR <- ggplot(temp2, aes (x=as.Date(date), y=cases_oneweek_rate_IRR))+geom_line()+
  theme(legend.position = "none")+ylim(0, 2)+facet_wrap(~state)+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")+
  theme(axis.text.x=element_text(angle=60, hjust=1), 
        strip.text.x = element_text(size = 8, margin = margin(1,1,1,1, unit = "pt")),
        panel.spacing = unit(0, 'pt'))+
  geom_hline(yintercept=1, linetype="dashed", color = "green") + 
  labs(x = "Month", y = "Weekly case rate ratio (men/women)", subtitle = "Case Rate Ratio")

# Mortality rate ratio
weekly_MRR <- ggplot(temp2, aes (x=as.Date(date), y=deaths_oneweek_rate_IRR))+geom_line()+
  theme(legend.position = "none")+ylim(0, 2)+facet_wrap(~state)+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")+
  geom_hline(yintercept=1, linetype="dashed", color = "blue")+
  theme(axis.text.x=element_text(angle=60, hjust=1), 
        strip.text.x = element_text(size = 8, margin = margin(1,1,1,1, unit = "pt")),
        panel.spacing = unit(0, 'pt'))+
  labs(x = "Month", y = "Weekly mortality rate ratio (men/women)", subtitle = "Mortality Rate Ratio")

rateRatios_combined <- weekly_cases_IRR / weekly_MRR + plot_annotation(tag_levels = 'a') + plot_layout(guides = 'collect')

ggsave(x=rateRatios_combined, filename = "figures/Combined_Val_Unval_data/RateRatios_combinedPlot.png", width = 10, height = 16)

############# 
# Figure 3: Mortality Rate (weekly and cumulative)

#deaths - RATE
MortalityRate_mvgAvg <- ggplot(temp,aes(x=as.Date(date),y=deaths_oneweek_rate_3wkrolling,group=sex,color=sex))+geom_line()+
  facet_wrap(~state,scales = "free_y")+ 
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")+
  theme(axis.text.x=element_text(angle=60, hjust=1), 
        strip.text.x = element_text(size = 8, margin = margin(1,1,1,1, unit = "pt")),
        panel.spacing = unit(0, 'pt'))+
  labs(x = "Month", y = "Three Week Rolling Average Mortality Rate per 100,000")

MortalityRate_mvgAvg_cumulative <- ggplot(temp,aes(x=as.Date(date),y=deaths_rate_3wkrolling,group=sex,color=sex))+geom_line()+
  facet_wrap(~state,scales = "free_y")+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")+
  theme(axis.text.x=element_text(angle=60, hjust=1), 
        strip.text.x = element_text(size = 8, margin = margin(1,1,1,1, unit = "pt")),
        panel.spacing = unit(0, 'pt'))+
  labs(x = "Month", y = "Three Week Rolling Average Cumulative Mortality Rate per 100,000")

mortality_combinedPlots <- MortalityRate_mvgAvg / MortalityRate_mvgAvg_cumulative +
   plot_annotation(tag_levels = 'a') + plot_layout(guides = 'collect')

ggsave(mortality_combinedPlots, filename = "figures/Combined_Val_Unval_data/mortality_combinedPlot.png", width = 12, height =16)