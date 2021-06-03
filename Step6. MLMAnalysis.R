library( tidyverse )
library( lme4 )
library(arm)

# Set your working directory to "Tracker Analysis"

load("GenderSci_CovidMortalityProject/Tracker Analysis/Data/Combined Validated and Unvalidated Tracker Data/tracker.long.RData")
load("GenderSci_CovidMortalityProject/Tracker Analysis/Data/Combined Validated and Unvalidated Tracker Data/tracker.wide.RData")

dat_Tracker<-mergeddata_long %>% dplyr::select(state,sex,date,deaths_oneweek, population) %>% filter(sex!="total") %>% 
    pivot_wider(names_from=sex, values_from=c("deaths_oneweek", "population")) %>% 
    filter(deaths_oneweek_men>=0 & deaths_oneweek_women>=0) %>% # get rid of weeks with negative values
    ungroup() %>% 
    mutate(pctwomen=population_women/(population_women+population_men),
           total_rate=(deaths_oneweek_men+deaths_oneweek_women)/(population_women+population_men)*100000,
           date.cat=as.factor(date),
           obs=1:n())


#### Fit the model  ####
gm_nostate <- glmer(cbind(deaths_oneweek_men, deaths_oneweek_women) ~ 1 + (1|date.cat)  +  (1|obs),
             family = binomial, data = dat_Tracker )
summary( gm_nostate )

gm_nodate <- glmer(cbind(deaths_oneweek_men, deaths_oneweek_women) ~ 1  + (1 | state) +  (1|obs),
             family = binomial, data = dat_Tracker )
summary( gm_nodate )

gm2 <- glmer(cbind(deaths_oneweek_men, deaths_oneweek_women) ~ 1 + (1|date.cat) + (1 | state) +  (1|obs),
             family = binomial, data = dat_Tracker )
summary( gm2 )

anova( gm_nostate,gm2  ) #tests if a random effect for state is needed 

anova( gm_nodate,gm2  ) # tests if a radnom effect for time is needed


## table with three models
library(texreg)
htmlreg(c(gm_nodate,gm_nostate,gm2),custom.model.names=c("Model 1", "Model 2", "Model 3"),digits = 4)

texreg(gm2)

# These are the individual state estimated proportion male deaths.
arm::invlogit( coef( gm2 )$state )

#### Looking at predictions ####

dat_Tracker$pred_pi = predict( gm2, type = "response" )
head( dat_Tracker )

dat_Tracker = mutate( dat_Tracker, 
              RR = ((pred_pi)/(1-pred_pi)) * (pctwomen)/(1-pctwomen) )

#### Make canonical set of states to predict for ####

# This calculates the predicted rates taking out the week and observation random
# effects, to give a fair comparison of all the states estimated relative rates.
can_states = expand_grid( state = unique( dat_Tracker$state ),
                          date.cat = -1,
                          obs = -1 )

states = dat_Tracker %>% dplyr::select( state, pctwomen ) %>%
    distinct()
nrow( states )
can_states = left_join( can_states, states, by="state" )
can_states<-data.frame(can_states)

can_states = dplyr::mutate(can_states,
                     pred_pi = predict(gm2, newdata=can_states, allow.new.levels = TRUE,#re.form=NA,
                                        type = "response"),
                     RR = ((pred_pi)/(1-pred_pi)) * (pctwomen)/(1-pctwomen) )

can_states

# These are the model-based estimates of the relative rate (death rate of men/death rate of women):
summary( can_states$RR )


can_time = expand_grid( state = -1,
                        date.cat = unique( dat_Tracker$date.cat ),
                        obs = -1 )

pct_women = 0.508 ## (percent of total population that is women in the USA - using ACS 2015-2019 pop estimates)

can_time = mutate( can_time,
                   pred_pi = predict( gm2, newdata=can_time, allow.new.levels = TRUE,
                                      type = "response"),
                   RR = ((pred_pi)/(1-pred_pi)) * (pct_women)/(1-pct_women) )

ggplot( can_time, aes( as.Date(date.cat), RR ) ) +
    geom_point()+geom_line()+geom_hline(yintercept=1)+scale_x_date(date_breaks="1 month")


gg  <- dat_Tracker %>% group_by( date.cat ) %>%
    summarise( avg_RR = mean( RR ) )

ggplot( gg, aes( as.Date( date.cat ), avg_RR ) ) +
    geom_line() + geom_point() 

NY<-dat_Tracker %>% filter(state=="New York")
TX<-dat_Tracker %>% filter(state=="Texas")
CT<-dat_Tracker %>% filter(state=="Connecticut")
    
#combined plot
  finalfigure<- ggplot( ) +
    geom_line( data=dat_Tracker, aes( date, RR, group=state ), alpha=0.7, color="grey",lwd=0.5) +
    geom_hline( yintercept = 1, col="black",linetype = "dashed" )+
    geom_line(data=NY,aes(date,RR),color="#1F78B4",lwd=1)+
    geom_line(data=CT,aes(date,RR),color="#33A02C",lwd=1)+
    geom_line(data=TX,aes(date,RR),color="#FF7F00",lwd=1)+
    geom_line(data=gg, aes( as.Date( date.cat ), avg_RR),color="black", lwd=2)+  # AVERAGE Line
    theme_bw()+
    scale_x_date(date_breaks="1 month",date_labels = "%b %Y")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    ylim(0,2)+
    labs(y= "Estimated Mortality Rate Ratio (Men/Women)", x = NULL )

ggsave(finalfigure, width = 6,
       height = 4,
       units = "in",
       dpi = 300, filename = "GenderSci_CovidMortalityProject/Tracker Analysis/figures/MLM output/Estimated.MRR.ModelOutput.png")
