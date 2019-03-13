#-----------------------------------
# Invasive trend analysis: Plot Frequency Total (PF_T)
#-----------------------------------
## ---- codesetup_PF_T ---- 
#-----------------------------------
library(tidyverse) # attaches most of the important packages in the tidyverse
library(lme4) # for glmer with Poisson
library(modelr) #for handling multiple models in tidyverse
library(broom.mixed)# for better model summary tables than default in nlme
library(sjstats) # for overdisp
library(prediction) # for find_data(model) function

options("scipen"=100, "digits"=4) # keeps TSN numbers as numbers 

source('./scripts/functions_for_ANALYSIS.R') # File containing functions

glmerCtlList <- glmerControl(optimizer=c("bobyqa","Nelder_Mead"),
  optCtrl=list(maxfun=2e10000)) # set controls for glmer so longer before times out

#-----------------------------------
## ---- readdata_PF_T ----
#-----------------------------
# Read in data.frame without guids in first two columns
df<-read.csv("./data/NETN-MIDN-ERMN-NCRN_total_invasives.csv")#[,-c(1,2)]
df<- df %>% arrange(park,plot_name,cycle)
df2<-df %>% filter(!is.na(plot.freq))

# only include guilds with at least 10% of plots with that guild and <90% of all
df3<-df2 %>% group_by(park) %>% mutate(nonzero=sum(plot.freq,na.rm=T)/n()) %>% 
  filter(between(nonzero,0.1,0.9)) %>% 
  droplevels() %>% ungroup(park) 

# FRHI Herbaceous, MIMA Shrub and SAHI Shrub are 100% present the whole time.
# Several other park/guild combinations are close to 100%, which causes problems in glmer
# Had to remove guilds that were 100 plot frequency in all cycles 
df_park<-df3 %>% group_by(park) %>% nest()

#-------------------------------
## ---- PF_T_diag ----
#-----------------------------------
analysis.title<-"Invasive Plot Frequency Total"

# Model without transformation
PFreq.mod<-function(df) {
  glmer(plot.freq~cycle+(1|plot_name), family=binomial, 
    control=glmerCtlList,data=df)
  } # random slope had singular fit, so went with simpler rand. intercept

prelim_by_park_PF_T<-df_park %>% mutate(model=map(data,PFreq.mod),
  resids=map2(data,model,add_residuals),pred=map2(data,model,add_predictions))

# Check conversion
conv_PF_T<-unlist(prelim_by_park_PF_T[['model']]) %>% map('optinfo') %>% 
  map('conv') %>% map('opt') %>% data.frame() %>% gather() 

conv.tbl_PF_T<-data.frame(cbind(park=levels(diag_PF_T$park),conv.code=conv_PF_T$value))
conv.tbl_PF_T # all 0s. 

#-----------------------------------
##  ----  model_PF_T  ---- 
#-----------------------------------
# Plot Frequency Results
#-----------------------------------
by_park_PF_T<-df_park %>% mutate(model=map(data,PFreq.mod),
  resids=map2(data,model,add_residuals),pred=map2(data,model,add_predictions))#,

# summarize model output
results_PF_T<-by_park_PF_T %>% mutate(summ=map(model,broom.mixed::tidy)) %>% 
  unnest(summ) %>%  filter(effect=='fixed') %>% 
  select(park,term,estimate,std.error) %>% arrange(park,term)

# reorder term factor, so can more easily associate the guilds with the terms, especially the reference term.
table(results_PF_T$term)

results_PF_T<-results_PF_T %>% arrange(park,term) %>% 
  mutate(estimate=round(estimate,3),std.error=round(std.error,3))

park_names2_PF_T<-rep(levels(df3$park),each=2) # make vector of park names
park_names2_PF_T

results_PF_T<-results_PF_T %>% mutate(coef=ifelse(grepl('cycle',term),'Slope','Intercept'))

##  ---- model_results_PF_T ---- 
#-----------------------------------
# Create bootstrapped CIs on intercept and slopes
#-----------------------------------
by_park_coefs_PF_T<-by_park_PF_T %>% 
  mutate(conf.coef=map(model,~bootMer(.x,FUN=fixef,nsim=1000, parallel='snow',
    ncpus=7))) %>% select(conf.coef)  

coefs_PF_T<-by_park_coefs_PF_T %>% 
  mutate(bootCIs=map(conf.coef, ~bootCI(boot.t=.x$t))) %>% unnest(bootCIs) %>% 
  mutate(park=as.factor(park_names2_PF_T),
    type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything())

coefs2_PF_T<-coefs_PF_T %>% gather(term,coef,-park,-type,na.rm=T) %>% 
  spread(type,coef) %>% mutate(term=as.factor(term))

coefs2_PF_T<-coefs2_PF_T %>% arrange(park,term)
coefs2_PF_T$coef<-rep(c('Slope','Intercept'))

results2_PF_T<-merge(results_PF_T,coefs2_PF_T,by=c('park', 'coef'))

results_final_PF_T<-results2_PF_T %>% 
  mutate(estimate=round(estimate,4),
    lower=round(lower,4),
    upper=round(upper,4), 
    sign=ifelse(lower>0 | upper<0,1,0)) %>% 
  select(park,coef,estimate,lower,upper,sign) 

write.csv(results_final_PF_T,'./results/results_PFreq-total-coefs.csv', row.names=F)

View(results_final_PF_T)

# Did not bootstrap for responses like other metrics. Ultimately only interested if the odds
# of a plot having an invasive increases over time, which we get at with the coefs. 