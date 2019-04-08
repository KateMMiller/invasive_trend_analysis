#-----------------------------------
# Invasive trend analysis: Average Cover Total Invasives (AC_T)
#-----------------------------------
## ---- codesetup_AC_T ---- 
#-----------------------------------
library(tidyverse) # attaches most of the important packages in the tidyverse
library(lme4) # for lmer
library(modelr) #for handling multiple models in tidyverse
library(broom.mixed)# for better model summary tables than default in nlme
library(prediction) # for find_data(model) function
library(lmeresampler) # for case bootstrap

options("scipen"=100, "digits"=4) # keeps TSN numbers as numbers 

source('./scripts/functions_for_ANALYSIS.R') # File containing functions

#-----------------------------------
## ---- readdata_AC_T ----
#-----------------------------
# Read in data.frame without guids in first two columns
df<-read.csv("./data/NETN-MIDN-ERMN-NCRN_total_invasives.csv")#[,-c(1,2)]
df<-df %>% filter(park!='SAHI' & park !='WOTR') %>% droplevels() %>% arrange(park,plot_name,cycle)

df_park<-df %>% group_by(park) %>% nest()

#-------------------------------
## ---- AC_T_diag ----
#-----------------------------------
analysis.title<-"Average % Invasive Cover Total"

# Model without transformation
avgcov.mod<-function(df) {lmer(avg.cover ~ cycle + (1|plot_name),data=df)}
# random slope had singular fit, so went with simpler rand. intercept

prelim_by_park_AC_T<-df_park %>% mutate(model=map(data,avgcov.mod),
  resids=map2(data,model,add_residuals),pred=map2(data,model,add_predictions))

diag_AC_T<-unnest(prelim_by_park_AC_T, resids, pred)
res_AC_T<-residPlot(diag_AC_T)
hist_AC_T<-histPlot(diag_AC_T) # residuals are pretty wonky. 

# Check conversion
conv_AC_T<-unlist(prelim_by_park_AC_T[['model']]) %>% map('optinfo') %>% 
  map('conv') %>% map('opt') %>% data.frame() %>% gather() 

conv.tbl_AC_T<-data.frame(cbind(park=levels(diag_AC_T$park),conv.code=conv_AC_T$value))
conv.tbl_AC_T # all 0s. 

#-----------------------------------
##  ----  model_AC_T  ---- 
#-----------------------------------
# Average Invasive % Cover Results
#-----------------------------------
by_park_AC_T<-df_park %>% mutate(model=map(data,avgcov.mod),
  resids=map2(data,model,add_residuals),pred=map2(data,model,add_predictions))#,

# summarize model output
results_AC_T<-by_park_AC_T %>% mutate(summ=map(model,broom.mixed::tidy)) %>% 
  unnest(summ) %>%  filter(effect=='fixed') %>% 
  select(park,term,estimate) %>% arrange(park,term)

# reorder term factor, so can more easily associate the guilds with the terms, especially the reference term.
results_AC_T<-results_AC_T %>% arrange(park,term) %>% 
  mutate(estimate=round(estimate,3))

park_names2_AC_T<-rep(levels(df$park),each=2) # make vector of park names
park_names2_AC_T

results_AC_T<-results_AC_T %>% mutate(coef=ifelse(grepl('cycle',term),'Slope','Intercept'))

##  ---- model_results_AC_T ---- 
#-----------------------------------
# Create bootstrapped CIs on intercept and slopes
#-----------------------------------
by_park_coefs_AC_T<-by_park_AC_T %>% 
  mutate(conf.coef=map(model,~case_bootstrap(.x, fn=fixed_fun, B=1000, resample=c(TRUE,FALSE)))) %>% 
  select(conf.coef)  

coefs_AC_T<-by_park_coefs_AC_T %>% 
  mutate(bootCIs=map(conf.coef, ~bootCI(boot.t=.x$t))) %>% unnest(bootCIs) %>% 
  mutate(park=as.factor(park_names2_AC_T),
         type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything())

coefs2_AC_T<-coefs_AC_T %>% gather(term,coef,-park,-type,na.rm=T) %>% 
  spread(type,coef) %>% mutate(term=as.factor(term))

coefs2_AC_T<-coefs2_AC_T %>% arrange(park,term)
coefs2_AC_T$coef<-rep(c('Slope','Intercept'))

results2_AC_T<-merge(results_AC_T,coefs2_AC_T,by=c('park', 'coef'))

results_final_AC_T<-results2_AC_T %>% 
  mutate(estimate=round(estimate,4),
         lower=round(lower,4),
         upper=round(upper,4), 
         sign=ifelse(lower>0 | upper<0,1,0)) %>% 
  select(park,coef,estimate,lower,upper,sign) 

#View(results_final_AC_T)
write.csv(results_final_AC_T,'./results/results_avecov-total-coefs_NP.csv', row.names=F)

##  ----  model_response_AC_T ---- 
#-----------------------------------
# Create bootstrapped CIs on response levels
#-----------------------------------
# Use parametric bootstrap to get confidence intervals around the response variable 
# for each cycle by guild level 
by_park_resp_AC_T<-by_park_AC_T %>% 
  mutate(conf.est=map(model,
    ~case_bootstrap(.x,fn=confFun,B=1000,resample=c(TRUE,FALSE))))

by_park_resp_AC_T<-by_park_resp_AC_T %>% mutate(cols=map(model,~getColNames(.x)), 
  boot.t=map2(conf.est,cols,~setColNames(.x,.y))) # make labels for output

resp_AC_T<-by_park_resp_AC_T %>% mutate(boot.ci=map(boot.t,~bootCI(.x))) %>% 
  select(boot.ci) %>% unnest() # Calculate 95% CIs from bootstrap output

resp_AC_T<-resp_AC_T %>% mutate(park=as.factor(park_names2_AC_T),
  type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything()) 
# puts labels on boot output

resp2_AC_T<-resp_AC_T %>% gather(gcyc,ci,c1:c3) %>% spread(type,ci) %>% na.omit()
# reshapes data so lower and upper CIs in separate columns

resp_mean_AC_T<-by_park_resp_AC_T %>% 
  mutate(boot.mean=map(boot.t,~bootMean(.x))) %>% 
  select(boot.mean) %>% unnest()

labelsCI_AC_T<-df %>% na.omit() %>% group_by(park,cycle) %>% 
  summarise(numplots=n(),lat.rank=first(lat.rank)) %>% droplevels()

respCIs_AC_T<-data.frame(labelsCI_AC_T[,c('cycle','numplots','lat.rank')],
  resp2_AC_T,resp_mean_AC_T) %>% select(park,everything())

colnames(respCIs_AC_T)<-c('park','cycle','numplots','lat.rank','group',
                          'lower','upper','mean')

respCIs2_AC_T<-respCIs_AC_T %>% 
  mutate(cycle2=as.factor(cycle)) %>% arrange(park,cycle)

slopes_AC_T<-results_final_AC_T %>% filter(coef=='Slope') %>% droplevels()

respCIs_final_AC_T<-merge(respCIs2_AC_T,
  slopes_AC_T[,c('park','sign')],by=c('park'),all.x=T)

respCIs_final_AC_T<-respCIs_final_AC_T %>% 
  mutate(sign=as.factor(sign), park=reorder(park,-lat.rank)) %>% 
  arrange(lat.rank,cycle)

#View(respCIs_final_AC_T)
write.csv(respCIs_final_AC_T,"./results/results_avecov-total-response_NP.csv")

