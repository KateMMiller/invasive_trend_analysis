#-----------------------------------
# Invasive trend analysis: Quadrat % Frequency Total (QF_T)
#-----------------------------------
## ---- codesetup_QF_T ---- 
#-----------------------------------
library(tidyverse) # attaches most of the important packages in the tidyverse
library(lme4) # for glmer with Poisson
library(modelr) #for handling multiple models in tidyverse
library(broom.mixed)# for better model summary tables than default in nlme
library(prediction) # for find_data(model) function
library(lmeresampler)

options("scipen"=100, "digits"=4) # keeps TSN numbers as numbers 

source('./scripts/functions_for_ANALYSIS.R') # File containing functions

glmerCtlList <- glmerControl(optimizer=c("bobyqa","Nelder_Mead"),
                             optCtrl=list(maxfun=2e10000)) # set controls for glmer so longer before times out

#-----------------------------------
## ---- readdata_QF_T ----
#-----------------------------
# Read in data.frame without guids in first two columns
df<-read.csv("./data/NETN-MIDN-ERMN-NCRN_total_invasives.csv")#[,-c(1,2)]
df<-df %>% filter(park!='SAHI' & park !='WOTR') %>% droplevels() %>% arrange(park,plot_name,cycle)
table(df$park)

df_park<-df %>% group_by(park) %>% nest()

#-------------------------------
## ---- QF_T_diag ----
#-----------------------------------
analysis.title<-"Quadrat % Invasive Frequency Total"

# Model without transformation
qfreq.mod<-function(df) {lmer(qpct.freq ~ cycle+ (1|plot_name),data=df)}
# random slope had singular fit, so went with simpler rand. intercept

prelim_by_park_QF_T<-df_park %>% mutate(model=map(data,qfreq.mod),
  resids=map2(data,model,add_residuals),pred=map2(data,model,add_predictions))

diag_QF_T<-unnest(prelim_by_park_QF_T, resids, pred)
#res_QF_T<-residPlot(diag_QF_T)
#hist_QF_T<-histPlot(diag_QF_T) 

# Check conversion
conv_QF_T<-unlist(prelim_by_park_QF_T[['model']]) %>% map('optinfo') %>% 
  map('conv') %>% map('opt') %>% data.frame() %>% gather() 

conv.tbl_QF_T<-data.frame(cbind(park=levels(diag_QF_T$park),conv.code=conv_QF_T$value))
conv.tbl_QF_T # all 0s. 

#-----------------------------------
##  ----  model_QF_T  ---- 
#-----------------------------------
# Quadrat % Frequency Results
#-----------------------------------
by_park_QF_T<-df_park %>% mutate(model=map(data,qfreq.mod),
  resids=map2(data,model,add_residuals),pred=map2(data,model,add_predictions))#,

# summarize model output
results_QF_T<-by_park_QF_T %>% mutate(summ=map(model,broom.mixed::tidy)) %>% 
  unnest(summ) %>%  filter(effect=='fixed') %>% 
  select(park,term,estimate) %>% arrange(park,term)

# reorder term factor, so can more easily associate the guilds with the terms, especially the reference term.
table(results_QF_T$term)

results_QF_T<-results_QF_T %>% arrange(park,term) %>% 
  mutate(estimate=round(estimate,3))

park_names2_QF_T<-rep(levels(df$park),each=2) # make vector of park names
park_names2_QF_T

results_QF_T<-results_QF_T %>% mutate(coef=ifelse(grepl('cycle',term),'Slope','Intercept'))

##  ---- model_results_QF_T ---- 
#-----------------------------------
# Create bootstrapped CIs on intercept and slopes
#-----------------------------------
by_park_coefs_QF_T<-by_park_QF_T %>% 
  mutate(conf.coef=map(model,~case_bootstrap(.x, fn=fixed_fun, B=1000, resample=c(TRUE,FALSE)))) %>% 
  select(conf.coef)  

coefs_QF_T<-by_park_coefs_QF_T %>% 
  mutate(bootCIs=map(conf.coef, ~bootCI(boot.t=.x$t))) %>% unnest(bootCIs) %>% 
  mutate(park=as.factor(park_names2_QF_T),
         type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything())

coefs2_QF_T<-coefs_QF_T %>% gather(term,coef,-park,-type,na.rm=T) %>% 
  spread(type,coef) %>% mutate(term=as.factor(term))

coefs2_QF_T<-coefs2_QF_T %>% arrange(park,term)
coefs2_QF_T$coef<-rep(c('Slope','Intercept'))

results2_QF_T<-merge(results_QF_T,coefs2_QF_T,by=c('park', 'coef'))

results_final_QF_T<-results2_QF_T %>% 
  mutate(estimate=round(estimate,4),
         lower=round(lower,4),
         upper=round(upper,4), 
         sign=ifelse(lower>0 | upper<0,1,0)) %>% 
  select(park,coef,estimate,lower,upper,sign) 

write.csv(results_final_QF_T,'./results/results_qfreq-total-coefs_NP.csv', row.names=F)

##  ----  model_response_QF_T ---- 
#-----------------------------------
# Create bootstrapped CIs on response levels
#-----------------------------------
# Use parametric bootstrap to get confidence intervals around the response variable 
# for each cycle by guild level 
by_park_resp_QF_T<-by_park_QF_T %>% 
  mutate(conf.est=map(model,
    ~case_bootstrap(.x,fn=confFun,B=1000,resample=c(TRUE,FALSE))))

by_park_resp_QF_T<-by_park_resp_QF_T %>% mutate(cols=map(model,~getColNames(.x)), 
  boot.t=map2(conf.est,cols,~setColNames(.x,.y))) # make labels for output

resp_QF_T<-by_park_resp_QF_T %>% mutate(boot.ci=map(boot.t,~bootCI(.x))) %>% 
  select(boot.ci) %>% unnest() # Calculate 95% CIs from bootstrap output

resp_QF_T<-resp_QF_T %>% mutate(park=as.factor(park_names2_QF_T),
  type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything()) 
# puts labels on boot output

resp2_QF_T<-resp_QF_T %>% gather(gcyc,ci,c1:c3) %>% spread(type,ci) %>% na.omit()
# reshapes data so lower and upper CIs in separate columns

resp_mean_QF_T<-by_park_resp_QF_T %>% 
  mutate(boot.mean=map(boot.t,~bootMean(.x))) %>% 
  select(boot.mean) %>% unnest()

labelsCI_QF_T<-df %>% na.omit() %>% group_by(park,cycle) %>% 
  summarise(numplots=n(),lat.rank=first(lat.rank)) %>% droplevels()

respCIs_QF_T<-data.frame(labelsCI_QF_T[,c('cycle','numplots','lat.rank')],
  resp2_QF_T,resp_mean_QF_T) %>% select(park,everything())

colnames(respCIs_QF_T)<-c('park','cycle','numplots','lat.rank','group',
                          'lower','upper','mean')

respCIs2_QF_T<-respCIs_QF_T %>% 
  mutate(cycle2=as.factor(cycle)) %>% arrange(park,cycle)

slopes_QF_T<-results_final_QF_T %>% filter(coef=='Slope') %>% droplevels()

respCIs_final_QF_T<-merge(respCIs2_QF_T,
  slopes_QF_T[,c('park','sign')],by=c('park'),all.x=T)

respCIs_final_QF_T<-respCIs_final_QF_T %>% 
  mutate(sign=as.factor(sign), park=reorder(park,-lat.rank)) %>% 
  arrange(lat.rank,cycle)

#View(respCIs_final_QF_T)

write.csv(respCIs_final_QF_T,"./results/results_qfreq-total-response_NP.csv")


