#-----------------------------------
# Invasive trend analysis: Quadrat Richness Total (QR_T)
#-----------------------------------
## ---- codesetup_QR_T ---- 
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
## ---- readdata_QR_T ----
#-----------------------------
# Read in data.frame without guids in first two columns
df<-read.csv("./data/NETN-MIDN-ERMN-NCRN_total_invasives.csv")#[,-c(1,2)]
df<-df %>% filter(park!='SAHI' & park !='WOTR' & park!='MONO') %>% 
  droplevels() %>% arrange(park,plot_name,cycle) # for some reason MONO breaks it
View(df)
df_park<-df %>% group_by(park) %>% nest()

#-------------------------------
## ---- QR_T_diag ----
#-----------------------------------
analysis.title<-"Quadrat Richness Total"

# Model without transformation
qrich.mod<-function(df) {lmer(avg.quad.r ~ cycle+ (1|plot_name),data=df)}
# random slope had singular fit, so went with simpler rand. intercept

prelim_by_park_QR_T<-df_park %>% mutate(model=map(data,qrich.mod) %>% set_names(df_park$park),
  resids=map2(data,model,add_residuals) %>% set_names(df_park$park),
  pred=map2(data,model,add_predictions) %>% set_names(df_park$park))

diag_QR_T<-unnest(prelim_by_park_QR_T, resids, pred)
res_QR_T<-residPlot(diag_QR_T)
hist_QR_T<-histPlot(diag_QR_T)

# Check conversion
conv_QR_T<-unlist(prelim_by_park_QR_T[['model']]) %>% map('optinfo') %>% 
  map('conv') %>% map('opt') %>% data.frame() %>% gather() 

conv_QR_T # all 0s. 

#-----------------------------------
##  ----  model_QR_T  ---- 
#-----------------------------------
# Quadrat Richness Results
#-----------------------------------
by_park_QR_T<-df_park %>% mutate(model=map(data,qrich.mod) %>% set_names(df_park$park),
  resids=map2(data,model,add_residuals) %>% set_names(df_park$park),
  pred=map2(data,model,add_predictions) %>% set_names(df_park$park))#,

# summarize model output
results_QR_T<-by_park_QR_T %>% mutate(summ=map(model,broom.mixed::tidy)) %>% 
  unnest(summ) %>%  filter(effect=='fixed') %>% 
  select(park,term,estimate) %>% arrange(park,term)

# reorder term factor, so can more easily associate the guilds with the terms, especially the reference term.
head(results_QR_T)
table(results_QR_T$term)

results_QR_T<-results_QR_T %>% arrange(park,term) %>% 
  mutate(estimate=round(estimate,3))

park_names2_QR_T<-rep(levels(df$park),each=2) # make vector of park names
park_names2_QR_T

results_QR_T<-results_QR_T %>% mutate(coef=ifelse(grepl('cycle',term),'Slope','Intercept'))

##  ---- model_results_QR_T ---- 
#-----------------------------------
# Create bootstrapped CIs on intercept and slopes
#-----------------------------------
by_park_coefs_QR_T<-by_park_QR_T %>% 
  mutate(conf.coef=map(model,~case_bootstrap(.x, fn=fixed_fun, B=1000, 
                                             resample=c(TRUE,FALSE))) %>% set_names(df_park$park)) %>% 
  select(conf.coef)  

coefs_QR_T<-by_park_coefs_QR_T %>% 
  mutate(bootCIs=map(conf.coef, ~bootCI(boot.t=.x$t))) %>% unnest(bootCIs) %>% 
  mutate(park=as.factor(park_names2_QR_T),
         type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything())

coefs2_QR_T<-coefs_QR_T %>% gather(term,coef,-park,-type,na.rm=T) %>% 
  spread(type,coef) %>% mutate(term=as.factor(term))

coefs2_QR_T<-coefs2_QR_T %>% arrange(park,term)
coefs2_QR_T$coef<-rep(c('Slope','Intercept'))

results2_QR_T<-merge(results_QR_T,coefs2_QR_T,by=c('park', 'coef'))

results_final_QR_T<-results2_QR_T %>% 
  mutate(estimate=round(estimate,4),
         lower=round(lower,4),
         upper=round(upper,4), 
         sign=ifelse(lower>0 | upper<0,1,0)) %>% 
  select(park,coef,estimate,lower,upper,sign) 

write.csv(results_final_QR_T,'./results/results_qrich-total-coefs_NP.csv', row.names=F)

##  ----  model_response_QR_T ---- 
#-----------------------------------
# Create bootstrapped CIs on response levels
#-----------------------------------
# Use parametric bootstrap to get confidence intervals around the response variable 
# for each cycle by guild level 
by_park_resp_QR_T<-by_park_QR_T %>% 
  mutate(conf.est=map(model,
    ~case_bootstrap(.x,fn=confFun,B=1000,resample=c(TRUE,FALSE))) %>% set_names(df_park$park))

by_park_resp_QR_T<-by_park_resp_QR_T %>% mutate(cols=map(model,~getColNames(.x)), 
  boot.t=map2(conf.est,cols,~setColNames(.x,.y))) # make labels for output

resp_QR_T<-by_park_resp_QR_T %>% mutate(boot.ci=map(boot.t,~bootCI(.x))) %>% 
  select(boot.ci) %>% unnest() # Calculate 95% CIs from bootstrap output

resp_QR_T<-resp_QR_T %>% mutate(park=as.factor(park_names2_QR_T),
  type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything()) 
# puts labels on boot output

resp2_QR_T<-resp_QR_T %>% gather(gcyc,ci,c1:c3) %>% spread(type,ci) %>% na.omit()
# reshapes data so lower and upper CIs in separate columns

resp_mean_QR_T<-by_park_resp_QR_T %>% 
  mutate(boot.mean=map(boot.t,~bootMean(.x))) %>% 
  select(boot.mean) %>% unnest()

labelsCI_QR_T<-df %>% na.omit() %>% group_by(park,cycle) %>% 
  summarise(numplots=n(),lat.rank=first(lat.rank)) %>% droplevels()

respCIs_QR_T<-data.frame(labelsCI_QR_T[,c('cycle','numplots','lat.rank')],
  resp2_QR_T,resp_mean_QR_T) %>% select(park,everything())

colnames(respCIs_QR_T)<-c('park','cycle','numplots','lat.rank','group',
                          'lower','upper','mean')

respCIs2_QR_T<-respCIs_QR_T %>% 
  mutate(cycle2=as.factor(cycle)) %>% arrange(park,cycle)

slopes_QR_T<-results_final_QR_T %>% filter(coef=='Slope') %>% droplevels()

respCIs_final_QR_T<-merge(respCIs2_QR_T,
  slopes_QR_T[,c('park','sign')],by=c('park'),all.x=T)

respCIs_final_QR_T<-respCIs_final_QR_T %>% 
  mutate(sign=as.factor(sign), park=reorder(park,-lat.rank)) %>% 
  arrange(lat.rank,cycle)

#View(respCIs_final_QR_T)

write.csv(respCIs_final_QR_T,"./results/results_qrich-total-response_NP.csv")

