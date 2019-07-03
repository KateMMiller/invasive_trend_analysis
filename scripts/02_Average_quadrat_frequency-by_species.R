#-----------------------------------
# Invasive trend analysis: Quadrat % Frequency by Species (QF_S)
#-----------------------------------
## ---- codesetup_QF_S ---- 
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
## ---- readdata_QF_S ----
#-----------------------------
# Read in data.frame without guids in first two columns
df<-read.csv("./data/NETN-MIDN-NCRN_species_invasives.csv")#[,-c(1,2)]
df<- df %>% arrange(park,plot_name,cycle,species) %>% filter(species!='noinvspp')
head(df)

# only include species with at least 10% of plots with that guild
df1<-df %>% group_by(park,species) %>% mutate(nonzero=sum(plot.freq,na.rm=T)/n()) %>% 
  filter((park!='ACAD'& nonzero>0.1)|(park=='ACAD'& species=='Rhamnus frangula')) %>% 
  filter(park!='SAHI' & park!='WOTR') %>% 
  droplevels() %>% ungroup(park,species)
table(df1$species,df1$park)
parkspp<-df1 %>% select(park,species) %>% unique()

df_park<-df1 %>% group_by(park) %>% nest()

df_park<-df_park %>% mutate(data=map(data,
                                     ~mutate(.x,nlev=length(unique(species)))))

park_names2<-rep(levels(df1$park),each=2) # make vector of park names
park_names2

#-------------------------------
## ---- QF_S_diag ----
#-----------------------------------
analysis.title<-"Quadrat % Invasive Frequency by Species"

# Model without transformation
qfreq.mod<-function(df) {
  if (df$nlev[1]>1) {lmer(qpct.freq ~ cycle*species + (1|plot_name),data=df)
  } else {lmer(qpct.freq~cycle+(1|plot_name),data=df)} 
} # random slope had singular fit, so went with simpler rand. intercept

prelim_by_park_QF_S<-df_park %>% mutate(model=map(data,qfreq.mod) %>% set_names(df_park$park),
                  resids=map2(data,model,add_residuals) %>% set_names(df_park$park),
                  pred=map2(data,model,add_predictions) %>% set_names(df_park$park))

diag_QF_S<-unnest(prelim_by_park_QF_S, resids, pred)
res_QF_S<-residPlot(diag_QF_S)
hist_QF_S<-histPlot(diag_QF_S) # residuals are pretty wonky. 

# Check conversion
conv_QF_S<-unlist(prelim_by_park_QF_S[['model']]) %>% map('optinfo') %>% 
  map('conv') %>% map('opt') %>% data.frame() %>% gather() 

conv_QF_S # all 0s. 

#-----------------------------------
##  ----  model_QF_S  ---- 
#-----------------------------------
# Average Invasive % Cover Results
#-----------------------------------
by_park_QF_S<-df_park %>% mutate(model=map(data,qfreq.mod) %>% set_names(df_park$park),
    resids=map2(data,model,add_residuals) %>% set_names(df_park$park),
    pred=map2(data,model,add_predictions) %>% set_names(df_park$park))

# summarize model output
results_QF_S<-by_park_QF_S %>% mutate(summ=map(model,broom.mixed::tidy)) %>% 
  unnest(summ) %>%  filter(effect=='fixed') %>% 
  select(park,term,estimate) %>% arrange(park,term)

terms<-c('cycle','(Intercept)')
remove<-c('cycle:species','species')

results_QF_S2<-results_QF_S %>% mutate(coef=ifelse(grepl('cycle',term), 'Slope', 'Intercept'), 
                                       species=case_when(
                                         grepl('cycle:species',term) ~ str_remove(term,'cycle:species'), 
                                         grepl('species',term) ~str_remove(term,'species')
                                       )) 
results_QF_S3<-results_QF_S2 %>% filter(!is.na(species)) %>% arrange(park,species)

first_alphas<-parkspp %>% arrange(park,species) %>% group_by(park) %>% summarise(species=first(species))

results_QF_S_first<-results_QF_S2 %>% filter(term %in% terms) %>% select(-species) %>% droplevels()
results_QF_S_first2<-merge(results_QF_S_first, first_alphas, by='park')

results_QF_S_comb<-rbind(results_QF_S3,results_QF_S_first2)
nrow(results_QF_S_comb) #386

results_QF_S_comb<-results_QF_S_comb %>% arrange(park,species,coef) %>% 
  mutate(estimate=round(estimate,3))

##  ---- model_results_QF_S ---- 
#-----------------------------------
# Create bootstrapped CIs on intercept and slopes
#-----------------------------------
by_park_coefs_QF_S<-by_park_QF_S %>% 
  mutate(conf.coef=map(model,~case_bootstrap(.x, fn=fixed_fun, B=1000, 
                                             resample=c(TRUE,FALSE))) %>% set_names(df_park$park)) %>% 
  select(conf.coef)  

coefs_QF_S<-by_park_coefs_QF_S %>% 
  mutate(bootCIs=map(conf.coef, ~bootCI(boot.t=.x$t))) %>% unnest(bootCIs) %>% 
  mutate(park=as.factor(park_names2),
         type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything())

coefs2_QF_S<-coefs_QF_S %>% gather(term,coef,-park,-type,na.rm=T) %>% 
  spread(type,coef) %>% mutate(term=as.factor(term))

terms2<-c('cycle','X.Intercept.')

coefs2_QF_S2<-coefs2_QF_S %>% mutate(coef=ifelse(grepl('cycle',term), 'Slope', 'Intercept'), 
                                     species2=case_when(
                                       grepl('cycle.species',term) ~ str_remove(term,'cycle.species'), 
                                       grepl('species',term) ~str_remove(term,'species'), 
                                       term %in% terms2 ~'AAAfirst_alpha'), park2=park) %>% 
  arrange(park2,species2) %>% select(-park)

parkspp2<-data.frame(rbind(parkspp,parkspp)) %>% arrange(park,species)

coefs2_QF_S3<-cbind(coefs2_QF_S2,parkspp2)

coefs2_QF_S_comb<-coefs2_QF_S3 %>% select(park,term,species,coef,lower,upper) %>% arrange(park,species,coef)

results2_QF_S<-merge(results_QF_S_comb,coefs2_QF_S_comb,by=c('park','species', 'coef'), all.x=T, all.y=T)

results3_QF_S<- results2_QF_S %>% group_by(park,coef) %>% 
  mutate(rank=dense_rank(species))

results3b_QF_S<-results3_QF_S %>% filter(rank==1) %>% droplevels() %>% 
  mutate(est.corfac=estimate) %>% select(park,coef,est.corfac)

results4_QF_S<- merge(results3_QF_S, results3b_QF_S, by=c('park','coef'), all.x=T,all.y=T)

results5_QF_S<-results4_QF_S %>% 
  mutate(est.cor=ifelse(rank==1,est.corfac,est.corfac+estimate),
         lower.cor=ifelse(rank==1,lower,est.corfac+lower),
         upper.cor=ifelse(rank==1,upper,est.corfac+upper))

results_final_QF_S<-results5_QF_S %>% 
  mutate(estimate=round(est.cor,4),
         lower=round(lower.cor,4),
         upper=round(upper.cor,4), 
         sign=ifelse(lower>0 | upper<0,1,0)) %>% 
  select(park,species,coef,estimate,lower,upper,sign) 

write.csv(results_final_QF_S,'./results/results_qfreq-by_species-coefs_NP.csv', row.names=F)

##  ----  model_response_QF_S ---- 
#-----------------------------------
# Create bootstrapped CIs on response levels
#-----------------------------------
# Use parametric bootstrap to get confidence intervals around the response variable 
# for each cycle by guild level 
by_park_resp_QF_S<-by_park_QF_S %>% 
  mutate(conf.est=map(model,
  ~case_bootstrap(.x, fn=confFunSpp, B=1000, resample=c(TRUE,FALSE))) %>% set_names(df_park$park))

by_park_resp_QF_S<-by_park_resp_QF_S %>% mutate(cols=map(model,~getColNamesSpp(.x)), 
  boot.t=map2(conf.est,cols,~setColNames(.x,.y))) # make labels for output

resp_QF_S<-by_park_resp_QF_S %>% mutate(boot.ci=map(boot.t,~bootCI(.x))) %>% 
  select(boot.ci) %>% unnest() # Calculate 95% CIs from bootstrap output

resp_QF_S<-resp_QF_S %>% mutate(park=as.factor(park_names2),
  type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything()) 
# puts labels on boot output

resp2_QF_S<-resp_QF_S %>% gather(gcyc,ci,c1:c3_Vincetoxicum) %>% spread(type,ci) %>% na.omit()
# reshapes data so lower and upper CIs in separate columns
# CHECK COLUMN ORDER WHEN NEW PARKS ADDED.

resp_mean_QF_S<-by_park_resp_QF_S %>% 
  mutate(boot.mean=map(boot.t,~bootMean(.x))) %>% 
  select(boot.mean) %>% unnest()

labelsCI_QF_S<-df1 %>% na.omit() %>% group_by(park,cycle,species) %>% 
  summarise(numplots=n(),lat.rank=first(lat.rank)) %>% droplevels() #na.omit removes NCRN shrubs and COLO C1

respCIs_QF_S<-data.frame(labelsCI_QF_S[,c('cycle','species','numplots','lat.rank')],
  resp2_QF_S,resp_mean_QF_S) %>% select(park,everything())

table(respCIs_QF_S$park,respCIs_QF_S$cycle)

colnames(respCIs_QF_S)<-c('park','cycle','species','numplots','lat.rank','group','lower','upper','mean')

respCIs2_QF_S<-respCIs_QF_S %>% 
  mutate(cycle2=as.factor(cycle)) %>% arrange(park,species,cycle)

slopes_QF_S<-results_final_QF_S %>% filter(coef=='Slope') %>% droplevels()

respCIs_final_QF_S<-merge(respCIs2_QF_S,
  slopes_QF_S[,c('park','species','sign')],by=c('park','species'),all.x=T)

respCIs_final_QF_S<-respCIs_final_QF_S %>% 
  mutate(sign=as.factor(sign),speciessign=as.factor(paste(species,sign,sep='_')),species=as.factor(species),
         park=reorder(park,-lat.rank)) %>% 
  arrange(lat.rank,species,cycle)

#View(respCIs_final_QF_S)
write.csv(respCIs_final_QF_S,"./results/results_qfreq-by_species-response_NP.csv")
