#-----------------------------------
# Invasive trend analysis: Average Cover by Species (AC_S)
#-----------------------------------
## ---- codesetup_AC_S ---- 
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
## ---- readdata_AC_S ----
#-----------------------------
# Read in data.frame without guids in first two columns
df<-read.csv("./data/NETN-MIDN-ERMN-NCRN_species_invasives.csv")#[,-c(1,2)]
df<- df %>% arrange(park,plot_name,cycle,species) %>% filter(species!='noinvspp')
head(df)
View(df1)
# only include species with non-zero cover in at least plot and present in at least 10% of plots
df1<-df %>% group_by(park,species) %>% mutate(nonzero=sum(plot.freq,na.rm=T)/n(), sumcov=sum(avg.cover)) %>% 
  filter((park!='ACAD'& nonzero>0.1 & sumcov>0)|(park=='ACAD'& species=='Rhamnus frangula')) %>% 
  filter(park!='SAHI' & park!='WOTR') %>% filter(!(park=="MONO" & species=='Ampelopsis brevipedunculata')) %>%
  filter(!(park=='MONO' & species=="Celastrus orbiculatus")) %>% 
  droplevels() %>% ungroup(park,species)

# Too many species and not enough plots/df for MONO, so had to remove the least abundant spp.
table(df1$species,df1$park)
parkspp<-df1 %>% select(park,species) %>% unique()

df_park<-df1 %>% group_by(park) %>% nest()

df_park<-df_park %>% mutate(data=map(data,
                    ~mutate(.x,nlev=length(unique(species)))))

park_names2<-rep(levels(df1$park),each=2) # make vector of park names
park_names2

#-------------------------------
## ---- AC_S_diag ----
#-----------------------------------
analysis.title<-"Average % Invasive Cover by Species"

# Model without transformation
avgcov.mod<-function(df) {
  if (df$nlev[1]>1) {lmer(avg.cover ~ cycle*species + (1|plot_name),data=df)
  } else {lmer(avg.cover~cycle+(1|plot_name),data=df)} 
  } # random slope had singular fit, so went with simpler rand. intercept

prelim_by_park_AC_S<-df_park %>% mutate(model=map(data,avgcov.mod) %>% set_names(df_park$park),
  resids=map2(data,model,add_residuals) %>% set_names(df_park$park),
  pred=map2(data,model,add_predictions) %>% set_names(df_park$park))

diag_AC_S<-unnest(prelim_by_park_AC_S, resids, pred)
res_AC_S<-residPlot(diag_AC_S)
hist_AC_S<-histPlot(diag_AC_S) # residuals are pretty wonky. 

# Check conversion
conv_AC_S<-unlist(prelim_by_park_AC_S[['model']]) %>% map('optinfo') %>% 
  map('conv') %>% map('opt') %>% data.frame() %>% gather() 

conv_AC_S

#-----------------------------------
##  ----  model_AC_S  ---- 
#-----------------------------------
# Average Invasive % Cover Results
#-----------------------------------
by_park_AC_S<-df_park %>% mutate(model=map(data,avgcov.mod) %>% set_names(df_park$park),
  resids=map2(data,model,add_residuals) %>% set_names(df_park$park),
  pred=map2(data,model,add_predictions) %>% set_names(df_park$park))#,

# summarize model output
results_AC_S<-by_park_AC_S %>% mutate(summ=map(model,broom.mixed::tidy)) %>% 
  unnest(summ) %>%  filter(effect=='fixed') %>% 
  select(park,term,estimate) %>% arrange(park,term)

terms<-c('cycle','(Intercept)')
remove<-c('cycle:species','species')

results_AC_S2<-results_AC_S %>% mutate(coef=ifelse(grepl('cycle',term), 'Slope', 'Intercept'), 
                                      species=case_when(
                                        grepl('cycle:species',term) ~ str_remove(term,'cycle:species'), 
                                        grepl('species',term) ~str_remove(term,'species')
                                                     )) 
results_AC_S3<-results_AC_S2 %>% filter(!is.na(species)) %>% arrange(park,species)

first_alphas<-parkspp %>% arrange(park,species) %>% group_by(park) %>% summarise(species=first(species))

results_AC_S_first<-results_AC_S2 %>% filter(term %in% terms) %>% select(-species) %>%  droplevels()
results_AC_S_first2<-merge(results_AC_S_first, first_alphas, by='park')

results_AC_S_comb<-rbind(results_AC_S3,results_AC_S_first2)
nrow(results_AC_S_comb) #552

results_AC_S_comb<-results_AC_S_comb %>% arrange(park,species,coef) %>% 
  mutate(estimate=round(estimate,3))

##  ---- model_results_AC_S ---- 
#-----------------------------------
# Create bootstrapped CIs on intercept and slopes
#-----------------------------------
by_park_coefs_AC_S<-by_park_AC_S %>% 
  mutate(conf.coef=map(model,~case_bootstrap(.x, fn=fixed_fun, B=1000, resample=c(TRUE,FALSE))) %>% 
  set_names(df_park$park)) %>% 
  select(conf.coef)  

coefs_AC_S<-by_park_coefs_AC_S %>% 
  mutate(bootCIs=map(conf.coef, ~bootCI(boot.t=.x$t))) %>% unnest(bootCIs) %>% 
  mutate(park=as.factor(park_names2),
    type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything())

coefs2_AC_S<-coefs_AC_S %>% gather(term,coef,-park,-type,na.rm=T) %>% 
  spread(type,coef) %>% mutate(term=as.factor(term))

terms2<-c('cycle','X.Intercept.')

coefs2_AC_S2<-coefs2_AC_S %>% mutate(coef=ifelse(grepl('cycle',term), 'Slope', 'Intercept'), 
                                      species2=case_when(
                                        grepl('cycle.species',term) ~ str_remove(term,'cycle.species'), 
                                        grepl('species',term) ~str_remove(term,'species'), 
                                        term %in% terms2 ~'AAAfirst_alpha'), park2=park) %>% 
  arrange(park2,species2) %>% select(-park)

parkspp2<-data.frame(rbind(parkspp,parkspp)) %>% arrange(park,species)

coefs2_AC_S3<-cbind(coefs2_AC_S2,parkspp2)

coefs2_AC_S_comb<-coefs2_AC_S3 %>% select(park,term,species,coef,lower,upper) %>% arrange(park,species,coef)

results2_AC_S<-merge(results_AC_S_comb,coefs2_AC_S_comb,by=c('park','species', 'coef'), all.x=T, all.y=T)

results3_AC_S<- results2_AC_S %>% group_by(park,coef) %>% 
  mutate(rank=dense_rank(species))

results3b_AC_S<-results3_AC_S %>% filter(rank==1) %>% droplevels() %>% 
  mutate(est.corfac=estimate) %>% select(park,coef,est.corfac)

results4_AC_S<- merge(results3_AC_S, results3b_AC_S, by=c('park','coef'), all.x=T,all.y=T)

results5_AC_S<-results4_AC_S %>% 
  mutate(est.cor=ifelse(rank==1,est.corfac,est.corfac+estimate),
         lower.cor=ifelse(rank==1,lower,est.corfac+lower),
         upper.cor=ifelse(rank==1,upper,est.corfac+upper))

results_final_AC_S<-results5_AC_S %>% 
  mutate(estimate=round(est.cor,4),
    lower=round(lower.cor,4),
    upper=round(upper.cor,4), 
    sign=ifelse(lower>0 | upper<0,1,0)) %>% 
  select(park,species,coef,estimate,lower,upper,sign) 
  
#View(results_final_AC_S)

write.csv(results_final_AC_S,'./results/results_avecov-by_species-coefs_NP.csv', row.names=F)

##  ----  model_response_AC_S ---- 
#-----------------------------------
# Create bootstrapped CIs on response levels
#-----------------------------------
# Use non-parametric bootstrap to get confidence intervals around the response variable 
# for each cycle by guild level 
by_park_resp_AC_S<-by_park_AC_S %>% 
  mutate(conf.est=map(model,
    ~case_bootstrap(.x, fn=confFunSpp, B=1000, resample=c(TRUE,FALSE))) %>% set_names(df_park$park))

by_park_resp_AC_S<-by_park_resp_AC_S %>% mutate(cols=map(model,~getColNamesSpp(.x)), 
  boot.t=map2(conf.est,cols,~setColNames(.x,.y))) # make labels for output

resp_AC_S<-by_park_resp_AC_S %>% mutate(boot.ci=map(boot.t,~bootCI(.x))) %>% 
  select(boot.ci) %>% unnest() # Calculate 95% CIs from bootstrap output

resp_AC_S<-resp_AC_S %>% mutate(park=as.factor(park_names2),
  type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything()) 
# puts labels on boot output

resp2_AC_S<-resp_AC_S %>% gather(gcyc,ci,c1:c3_Vincetoxicum) %>% spread(type,ci) %>% na.omit()
# reshapes data so lower and upper CIs in separate columns
# CHECK COLUMN ORDER WHEN NEW PARKS ADDED.

resp_mean_AC_S<-by_park_resp_AC_S %>% 
  mutate(boot.mean=map(boot.t,~bootMean(.x))) %>% 
  select(boot.mean) %>% unnest()

labelsCI_AC_S<-df1 %>% na.omit() %>% group_by(park,cycle,species) %>% 
  summarise(numplots=n(),lat.rank=first(lat.rank)) %>% droplevels() #na.omit removes NCRN shrubs and COLO C1

respCIs_AC_S<-data.frame(labelsCI_AC_S[,c('cycle','species','numplots','lat.rank')],
  resp2_AC_S,resp_mean_AC_S) %>% select(park,everything())

table(respCIs_AC_S$park,respCIs_AC_S$cycle)

colnames(respCIs_AC_S)<-c('park','cycle','species','numplots','lat.rank','group','lower','upper','mean')

respCIs2_AC_S<-respCIs_AC_S %>% 
  mutate(cycle2=as.factor(cycle)) %>% arrange(park,species,cycle)

slopes_AC_S<-results_final_AC_S %>% filter(coef=='Slope') %>% droplevels()

respCIs_final_AC_S<-merge(respCIs2_AC_S,
  slopes_AC_S[,c('park','species','sign')],by=c('park','species'),all.x=T)

respCIs_final_AC_S<-respCIs_final_AC_S %>% 
  mutate(sign=as.factor(sign),speciessign=as.factor(paste(species,sign,sep='_')),species=as.factor(species),
    park=reorder(park,-lat.rank)) %>% 
  arrange(lat.rank,species,cycle)

View(respCIs_final_AC_S)
write.csv(respCIs_final_AC_S,"./results/results_avecov-by_species-response_NP.csv")
