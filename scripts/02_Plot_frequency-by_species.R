#-----------------------------------
# Invasive trend analysis: Plot Frequency by Species (PF_S)
#-----------------------------------
## ---- codesetup_PF_S ---- 
#-----------------------------------
library(tidyverse) # attaches most of the important packages in the tidyverse
library(lme4) # for glmer with logistic regression
library(modelr) #for handling multiple models in tidyverse
library(broom.mixed)# for better model summary tables than default in nlme
library(prediction) # for find_data(model) function
library(beepr) # indicates when model run is finished

options("scipen"=100, "digits"=4) # keeps TSN numbers as numbers 

source('./scripts/functions_for_ANALYSIS.R') # File containing functions

glmerCtlList <- glmerControl(optimizer=c("bobyqa","Nelder_Mead"),
  optCtrl=list(maxfun=2e10000)) # set controls for glmer so longer before times out

#-----------------------------------
## ---- readdata_PF_S ----
#-----------------------------
# Read in data.frame without guids in first two columns
df<-read.csv("./data/NETN-MIDN-ERMN-NCRN_species_invasives.csv")#[,-c(1,2)]
df<- df %>% arrange(park,plot_name,cycle,species) %>% filter(species!='noinvspp')

# only include speciess with at least 10% of plots with that species and <90% of all
df1<-df %>% group_by(park,species) %>% mutate(nonzero=sum(plot.freq,na.rm=T)/n()) %>% 
  filter((park!='ACAD'& between(nonzero,0.1,0.9))|(park=='ACAD'& species=='Rhamnus frangula')) %>% 
  filter(park!='SAHI' & park!='WOTR') %>%
  droplevels() %>% ungroup(park,species) 
parkspp<-df1 %>% select(park,species) %>% unique()

df_park<-df1 %>% group_by(park) %>% nest()

df_park<-df_park %>% mutate(data=map(data,
                    ~mutate(.x,nlev=length(unique(species)))))

park_names2<-rep(levels(df1$park),each=2) # make vector of park names
park_names2

#-----------------------------------
##  ----  model_PF_S  ---- 
#-----------------------------------
# Plot Frequency Results
#-----------------------------------
analysis.title<-"Invasive Plot Frequency by Species"

# Model without transformation
PFreq.mod<-function(df) {
  if (df$nlev[1]>1) {glmer(plot.freq ~ cycle*species + (1|plot_name), 
    family=binomial, control=glmerCtlList, data=df)
  } else {glmer(plot.freq~cycle+(1|plot_name), family=binomial, 
    control=glmerCtlList,data=df)} 
  } # random slope had singular fit, so went with simpler rand. intercept
  # It's too time intensive to run models twice.

by_park_PF_S<-df_park %>% mutate(model=map(data,PFreq.mod) %>% set_names(df_park$park),
  resids=map2(data,model,add_residuals) %>% set_names(df_park$park),
  pred=map2(data,model,add_predictions) %>% set_names(df_park$park))#,

diag_PF_S<-unnest(by_park_PF_S, resids, pred)
#res_PF_S<-residPlot(diag_PF_S)
#hist_PF_S<-histPlot(diag_PF_S) 

# Check conversion
conv_PF_S<-unlist(by_park_PF_S[['model']]) %>% map('optinfo') %>% 
  map('conv') %>% map('opt') %>% data.frame() %>% gather() 

conv_PF_S # all 0s. 

# summarize model output
results_PF_S<-by_park_PF_S %>% mutate(summ=map(model,broom.mixed::tidy)) %>% 
  unnest(summ) %>%  filter(effect=='fixed') %>% 
  select(park,term,estimate,std.error) %>% arrange(park,term)

terms<-c('cycle','(Intercept)')
remove<-c('cycle:species','species')

results_PF_S2<-results_PF_S %>% mutate(coef=ifelse(grepl('cycle',term), 'Slope', 'Intercept'), 
                                       species=case_when(
                                         grepl('cycle:species',term) ~ str_remove(term,'cycle:species'), 
                                         grepl('species',term) ~str_remove(term,'species')
                                       )) 
results_PF_S3<-results_PF_S2 %>% filter(!is.na(species)) %>% arrange(park,species)

first_alphas<-parkspp %>% arrange(park,species) %>% group_by(park) %>% summarise(species=first(species))

results_PF_S_first<-results_PF_S2 %>% filter(term %in% terms) %>% select(-species) %>%  droplevels()
results_PF_S_first2<-merge(results_PF_S_first, first_alphas, by='park')

results_PF_S_comb<-rbind(results_PF_S3,results_PF_S_first2)
nrow(results_PF_S_comb) #386

results_PF_S_comb<-results_PF_S_comb %>% arrange(park,species,coef) %>% 
  mutate(estimate=round(estimate,3))

#View(results_PF_S_comb)
##  ---- model_results_PF_S ---- 
#-----------------------------------
# Create bootstrapped CIs on intercept and slopes
#-----------------------------------
by_park_coefs_PF_S<-by_park_PF_S %>% 
  mutate(conf.coef=map(model,~bootMer(.x,FUN=fixed_fun,nsim=1000, parallel='snow',
    ncpus=10)) %>% set_names(df_park$park)) %>% 
  select(conf.coef)  

coefs_PF_S<-by_park_coefs_PF_S %>% 
  mutate(bootCIs=map(conf.coef, ~bootCI(boot.t=.x$t))) %>% unnest(bootCIs) %>% 
  mutate(park=as.factor(park_names2),
    type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything())

coefs2_PF_S<-coefs_PF_S %>% gather(term,coef,-park,-type,na.rm=T) %>% 
  spread(type,coef) %>% mutate(term=as.factor(term))

terms2<-c('cycle','X.Intercept.')

coefs2_PF_S2<-coefs2_PF_S %>% mutate(coef=ifelse(grepl('cycle',term), 'Slope', 'Intercept'), 
                                     species2=case_when(
                                       grepl('cycle.species',term) ~ str_remove(term,'cycle.species'), 
                                       grepl('species',term) ~str_remove(term,'species'), 
                                       term %in% terms2 ~'AAAfirst_alpha'), park2=park) %>% 
  arrange(park2,species2) %>% select(-park)

parkspp2<-data.frame(rbind(parkspp,parkspp)) %>% arrange(park,species)

#coefs2_PF_S2 is missing slopes for Viburnum dilatatum in 2 parks. Not sure what happened.
missing_row1<-c('cycle.speciesViburnum.dilatatum', 0, 0, 'Slope', 'Viburnum.dilatatum', 'GWMP')
missing_row2<-c('cycle.speciesViburnum.dilatatum', 0, 0, 'Slope', 'Viburnum.dilatatum', 'ROCR')

coefs2_PF_S2b<-rbind(coefs2_PF_S2, missing_row1, missing_row2)
coefs2_PF_S2b<-coefs2_PF_S2b %>% arrange(park2, species2, coef)

coefs2_PF_S3<-cbind(coefs2_PF_S2b,parkspp2)

coefs2_PF_S_comb<-coefs2_PF_S3 %>% select(park,term,species,coef,lower,upper) %>% arrange(park,species,coef)

results2_PF_S<-merge(results_PF_S_comb,coefs2_PF_S_comb,by=c('park','species', 'coef', 'term'), all.x=T, all.y=T)

results3_PF_S<- results2_PF_S %>% group_by(park,coef) %>% 
  mutate(rank=dense_rank(species))

results3b_PF_S<-results3_PF_S %>% filter(rank==1) %>% droplevels() %>% 
  mutate(est.corfac=estimate) %>% select(park,coef,est.corfac)

names(results3b_PF_S)

results4_PF_S<- merge(results3_PF_S, results3b_PF_S, by=c('park','coef'), all.x=T,all.y=T)

names(results4_PF_S)
results4_PF_S[,c(5,7,8,10)][is.na(results4_PF_S[,c(5,7,8,10)])]<-0
results4_PF_S$lower<-as.numeric(results4_PF_S$lower)
results4_PF_S$upper<-as.numeric(results4_PF_S$upper)

results5_PF_S<-results4_PF_S %>% 
  mutate(est.cor=ifelse(rank==1,est.corfac,est.corfac+estimate),
         lower.cor=ifelse(rank==1,lower,est.corfac+lower),
         upper.cor=ifelse(rank==1,upper,est.corfac+upper))

results_final_PF_S<-results5_PF_S %>% 
  mutate(estimate=round(est.cor,4),
         lower=round(lower.cor,4),
         upper=round(upper.cor,4), 
         sign=ifelse(lower>0 | upper<0,1,0)) %>% 
  select(park,species,coef,estimate,lower,upper,sign) 

View(results_final_PF_S)

write.csv(results_final_PF_S,'./results/results_PFreq-by_species-coefs.csv', row.names=F)


# Did not bootstrap for responses like other metrics. Ultimately only interested if the odds
# of a plot having an invasive increases over time, which we get at with the coefs. 