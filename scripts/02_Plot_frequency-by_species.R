#-----------------------------------
# Invasive trend analysis: Plot Frequency by Species (PF_S)
#-----------------------------------
## ---- codesetup_PF_S ---- 
#-----------------------------------
library(tidyverse) # attaches most of the important packages in the tidyverse
library(lme4) # for glmer with Poisson
library(modelr) #for handling multiple models in tidyverse
library(broom.mixed)# for better model summary tables than default in nlme
library(prediction) # for find_data(model) function

options("scipen"=100, "digits"=4) # keeps TSN numbers as numbers 

source('./scripts/functions_for_ANALYSIS.R') # File containing functions

glmerCtlList <- glmerControl(optimizer=c("bobyqa","Nelder_Mead"),
  optCtrl=list(maxfun=2e10000)) # set controls for glmer so longer before times out

#-----------------------------------
## ---- readdata_PF_S ----
#-----------------------------
# Read in data.frame without guids in first two columns
df<-read.csv("./data/NETN-MIDN-ERMN-NCRN_species_invasives.csv")#[,-c(1,2)]
df<- df %>% arrange(park,plot_name,cycle,species)
df2<-na.omit(df)
# only include speciess with at least 10% of plots with that species and <90% of all
df3<-df2 %>% group_by(park,species) %>% mutate(nonzero=sum(plot.freq,na.rm=T)/n()) %>% 
  filter((park!='ACAD'& between(nonzero,0.1,0.9))|(park=='ACAD'& species=='Shrub')) %>% 
  droplevels() %>% ungroup(park,species) 

df4<-df3 %>% filter(park!='SAHI' & park!="WOTR") %>% droplevels()

# FRHI Herbaceous, MIMA Shrub and SAHI Shrub are 100% present the whole time.
# Several other park/species combinations are close to 100%, which causes problems in glmer
# Had to remove speciess that were 100 plot frequency in all cycles 
df_park<-df4 %>% group_by(park) %>% nest()

df_park<-df_park %>% mutate(data=map(data,
                    ~mutate(.x,nlev=length(unique(species)))))
#-------------------------------
## ---- PF_S_diag ----
#-----------------------------------
analysis.title<-"Invasive Plot Frequency by Species"

# Model without transformation
PFreq.mod<-function(df) {
  if (df$nlev[1]>1) {glmer(plot.freq ~ cycle*species + (1|plot_name), 
    family=binomial, control=glmerCtlList, data=df)
  } else {glmer(plot.freq~cycle+(1|plot_name), family=binomial, 
    control=glmerCtlList,data=df)} 
  } # random slope had singular fit, so went with simpler rand. intercept

prelim_by_park_PF_S<-df_park %>% mutate(model=map(data,PFreq.mod),
  resids=map2(data,model,add_residuals),pred=map2(data,model,add_predictions))

# Check conversion
conv_PF_S<-unlist(prelim_by_park_PF_S[['model']]) %>% map('optinfo') %>% 
  map('conv') %>% map('opt') %>% data.frame() %>% gather() 

conv.tbl_PF_S<-data.frame(cbind(park=levels(diag_PF_S$park),conv.code=conv_PF_S$value))
conv.tbl_PF_S # all 0s. 

#-----------------------------------
##  ----  model_PF_S  ---- 
#-----------------------------------
# Plot Frequency Results
#-----------------------------------
by_park_PF_S<-df_park %>% mutate(model=map(data,PFreq.mod),
  resids=map2(data,model,add_residuals),pred=map2(data,model,add_predictions))#,

# summarize model output
results_PF_S<-by_park_PF_S %>% mutate(summ=map(model,broom.mixed::tidy)) %>% 
  unnest(summ) %>%  filter(effect=='fixed') %>% 
  select(park,term,estimate,std.error) %>% arrange(park,term)

# reorder term factor, so can more easily associate the speciess with the terms, especially the reference term.
table(results_PF_S$term)
results_PF_S$term<-ordered(results_PF_S$term, 
  c("(Intercept)","cycle","speciesHerbaceous","cycle:speciesHerbaceous",
    "speciesShrub","cycle:speciesShrub","speciesTree","cycle:speciesTree")) 

results_PF_S<-results_PF_S %>% arrange(park,term) %>% 
  mutate(estimate=round(estimate,3),std.error=round(std.error,3))

# create species labels, so we know what the first level for each model is.
species_labels1_PF_S<-df4 %>% group_by(park,species) %>% summarise(species2=first(species)) %>% 
  select(-species) %>%  arrange(park,species2) 

species_labels2_PF_S<-data.frame(bind_rows(species_labels1_PF_S,
  species_labels1_PF_S) %>% arrange(park,species2)) 

species_labels2_PF_S$coef<-rep(c('Intercept','Slope'))

park_names2_PF_S<-rep(levels(df4$park),each=2) # make vector of park names

results_PF_S<-results_PF_S %>% mutate(species=species_labels2_PF_S$species2,
  coef=ifelse(grepl('cycle',term),'Slope','Intercept'))

##  ---- model_results_PF_S ---- 
#-----------------------------------
# Create bootstrapped CIs on intercept and slopes
#-----------------------------------
by_park_coefs_PF_S<-by_park_PF_S %>% 
  mutate(conf.coef=map(model,~bootMer(.x,FUN=fixed_fun,nsim=1000, parallel='snow',
    ncpus=11))) %>% 
  select(conf.coef)  

coefs_PF_S<-by_park_coefs_PF_S %>% 
  mutate(bootCIs=map(conf.coef, ~bootCI(boot.t=.x$t))) %>% unnest(bootCIs) %>% 
  mutate(park=as.factor(park_names2_PF_S),
    type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything())

coefs2_PF_S<-coefs_PF_S %>% gather(term,coef,-park,-type,na.rm=T) %>% 
  spread(type,coef) %>% mutate(term=as.factor(term))

coefs2_PF_S$term<-ordered(coefs2_PF_S$term, 
  c("X.Intercept.","cycle","speciesHerbaceous","cycle.speciesHerbaceous",
    "speciesShrub","cycle.speciesShrub","speciesTree","cycle.speciesTree")) 

coefs2_PF_S<-coefs2_PF_S %>% arrange(park,term)
coefs3_PF_S<-data.frame(species_labels2_PF_S,coefs2_PF_S[,-1]) # removes park, so no dups
names(coefs3_PF_S)[names(coefs3_PF_S)=='species2']<-'species'

results2_PF_S<-merge(results_PF_S,coefs3_PF_S,by=c('park','species', 'coef'))

results3_PF_S<- results2_PF_S %>% group_by(park,coef) %>% 
  mutate(rank=dense_rank(species))

results3b_PF_S<-results3_PF_S %>% filter(rank==1) %>% droplevels() %>% 
  mutate(est.corfPF=estimate) %>% select(park,coef,est.corfPF)

results4_PF_S<- merge(results3_PF_S, results3b_PF_S, by=c('park','coef'), all.x=T,all.y=T)
results5_PF_S<-results4_PF_S %>% 
  mutate(est.cor=ifelse(rank==1,est.corfPF,est.corfPF+estimate),
         lower.cor=ifelse(rank==1,lower,est.corfPF+lower),
         upper.cor=ifelse(rank==1,upper,est.corfPF+upper))

results_final_PF_S<-results5_PF_S %>% 
  mutate(estimate=round(est.cor,4),
         lower=round(lower.cor,4),
         upper=round(upper.cor,4), 
         sign=ifelse(lower>0 | upper<0,1,0)) %>% 
  select(park,species,coef,estimate,lower,upper,sign) 

write.csv(results_final_PF_S,'./results/results_PFreq-by_species-coefs.csv', row.names=F)

#View(results_final_PF_S)

# Did not bootstrap for responses like other metrics. Ultimately only interested if the odds
# of a plot having an invasive increases over time, which we get at with the coefs. 