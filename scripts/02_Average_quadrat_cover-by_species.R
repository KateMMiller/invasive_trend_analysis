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
df<-read.csv("./data/NETN-MIDN_species_invasives.csv")#[,-c(1,2)]
df<- df %>% arrange(park,plot_name,cycle,species) %>% filter(species!='noinvspp')
head(df)

# only include species with at least 10% of plots with that guild
df1<-df %>% group_by(park,species) %>% mutate(nonzero=sum(plot.freq,na.rm=T)/n()) %>% 
  filter((park!='ACAD'& nonzero>0.1)|(park=='ACAD'& species=='Rhamnus frangula')) %>% 
  filter(park!='SAHI') %>% 
  droplevels() %>% ungroup(park,species)
table(df1$species,df1$park)
#df2<-df1 %>% filter(!(network=='NCRN'& guild=='Tree') & (park!='SAHI') & (park!='WOTR')) %>% droplevels()

df_park<-df1 %>% group_by(park) %>% nest()

df_park<-df_park %>% mutate(data=map(data,
                    ~mutate(.x,nlev=length(unique(species)))))

#-------------------------------
## ---- AC_S_diag ----
#-----------------------------------
analysis.title<-"Average % Invasive Cover by Species"

# Model without transformation
avgcov.mod<-function(df) {
  if (df$nlev[1]>1) {lmer(avg.cover ~ cycle*species + (1|plot_name),data=df)
  } else {lmer(avg.cover~cycle+(1|plot_name),data=df)} 
  } # random slope had singular fit, so went with simpler rand. intercept

prelim_by_park_AC_S<-df_park %>% mutate(model=map(data,avgcov.mod),
  resids=map2(data,model,add_residuals),pred=map2(data,model,add_predictions))

diag_AC_S<-unnest(prelim_by_park_AC_S, resids, pred)
res_AC_S<-residPlot(diag_AC_S)
hist_AC_S<-histPlot(diag_AC_S) # residuals are pretty wonky. 

# Check conversion
conv_AC_S<-unlist(prelim_by_park_AC_S[['model']]) %>% map('optinfo') %>% 
  map('conv') %>% map('opt') %>% data.frame() %>% gather() 

conv.tbl_AC_S<-data.frame(cbind(park=levels(diag_AC_S$park),conv.code=conv_AC_S$value))
conv.tbl_AC_S # all 0s. 

#-----------------------------------
##  ----  model_AC_S  ---- 
#-----------------------------------
# Average Invasive % Cover Results
#-----------------------------------
by_park_AC_S<-df_park %>% mutate(model=map(data,avgcov.mod),
  resids=map2(data,model,add_residuals),pred=map2(data,model,add_predictions))#,

# summarize model output
results_AC_S<-by_park_AC_S %>% mutate(summ=map(model,broom.mixed::tidy)) %>% 
  unnest(summ) %>%  filter(effect=='fixed') %>% 
  select(park,term,estimate) %>% arrange(park,term)

# reorder term factor, so can more easily associate the guilds with the terms, especially the reference term.
terms<-data.frame(table(results_AC_S$term))
terms
#+++++++++++++++++++++++++++++++++++++
# HAVE TO CHECK NEXT LINE WHEN I ADD NEW DATA
#+++++++++++++++++++++++++++++++++++++
results_AC_S$term<-ordered(results_AC_S$term, 
  c("(Intercept)", "cycle", "speciesAilanthus altissima", "cycle:speciesAilanthus altissima", "speciesAlliaria petiolata",
    "cycle:speciesAlliaria petiolata", "speciesBerberis thunbergii", "cycle:speciesBerberis thunbergii", 
    "speciesCardamine impatiens", "cycle:speciesCardamine impatiens", "speciesCelastrus orbiculatus",
    "cycle:speciesCelastrus orbiculatus", "speciesElaeagnus", "cycle:speciesElaeagnus", "speciesEuonymus", 
    "cycle:speciesEuonymus", "speciesEuonymus fortunei", "cycle:speciesEuonymus fortunei", "speciesHedera helix",
    "cycle:speciesHedera helix", "speciesLigustrum", "cycle:speciesLigustrum", "speciesLonicera - Exotic", 
    "cycle:speciesLonicera - Exotic", "speciesLonicera japonica", "cycle:speciesLonicera japonica", 
    "speciesMicrostegium vimineum", "cycle:speciesMicrostegium vimineum", "speciesPersicaria perfoliata", 
    "cycle:speciesPersicaria perfoliata", "speciesPhotinia villosa", "cycle:speciesPhotinia villosa", 
    "speciesRhamnus cathartica", "cycle:speciesRhamnus cathartica", "speciesRhamnus frangula", "cycle:speciesRhamnus frangula",
    "speciesRobinia pseudoacacia", "cycle:speciesRobinia pseudoacacia", "speciesRosa multiflora", "cycle:speciesRosa multiflora",
    "speciesRubus phoenicolasius", "cycle:speciesRubus phoenicolasius", "speciesViburnum dilatatum", 
    "cycle:speciesViburnum dilatatum", "speciesVincetoxicum", "cycle:speciesVincetoxicum")) 

results_AC_S<-results_AC_S %>% arrange(park,term) %>% 
  mutate(estimate=round(estimate,3))

# create species labels, so we know what the first level for each model is.
species_labels1_AC_S<-df1 %>% group_by(park,species) %>% summarise(species2=first(species)) %>% 
  select(-species) %>%  arrange(park,species2) 

species_labels2_AC_S<-data.frame(bind_rows(species_labels1_AC_S,
  species_labels1_AC_S) %>% arrange(park,species2)) 

species_labels2_AC_S$coef<-rep(c('Intercept','Slope'))

park_names2_AC_S<-rep(levels(df1$park),each=2) # make vector of park names

results_AC_S<-results_AC_S %>% mutate(species=species_labels2_AC_S$species2,
  coef=ifelse(grepl('cycle',term),'Slope','Intercept'))

##  ---- model_results_AC_S ---- 
#-----------------------------------
# Create bootstrapped CIs on intercept and slopes
#-----------------------------------
by_park_coefs_AC_S<-by_park_AC_S %>% 
  mutate(conf.coef=map(model,~case_bootstrap(.x, fn=fixed_fun, B=1000, resample=c(TRUE,FALSE)))) %>% 
  select(conf.coef)  

coefs_AC_S<-by_park_coefs_AC_S %>% 
  mutate(bootCIs=map(conf.coef, ~bootCI(boot.t=.x$t))) %>% unnest(bootCIs) %>% 
  mutate(park=as.factor(park_names2_AC_S),
    type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything())

coefs2_AC_S<-coefs_AC_S %>% gather(term,coef,-park,-type,na.rm=T) %>% 
  spread(type,coef) %>% mutate(term=as.factor(term))

terms2<-data.frame(table(coefs2_AC_S$term))
terms2
coefs2_AC_S$term<-ordered(coefs2_AC_S$term, 
  c("X.Intercept.", "cycle", "speciesAilanthus.altissima", "cycle.speciesAilanthus.altissima", "speciesAlliaria.petiolata", 
    "cycle.speciesAlliaria.petiolata", "speciesBerberis.thunbergii", "cycle.speciesBerberis.thunbergii", 
    "speciesCardamine.impatiens", "cycle.speciesCardamine.impatiens", "speciesCelastrus.orbiculatus", 
    "cycle.speciesCelastrus.orbiculatus", "speciesElaeagnus", "cycle.speciesElaeagnus", "speciesEuonymus", 
    "cycle.speciesEuonymus", "speciesEuonymus.fortunei", "cycle.speciesEuonymus.fortunei", "speciesHedera.helix", 
    "cycle.speciesHedera.helix", "speciesLigustrum", "cycle.speciesLigustrum", "speciesLonicera...Exotic", 
    "cycle.speciesLonicera...Exotic", "speciesLonicera.japonica", "cycle.speciesLonicera.japonica", 
    "speciesMicrostegium.vimineum", "cycle.speciesMicrostegium.vimineum", "speciesPersicaria.perfoliata", 
    "cycle.speciesPersicaria.perfoliata", "speciesPhotinia.villosa", "cycle.speciesPhotinia.villosa", "speciesRhamnus.cathartica",
    "cycle.speciesRhamnus.cathartica", "speciesRhamnus.frangula", "cycle.speciesRhamnus.frangula", "speciesRobinia.pseudoacacia", 
    "cycle.speciesRobinia.pseudoacacia", "speciesRosa.multiflora", "cycle.speciesRosa.multiflora", 
    "speciesRubus.phoenicolasius", "cycle.speciesRubus.phoenicolasius", "speciesViburnum.dilatatum", 
    "cycle.speciesViburnum.dilatatum", "speciesVincetoxicum", "cycle.speciesVincetoxicum" )) 

coefs2_AC_S<-coefs2_AC_S %>% arrange(park,term)

coefs3_AC_S<-data.frame(species_labels2_AC_S,coefs2_AC_S[,-1]) # removes park, so no dups
names(coefs3_AC_S)[names(coefs3_AC_S)=='species2']<-'species'

results2_AC_S<-merge(results_AC_S,coefs3_AC_S,by=c('park','species', 'coef'))

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
# Use parametric bootstrap to get confidence intervals around the response variable 
# for each cycle by guild level 
by_park_resp_AC_S<-by_park_AC_S %>% 
  mutate(conf.est=map(model,
    ~case_bootstrap(.x, fn=confFunSpp, B=1000, resample=c(TRUE,FALSE))))

by_park_resp_AC_S<-by_park_resp_AC_S %>% mutate(cols=map(model,~getColNamesSpp(.x)), 
  boot.t=map2(conf.est,cols,~setColNames(.x,.y))) # make labels for output

resp_AC_S<-by_park_resp_AC_S %>% mutate(boot.ci=map(boot.t,~bootCI(.x))) %>% 
  select(boot.ci) %>% unnest() # Calculate 95% CIs from bootstrap output

resp_AC_S<-resp_AC_S %>% mutate(park=as.factor(park_names2_AC_S),
  type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything()) 
# puts labels on boot output
names(resp_AC_S)

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