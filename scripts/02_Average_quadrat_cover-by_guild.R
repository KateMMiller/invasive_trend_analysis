#-----------------------------------
# Invasive trend analysis: Average Cover by Guild (AC_G)
#-----------------------------------
## ---- codesetup_AC_G ---- 
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
## ---- readdata_AC_G ----
#-----------------------------
# Read in data.frame without guids in first two columns
df<-read.csv("./data/NETN-MIDN-ERMN-NCRN_guild_invasives.csv")#[,-c(1,2)]
df<- df %>% arrange(park,plot_name,cycle,guild)

# only include guilds with at least 10% of plots with that guild
df1<-df %>% group_by(park,guild) %>% mutate(nonzero=sum(plot.freq,na.rm=T)/n()) %>% 
  filter((park!='ACAD'& nonzero>0.1)|(park=='ACAD'& guild=='Shrub')) %>% 
  droplevels() %>% ungroup(park,guild)

df2<-df1 %>% filter(!(network=='NCRN'& guild=='Tree') & (park!='SAHI') & (park!='WOTR')) %>% droplevels()

df_park<-df2 %>% group_by(park) %>% nest()

df_park<-df_park %>% mutate(data=map(data,
                    ~mutate(.x,nlev=length(unique(guild)))))

#-------------------------------
## ---- AC_G_diag ----
#-----------------------------------
analysis.title<-"Average % Invasive Cover by Guild"

# Model without transformation
avgcov.mod<-function(df) {
  if (df$nlev[1]>1) {lmer(avg.cover ~ cycle*guild + (1|plot_name),data=df)
  } else {lmer(avg.cover~cycle+(1|plot_name),data=df)} 
  } # random slope had singular fit, so went with simpler rand. intercept

prelim_by_park_AC_G<-df_park %>% mutate(model=map(data,avgcov.mod),
  resids=map2(data,model,add_residuals),pred=map2(data,model,add_predictions))

diag_AC_G<-unnest(prelim_by_park_AC_G, resids, pred)
res_AC_G<-residPlot(diag_AC_G)
hist_AC_G<-histPlot(diag_AC_G) # residuals are pretty wonky. 

# Check conversion
conv_AC_G<-unlist(prelim_by_park_AC_G[['model']]) %>% map('optinfo') %>% 
  map('conv') %>% map('opt') %>% data.frame() %>% gather() 

conv.tbl_AC_G<-data.frame(cbind(park=levels(diag_AC_G$park),conv.code=conv_AC_G$value))
conv.tbl_AC_G # all 0s. 

#-----------------------------------
##  ----  model_AC_G  ---- 
#-----------------------------------
# Average Invasive % Cover Results
#-----------------------------------
by_park_AC_G<-df_park %>% mutate(model=map(data,avgcov.mod),
  resids=map2(data,model,add_residuals),pred=map2(data,model,add_predictions))#,

# summarize model output
results_AC_G<-by_park_AC_G %>% mutate(summ=map(model,broom.mixed::tidy)) %>% 
  unnest(summ) %>%  filter(effect=='fixed') %>% 
  select(park,term,estimate) %>% arrange(park,term)

# reorder term factor, so can more easily associate the guilds with the terms, especially the reference term.
table(results_AC_G$term)
results_AC_G$term<-ordered(results_AC_G$term, 
  c("(Intercept)","cycle","guildHerbaceous","cycle:guildHerbaceous",
    "guildShrub","cycle:guildShrub","guildTree","cycle:guildTree")) 

results_AC_G<-results_AC_G %>% arrange(park,term) %>% 
  mutate(estimate=round(estimate,3))

# create guild labels, so we know what the first level for each model is.
guild_labels1_AC_G<-df2 %>% group_by(park,guild) %>% summarise(guild2=first(guild)) %>% 
  select(-guild) %>%  arrange(park,guild2) 

guild_labels2_AC_G<-data.frame(bind_rows(guild_labels1_AC_G,
  guild_labels1_AC_G) %>% arrange(park,guild2)) 

guild_labels2_AC_G$coef<-rep(c('Intercept','Slope'))

park_names2_AC_G<-rep(levels(df2$park),each=2) # make vector of park names

results_AC_G<-results_AC_G %>% mutate(guild=guild_labels2_AC_G$guild2,
  coef=ifelse(grepl('cycle',term),'Slope','Intercept'))

##  ---- model_results_AC_G ---- 
#-----------------------------------
# Create bootstrapped CIs on intercept and slopes
#-----------------------------------
by_park_coefs_AC_G<-by_park_AC_G %>% 
  mutate(conf.coef=map(model,~case_bootstrap(.x, fn=fixed_fun, B=1000, resample=c(TRUE,FALSE)))) %>% 
  select(conf.coef)  

coefs_AC_G<-by_park_coefs_AC_G %>% 
  mutate(bootCIs=map(conf.coef, ~bootCI(boot.t=.x$t))) %>% unnest(bootCIs) %>% 
  mutate(park=as.factor(park_names2_AC_G),
    type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything())

coefs2_AC_G<-coefs_AC_G %>% gather(term,coef,-park,-type,na.rm=T) %>% 
  spread(type,coef) %>% mutate(term=as.factor(term))

coefs2_AC_G$term<-ordered(coefs2_AC_G$term, 
  c("X.Intercept.","cycle","guildHerbaceous","cycle.guildHerbaceous",
    "guildShrub","cycle.guildShrub","guildTree","cycle.guildTree")) 

coefs2_AC_G<-coefs2_AC_G %>% arrange(park,term)

coefs3_AC_G<-data.frame(guild_labels2_AC_G,coefs2_AC_G[,-1]) # removes park, so no dups
names(coefs3_AC_G)[names(coefs3_AC_G)=='guild2']<-'guild'

results2_AC_G<-merge(results_AC_G,coefs3_AC_G,by=c('park','guild', 'coef'))

results3_AC_G<- results2_AC_G %>% group_by(park,coef) %>% 
  mutate(rank=dense_rank(guild))

results3b_AC_G<-results3_AC_G %>% filter(rank==1) %>% droplevels() %>% 
  mutate(est.corfac=estimate) %>% select(park,coef,est.corfac)

results4_AC_G<- merge(results3_AC_G, results3b_AC_G, by=c('park','coef'), all.x=T,all.y=T)

results5_AC_G<-results4_AC_G %>% 
  mutate(est.cor=ifelse(rank==1,est.corfac,est.corfac+estimate),
         lower.cor=ifelse(rank==1,lower,est.corfac+lower),
         upper.cor=ifelse(rank==1,upper,est.corfac+upper))

results_final_AC_G<-results5_AC_G %>% 
  mutate(estimate=round(est.cor,4),
    lower=round(lower.cor,4),
    upper=round(upper.cor,4), 
    sign=ifelse(lower>0 | upper<0,1,0)) %>% 
  select(park,guild,coef,estimate,lower,upper,sign) 
  
#View(results_final_AC_G)
write.csv(results_final_AC_G,'./results/results_avecov-by_guild-coefs_NP.csv', row.names=F)

##  ----  model_response_AC_G ---- 
#-----------------------------------
# Create bootstrapped CIs on response levels
#-----------------------------------
# Use parametric bootstrap to get confidence intervals around the response variable 
# for each cycle by guild level 
by_park_resp_AC_G<-by_park_AC_G %>% 
  mutate(conf.est=map(model,
    ~case_bootstrap(.x, fn=confFun, B=1000, resample=c(TRUE,FALSE))))

by_park_resp_AC_G<-by_park_resp_AC_G %>% mutate(cols=map(model,~getColNames(.x)), 
  boot.t=map2(conf.est,cols,~setColNames(.x,.y))) # make labels for output

resp_AC_G<-by_park_resp_AC_G %>% mutate(boot.ci=map(boot.t,~bootCI(.x))) %>% 
  select(boot.ci) %>% unnest() # Calculate 95% CIs from bootstrap output

resp_AC_G<-resp_AC_G %>% mutate(park=as.factor(park_names2_AC_G),
  type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything()) 
# puts labels on boot output

resp2_AC_G<-resp_AC_G %>% gather(gcyc,ci,c1:c3_Tree) %>% spread(type,ci) %>% na.omit()
# reshapes data so lower and upper CIs in separate columns
# CHECK COLUMN ORDER WHEN NEW PARKS ADDED.

resp_mean_AC_G<-by_park_resp_AC_G %>% 
  mutate(boot.mean=map(boot.t,~bootMean(.x))) %>% 
  select(boot.mean) %>% unnest()

labelsCI_AC_G<-df2 %>% na.omit() %>% group_by(park,cycle,guild) %>% 
  summarise(numplots=n(),lat.rank=first(lat.rank)) %>% droplevels() #na.omit removes NCRN shrubs and COLO C1

respCIs_AC_G<-data.frame(labelsCI_AC_G[,c('cycle','guild','numplots','lat.rank')],
  resp2_AC_G,resp_mean_AC_G) %>% select(park,everything())

colnames(respCIs_AC_G)<-c('park','cycle','guild','numplots','lat.rank','group','lower','upper','mean')

respCIs2_AC_G<-respCIs_AC_G %>% 
  mutate(cycle2=as.factor(cycle)) %>% arrange(park,guild,cycle)

slopes_AC_G<-results_final_AC_G %>% filter(coef=='Slope') %>% droplevels()

respCIs_final_AC_G<-merge(respCIs2_AC_G,
  slopes_AC_G[,c('park','guild','sign')],by=c('park','guild'),all.x=T)

respCIs_final_AC_G<-respCIs_final_AC_G %>% 
  mutate(sign=as.factor(sign),guildsign=as.factor(paste(guild,sign,sep='_')),guild=as.factor(guild),
    park=reorder(park,-lat.rank)) %>% 
  arrange(lat.rank,guild,cycle)

#View(respCIs_final_AC_G)
write.csv(respCIs_final_AC_G,"./results/results_avecov-by_guild-response_NP.csv")
