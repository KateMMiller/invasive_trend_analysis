#-----------------------------------
# Invasive trend analysis: Plot Frequency by Guild (PF_G)
#-----------------------------------
## ---- codesetup_PF_G ---- 
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
## ---- readdata_PF_G ----
#-----------------------------
# Read in data.frame without guids in first two columns
df<-read.csv("./data/NETN-MIDN-ERMN-NCRN_guild_invasives.csv")#[,-c(1,2)]
df<- df %>% arrange(park,plot_name,cycle,guild)
df2<-na.omit(df)
# only include guilds with at least 10% of plots with that guild and <90% of all
df3<-df2 %>% group_by(park,guild) %>% mutate(nonzero=sum(plot.freq,na.rm=T)/n()) %>% 
  filter((park!='ACAD'& between(nonzero,0.1,0.9))|(park=='ACAD'& guild=='Shrub')) %>% 
  droplevels() %>% ungroup(park,guild) 

df4<-df3 %>% filter(park!='SAHI' & park!="WOTR") %>% droplevels()

# FRHI Herbaceous, MIMA Shrub and SAHI Shrub are 100% present the whole time.
# Several other park/guild combinations are close to 100%, which causes problems in glmer
# Had to remove guilds that were 100 plot frequency in all cycles 
df_park<-df4 %>% group_by(park) %>% nest()

df_park<-df_park %>% mutate(data=map(data,
                    ~mutate(.x,nlev=length(unique(guild)))))
#-------------------------------
## ---- PF_G_diag ----
#-----------------------------------
analysis.title<-"Invasive Plot Frequency by Guild"

# Model without transformation
PFreq.mod<-function(df) {
  if (df$nlev[1]>1) {glmer(plot.freq ~ cycle*guild + (1|plot_name), 
    family=binomial, control=glmerCtlList, data=df)
  } else {glmer(plot.freq~cycle+(1|plot_name), family=binomial, 
    control=glmerCtlList,data=df)} 
  } # random slope had singular fit, so went with simpler rand. intercept

prelim_by_park_PF_G<-df_park %>% mutate(model=map(data,PFreq.mod),
  resids=map2(data,model,add_residuals),pred=map2(data,model,add_predictions))

# Check conversion
conv_PF_G<-unlist(prelim_by_park_PF_G[['model']]) %>% map('optinfo') %>% 
  map('conv') %>% map('opt') %>% data.frame() %>% gather() 

conv.tbl_PF_G<-data.frame(cbind(park=levels(diag_PF_G$park),conv.code=conv_PF_G$value))
conv.tbl_PF_G # all 0s. 

#-----------------------------------
##  ----  model_PF_G  ---- 
#-----------------------------------
# Plot Frequency Results
#-----------------------------------
by_park_PF_G<-df_park %>% mutate(model=map(data,PFreq.mod),
  resids=map2(data,model,add_residuals),pred=map2(data,model,add_predictions))#,

# summarize model output
results_PF_G<-by_park_PF_G %>% mutate(summ=map(model,broom.mixed::tidy)) %>% 
  unnest(summ) %>%  filter(effect=='fixed') %>% 
  select(park,term,estimate,std.error) %>% arrange(park,term)

# reorder term factor, so can more easily associate the guilds with the terms, especially the reference term.
table(results_PF_G$term)
results_PF_G$term<-ordered(results_PF_G$term, 
  c("(Intercept)","cycle","guildHerbaceous","cycle:guildHerbaceous",
    "guildShrub","cycle:guildShrub","guildTree","cycle:guildTree")) 

results_PF_G<-results_PF_G %>% arrange(park,term) %>% 
  mutate(estimate=round(estimate,3),std.error=round(std.error,3))

# create guild labels, so we know what the first level for each model is.
guild_labels1_PF_G<-df4 %>% group_by(park,guild) %>% summarise(guild2=first(guild)) %>% 
  select(-guild) %>%  arrange(park,guild2) 

guild_labels2_PF_G<-data.frame(bind_rows(guild_labels1_PF_G,
  guild_labels1_PF_G) %>% arrange(park,guild2)) 

guild_labels2_PF_G$coef<-rep(c('Intercept','Slope'))

park_names2_PF_G<-rep(levels(df4$park),each=2) # make vector of park names

results_PF_G<-results_PF_G %>% mutate(guild=guild_labels2_PF_G$guild2,
  coef=ifelse(grepl('cycle',term),'Slope','Intercept'))

##  ---- model_results_PF_G ---- 
#-----------------------------------
# Create bootstrapped CIs on intercept and slopes
#-----------------------------------
by_park_coefs_PF_G<-by_park_PF_G %>% 
  mutate(conf.coef=map(model,~bootMer(.x,FUN=fixed_fun,nsim=1000, parallel='snow',
    ncpus=11))) %>% 
  select(conf.coef)  

coefs_PF_G<-by_park_coefs_PF_G %>% 
  mutate(bootCIs=map(conf.coef, ~bootCI(boot.t=.x$t))) %>% unnest(bootCIs) %>% 
  mutate(park=as.factor(park_names2_PF_G),
    type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything())

coefs2_PF_G<-coefs_PF_G %>% gather(term,coef,-park,-type,na.rm=T) %>% 
  spread(type,coef) %>% mutate(term=as.factor(term))

coefs2_PF_G$term<-ordered(coefs2_PF_G$term, 
  c("X.Intercept.","cycle","guildHerbaceous","cycle.guildHerbaceous",
    "guildShrub","cycle.guildShrub","guildTree","cycle.guildTree")) 

coefs2_PF_G<-coefs2_PF_G %>% arrange(park,term)
coefs3_PF_G<-data.frame(guild_labels2_PF_G,coefs2_PF_G[,-1]) # removes park, so no dups
names(coefs3_PF_G)[names(coefs3_PF_G)=='guild2']<-'guild'

results2_PF_G<-merge(results_PF_G,coefs3_PF_G,by=c('park','guild', 'coef'))

results3_PF_G<- results2_PF_G %>% group_by(park,coef) %>% 
  mutate(rank=dense_rank(guild))

results3b_PF_G<-results3_PF_G %>% filter(rank==1) %>% droplevels() %>% 
  mutate(est.corfPF=estimate) %>% select(park,coef,est.corfPF)

results4_PF_G<- merge(results3_PF_G, results3b_PF_G, by=c('park','coef'), all.x=T,all.y=T)
results5_PF_G<-results4_PF_G %>% 
  mutate(est.cor=ifelse(rank==1,est.corfPF,est.corfPF+estimate),
         lower.cor=ifelse(rank==1,lower,est.corfPF+lower),
         upper.cor=ifelse(rank==1,upper,est.corfPF+upper))

results_final_PF_G<-results5_PF_G %>% 
  mutate(estimate=round(est.cor,4),
         lower=round(lower.cor,4),
         upper=round(upper.cor,4), 
         sign=ifelse(lower>0 | upper<0,1,0)) %>% 
  select(park,guild,coef,estimate,lower,upper,sign) 

write.csv(results_final_PF_G,'./results/results_PFreq-by_guild-coefs.csv', row.names=F)

#View(results_final_PF_G)

# Did not bootstrap for responses like other metrics. Ultimately only interested if the odds
# of a plot having an invasive increases over time, which we get at with the coefs. 