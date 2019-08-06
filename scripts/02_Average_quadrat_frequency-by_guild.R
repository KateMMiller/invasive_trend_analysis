#-----------------------------------
# Invasive trend analysis: Quadrat % Frequency by Guild (QF_G)
#-----------------------------------
## ---- codesetup_QF_G ---- 
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
## ---- readdata_QF_G ----
#-----------------------------
# Read in data.frame without guids in first two columns
df<-read.csv("./data/NETN-MIDN-ERMN-NCRN_guild_invasives.csv")#[,-c(1,2)]
df<- df %>% arrange(park,plot_name,cycle,guild)

# only include guilds with at least 10% of plots with that guild
df1<-df %>% group_by(park,guild) %>% mutate(nonzero=sum(plot.freq,na.rm=T)/n(),
                                            sumfreq=sum(qpct.freq)) %>% 
  filter((park!='ACAD'& nonzero>0.1 & sumfreq>0)|(park=='ACAD'& guild=='Shrub')) %>% 
  droplevels() %>% ungroup(park,guild)

View(df2)
df2<-df1 %>% filter(!(network=='NCRN'& guild=='Tree') & 
                      !(park=='SAHI') & (park!='WOTR')) %>% 
  filter(!(park=='MONO' & guild=='Shrub')) %>% droplevels() 
#had issues with MONO converging.

df_park<-df2 %>% group_by(park) %>% nest()

df_park<-df_park %>% mutate(data=map(data,
                                     ~mutate(.x,nlev=length(unique(guild)))))
#-------------------------------
## ---- QF_G_diag ----
#-----------------------------------
analysis.title<-"Quadrat % Invasive Frequency by Guild"

# Model without transformation
qfreq.mod<-function(df) {
  if (df$nlev[1]>1) {lmer(qpct.freq ~ cycle*guild + (1|plot_name),data=df)
  } else {lmer(qpct.freq~cycle+(1|plot_name),data=df)} 
} # random slope had singular fit, so went with simpler rand. intercept

prelim_by_park_QF_G<-df_park %>% mutate(model=map(data,qfreq.mod) %>% set_names(df_park$park),
                                        resids=map2(data,model,add_residuals) %>% set_names(df_park$park),
                                        pred=map2(data,model,add_predictions) %>% set_names(df_park$park))

diag_QF_G<-unnest(prelim_by_park_QF_G, resids, pred)
res_QF_G<-residPlot(diag_QF_G)
hist_QF_G<-histPlot(diag_QF_G) 

# Check conversion
conv_QF_G<-unlist(prelim_by_park_QF_G[['model']]) %>% map('optinfo') %>% 
  map('conv') %>% map('opt') %>% data.frame() %>% gather() 

conv_QF_G # all 0s. 

#-----------------------------------
##  ----  model_QF_G  ---- 
#-----------------------------------
# Quadrat % Frequency Results
#-----------------------------------
by_park_QF_G<-df_park %>% mutate(model=map(data,qfreq.mod) %>% set_names(df_park$park),
                                 resids=map2(data,model,add_residuals) %>% set_names(df_park$park),
                                 pred=map2(data,model,add_predictions) %>% set_names(df_park$park))#,

# summarize model output
results_QF_G<-by_park_QF_G %>% mutate(summ=map(model,broom.mixed::tidy)) %>% 
  unnest(summ) %>%  filter(effect=='fixed') %>% 
  select(park,term,estimate) %>% arrange(park,term)

# reorder term factor, so can more easily associate the guilds with the terms, especially the reference term.
table(results_QF_G$term)
results_QF_G$term<-ordered(results_QF_G$term, 
                           c("(Intercept)","cycle","guildHerbaceous","cycle:guildHerbaceous",
                             "guildShrub","cycle:guildShrub","guildTree","cycle:guildTree")) 

results_QF_G<-results_QF_G %>% arrange(park,term) %>% 
  mutate(estimate=round(estimate,3))

# create guild labels, so we know what the first level for each model is.
guild_labels1_QF_G<-df2 %>% group_by(park,guild) %>% summarise(guild2=first(guild)) %>% 
  select(-guild) %>%  arrange(park,guild2) 

guild_labels2_QF_G<-data.frame(bind_rows(guild_labels1_QF_G,
                                         guild_labels1_QF_G) %>% arrange(park,guild2)) 

guild_labels2_QF_G$coef<-rep(c('Intercept','Slope'))

park_names2_QF_G<-rep(levels(df2$park),each=2) # make vector of park names

results_QF_G<-results_QF_G %>% mutate(guild=guild_labels2_QF_G$guild2,
                                      coef=ifelse(grepl('cycle',term),'Slope','Intercept'))

##  ---- model_results_QF_G ---- 
#-----------------------------------
# Create bootstrapped CIs on intercept and slopes
#-----------------------------------
by_park_coefs_QF_G<-by_park_QF_G %>% 
  mutate(conf.coef=map(model,~case_bootstrap(.x, fn=fixed_fun, B=1000, 
                                             resample=c(TRUE,FALSE))) %>% set_names(df_park$park)) %>% 
  select(conf.coef)   

coefs_QF_G<-by_park_coefs_QF_G %>% 
  mutate(bootCIs=map(conf.coef, ~bootCI(boot.t=.x$t))) %>% unnest(bootCIs) %>% 
  mutate(park=as.factor(park_names2_QF_G),
         type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything())

coefs2_QF_G<-coefs_QF_G %>% gather(term,coef,-park,-type,na.rm=T) %>% 
  spread(type,coef) %>% mutate(term=as.factor(term))

coefs2_QF_G$term<-ordered(coefs2_QF_G$term, 
                          c("X.Intercept.","cycle","guildHerbaceous","cycle.guildHerbaceous",
                            "guildShrub","cycle.guildShrub","guildTree","cycle.guildTree")) 

coefs2_QF_G<-coefs2_QF_G %>% arrange(park,term)
coefs3_QF_G<-data.frame(guild_labels2_QF_G,coefs2_QF_G[,-1]) # removes park, so no dups
names(coefs3_QF_G)[names(coefs3_QF_G)=='guild2']<-'guild'

results2_QF_G<-merge(results_QF_G,coefs3_QF_G,by=c('park','guild', 'coef'))

results3_QF_G<- results2_QF_G %>% group_by(park,coef) %>% 
  mutate(rank=dense_rank(guild))

results3b_QF_G<-results3_QF_G %>% filter(rank==1) %>% droplevels() %>% 
  mutate(est.corfQF=estimate) %>% select(park,coef,est.corfQF)

results4_QF_G<- merge(results3_QF_G, results3b_QF_G, by=c('park','coef'), all.x=T,all.y=T)

results5_QF_G<-results4_QF_G %>% 
  mutate(est.cor=ifelse(rank==1,est.corfQF,est.corfQF+estimate),
         lower.cor=ifelse(rank==1,lower,est.corfQF+lower),
         upper.cor=ifelse(rank==1,upper,est.corfQF+upper))

results_final_QF_G<-results5_QF_G %>% 
  mutate(estimate=round(est.cor,4),
         lower=round(lower.cor,4),
         upper=round(upper.cor,4), 
         sign=ifelse(lower>0 | upper<0,1,0)) %>% 
  select(park,guild,coef,estimate,lower,upper,sign) 

write.csv(results_final_QF_G,'./results/results_qfreq-by_guild-coefs_NP.csv', row.names=F)
#View(results_final_QF_G)

##  ----  model_response_QF_G ---- 
#-----------------------------------
# Create bootstrapped CIs on response levels
#-----------------------------------
# Use parametric bootstrap to get confidence intervals around the response variable 
# for each cycle by guild level 
by_park_resp_QF_G<-by_park_QF_G %>% 
  mutate(conf.est=map(model,
                      ~case_bootstrap(.x, fn=confFun, B=1000, resample=c(TRUE,FALSE))) %>% set_names(df_park$park))

by_park_resp_QF_G<-by_park_resp_QF_G %>% mutate(cols=map(model,~getColNames(.x)), 
                                                boot.t=map2(conf.est,cols,~setColNames(.x,.y))) # make labels for output

resp_QF_G<-by_park_resp_QF_G %>% mutate(boot.ci=map(boot.t,~bootCI(.x))) %>% 
  select(boot.ci) %>% unnest() # Calculate 95% CIs from bootstrap output

resp_QF_G<-resp_QF_G %>% mutate(park=as.factor(park_names2_QF_G),
                                type=rep(c('lower','upper'),times=length(levels(park)))) %>% 
  select(park,type,everything()) 

# puts labels on boot output
resp2_QF_G<-resp_QF_G %>% gather(gcyc,ci,c1:c3_Tree) %>% spread(type,ci) %>% na.omit()

# reshapes data so lower and upper CIs in separate columns
# CHECK COLUMN ORDER WHEN NEW PARKS ADDED.
resp_mean_QF_G<-by_park_resp_QF_G %>% 
  mutate(boot.mean=map(boot.t,~bootMean(.x))) %>% 
  select(boot.mean) %>% unnest()

labelsCI_QF_G<-df2 %>% filter(!(park=='COLO'& cycle==1)) %>%  group_by(park,cycle,guild) %>% 
  summarise(numplots=n(),lat.rank=first(lat.rank)) %>% droplevels()

respCIs_QF_G<-data.frame(labelsCI_QF_G[,c('cycle','guild','numplots','lat.rank')],
                         resp2_QF_G,resp_mean_QF_G) %>% select(park,everything())

colnames(respCIs_QF_G)<-c('park','cycle','guild','numplots','lat.rank','group','lower','upper','mean')

respCIs2_QF_G<-respCIs_QF_G %>% 
  mutate(cycle2=as.factor(cycle)) %>% arrange(park,guild,cycle)

slopes_QF_G<-results_final_QF_G %>% filter(coef=='Slope') %>% droplevels()

respCIs_final_QF_G<-merge(respCIs2_QF_G,
                          slopes_QF_G[,c('park','guild','sign')],by=c('park','guild'),all.x=T)

respCIs_final_QF_G<-respCIs_final_QF_G %>% 
  mutate(sign=as.factor(sign),guildsign=as.factor(paste(guild,sign,sep='_')),guild=as.factor(guild),
         park=reorder(park,-lat.rank)) %>% 
  arrange(lat.rank,guild,cycle)

#View(respCIs_final_QF_G)

write.csv(respCIs_final_QF_G,"./results/results_qfreq-by_guild-response_NP.csv")
