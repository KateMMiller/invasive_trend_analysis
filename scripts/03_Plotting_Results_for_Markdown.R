#-----------------------------------------------
# Invasive trend analysis: Plotting Results
#-----------------------------------------------
library(tidyverse)
library(cowplot)

# Since these are sourced in R Markdown, I need ../ to go back a folder level
# source('./scripts/functions_for_PLOTTING.R') # only turn this on, if I'm using outside 
# of R Markdown file

addC1_COLO<-function(df){
  colo<-df %>% filter(park=='COLO', cycle==2) %>% droplevels()
  colo1<-colo %>% mutate(cycle=1,numplots=0,group='c1',lower=NA,upper=NA,mean=NA,cycle2=1)
    df<-rbind(df,colo1)}

#++++++++++++++++++++++++++++++++++
# Loading tables of coefficients 
#++++++++++++++++++++++++++++++++++
avgcov_total_coefs<-read.csv("../results/results_avecov-total-coefs_NP.csv")
sign_inc_AT<-avgcov_total_coefs %>% filter(coef=='Slope' & sign==1 & estimate>0) %>% 
  summarise(numsign=as.character(sum(sign))) 
sign_inc_AT

sign_dec_AT<-avgcov_total_coefs %>% filter(coef=='Slope' & sign==1 & estimate<0) %>% 
  summarise(numsign=as.character(sum(sign)))
sign_dec_AT

qfreq_total_coefs<-read.csv("../results/results_qfreq-total-coefs_NP.csv")
sign_inc_QF<-qfreq_total_coefs %>% filter(coef=='Slope' & sign==1 & estimate>0) %>% 
  summarise(numsign=as.character(sum(sign))) 
sign_inc_QF

sign_dec_QF<-qfreq_total_coefs %>% filter(coef=='Slope' & sign==1 & estimate<0) %>% 
  summarise(numsign=as.character(sum(sign))) 
sign_dec_QF

qrich_total_coefs<-read.csv("../results/results_qrich-total-coefs_NP.csv")
sign_inc_QR<-qrich_total_coefs %>% filter(coef=='Slope' & sign==1 & estimate>0) %>% 
  summarise(numsign=as.character(sum(sign))) 
sign_inc_QR

sign_dec_QR<-qrich_total_coefs %>% filter(coef=='Slope' & sign==1 & estimate<0) %>% 
  summarise(numsign=as.character(sum(sign))) 
sign_dec_QR
#-------- Plot frequency takes more work, b/c have to add non-modeled parks back in
pfreq_total_coefs<-read.csv('../results/results_PFreq-total-coefs_NP.csv')
pfreq_total_slopes<-pfreq_total_coefs %>% filter(coef=='Slope') %>% droplevels()

dft<-read.csv("../data/NETN-MIDN-ERMN-NCRN_total_invasives.csv")
dft_pf<-dft %>% arrange(park,plot_name,cycle) %>% select(park,plot_name,cycle,lat.rank,plot.freq) %>% 
  filter(cycle==3)
dft_pf2<-dft_pf %>% group_by(park,cycle) %>% summarise(plot.freq=sum(plot.freq,na.rm=T),num.plots=n(),
  pfreq=round(((plot.freq/num.plots)*100),2),lat.rank=first(lat.rank))

pfreq_total_comb<-merge(dft_pf2,pfreq_total_slopes, by=c('park'),all.x=T)
pfreq_total_comb$sign[is.na(pfreq_total_comb$sign)]<-0

pfreq_total<- pfreq_total_comb %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle),sign=as.numeric(sign), park=reorder(park, -lat.rank))
pfreq_total$sign[is.na(pfreq_total$sign)]<-0

sign_inc_PF<-pfreq_total %>% filter(coef=='Slope' & sign==1 & estimate>0) %>% 
  summarise(numsign=as.character(sum(sign, na.rm=T))) 

sign_inc_PF

sign_dec_PF<-pfreq_total %>% filter(coef=='Slope' & sign==1 & estimate<0) %>% 
  summarise(numsign=as.character(sum(sign))) 

sign_dec_PF

#------
avgcov_guild_coefs<-read.csv("../results/results_avecov-by_guild-coefs_NP.csv")
qfreq_guild_coefs<-read.csv("../results/results_qfreq-by_guild-coefs_NP.csv")
qrich_guild_coefs<-read.csv("../results/results_qrich-by_guild-coefs_NP.csv")

# For park-level plot % frequency
# Some parks weren't modeled because either all 1 or all 0.
pfreq_guild_coefs<-read.csv('../results/results_pfreq-by_guild-coefs.csv')
pfreq_guild_slopes<-pfreq_guild_coefs %>% filter(coef=='Slope') %>% droplevels()

df<-read.csv("../data/NETN-MIDN-ERMN-NCRN_guild_invasives.csv")#[,-c(1,2)]
df_pf<- df %>% arrange(park,plot_name,cycle,guild) %>% select(park,plot_name,cycle,guild,lat.rank,plot.freq)
df_pf2<-df_pf %>% group_by(park,guild,cycle) %>% 
  summarise(plot.freq=sum(plot.freq), num.plots=n(),pfreq=round(((plot.freq/num.plots)*100),2), lat.rank=first(lat.rank))
head(df_pf2)

pfreq_guild_comb<-merge(df_pf2,pfreq_guild_slopes, by=c('park','guild'),all.x=T)
pfreq_guild_comb$sign[is.na(pfreq_guild_comb$sign)]<-0

pfreq_guild_comb<- pfreq_guild_comb %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle),sign=as.factor(sign), park=reorder(park, -lat.rank))
pfreq_guild<-pfreq_guild_comb %>% mutate(guild=recode(guild,'Herbaceous'='Forb'))

#+++++++++++++++++++++++++++++++++++++++++++++++
# Park-level figures
#+++++++++++++++++++++++++++++++++++++++++++++++
#---------------------------------------------
# Average % Cover - Total
#---------------------------------------------
avgcov_total<-read.csv("../results/results_avecov-total-response_NP.csv")[,-1]
avgcov_total<-addC1_COLO(avgcov_total)

avgcov_total<- avgcov_total %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign), park=reorder(park, -lat.rank))

plot_avgcov_t<-suppressWarnings(plotCoverParkTotal(avgcov_total))
plot_avgcov_t
#-----------------------------------------------
# Quadrat % Frequency - Total
#-----------------------------------------------
qfreq_total<-read.csv("../results/results_qfreq-total-response_NP.csv")[,-1]
qfreq_total<-addC1_COLO(qfreq_total)

qfreq_total<- qfreq_total %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign), park=reorder(park, -lat.rank))

plot_qfreq_t<-plotQFreqParkTotal(qfreq_total)
plot_qfreq_t
#-----------------------------------------------
# Quadrat Richness - Total
#-----------------------------------------------
qrich_total<-read.csv("../results/results_qrich-total-response_NP.csv")[,-1]
qrich_total<-addC1_COLO(qrich_total)

qrich_total<- qrich_total %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign), park=reorder(park, -lat.rank))

plot_qrich_t<-suppressWarnings(plotQRichParkTotal(qrich_total))
plot_qrich_t
#++++++++++++++++++++++++++++++++++++++++++++++
# Overall Figures per metric
#++++++++++++++++++++++++++++++++++++++++++++++
plot_avgcov_t_all<-plotTotalByMetrics(avgcov_total, metric= "Average % Cover", 
                                      axis_name='% Cover')+ 
  scale_y_continuous(limits=c(-10,60),breaks=c(0,20,40,60),labels=c(0,20,40,60))

plot_qfreq_t_all<-plotTotalByMetrics(qfreq_total, metric= "Quadrat % Frequency", 
                                     axis_name='% Frequency')

plot_qrich_t_all<-plotTotalByMetrics(qrich_total, metric= "Average Quadrat Richness", 
                                     axis_name='# Species')+labs(x='Cycle')

#-----------------------------------------------
# Average % Cover - by Guild
#-----------------------------------------------
avgcov_guild<-read.csv("../results/results_avecov-by_guild-response_NP.csv")[,-1]
avgcov_guild<-addC1_COLO(avgcov_guild)
avgcov_guild<-avgcov_guild %>% mutate(guild=recode(guild,'Herbaceous'='Forb'))

avgcov_guild<- avgcov_guild %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign), park=reorder(park, -lat.rank))

plot_avgcov_g<-plotCoverParkGuild(avgcov_guild)

#-----------------------------------------------
# Quadrat % Frequency - by Guild
#-----------------------------------------------
qfreq_guild<-read.csv("../results/results_qfreq-by_guild-response_NP.csv")[,-1]
qfreq_guild<-addC1_COLO(qfreq_guild)
qfreq_guild<-qfreq_guild %>% mutate(guild=recode(guild,'Herbaceous'='Forb'))

qfreq_guild<- qfreq_guild %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign), park=reorder(park, -lat.rank))

plot_qfreq_g<-plotQFreqParkGuild(qfreq_guild)

#-----------------------------------------------
# Quadrat Richness - by Guild
#-----------------------------------------------
qrich_guild<-read.csv("../results/results_qrich-by_guild-response_NP.csv")[,-1]
qrich_guild<-addC1_COLO(qrich_guild)
qrich_guild<-qrich_guild %>% mutate(guild=recode(guild,'Herbaceous'='Forb'))

qrich_guild<- qrich_guild %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign), park=reorder(park, -lat.rank))

plot_qrich_g<-plotQRichParkGuild(qrich_guild)
#----------------------------------------------

#----------------------------------------------
# Overall guilds by metrics
#----------------------------------------------
# Average % Cover
gram_avgcov_g<-plotGuildsByMetrics(df=avgcov_guild, guild_name='Graminoid', 
             axis_name='Avg. % Cover', yrange=c(-5,40))
  
herb_avgcov_g<-plotGuildsByMetrics(df=avgcov_guild, guild_name='Forb', 
             axis_name=NULL, yrange=c(-0.5,20))

shrub_avgcov_g<-plotGuildsByMetrics(df=avgcov_guild, guild_name='Shrub', 
             axis_name=NULL, yrange=c(-5,40))

tree_avgcov_g<-plotGuildsByMetrics(df=avgcov_guild, guild_name='Tree', 
             axis_name='Avg. % Cover', yrange=c(-0.5,3))

# Quadrat Frequency
gram_qfreq_g<-plotGuildsByMetrics(df=qfreq_guild, guild_name='Graminoid', 
                                   axis_name='% Frequency', yrange=c(-5,100))

herb_qfreq_g<-plotGuildsByMetrics(df=qfreq_guild, guild_name='Forb', 
                                   axis_name=NULL, yrange=c(-5,100))

shrub_qfreq_g<-plotGuildsByMetrics(df=qfreq_guild, guild_name='Shrub', 
                                    axis_name=NULL, yrange=c(-5,100))

tree_qfreq_g<-plotGuildsByMetrics(df=qfreq_guild, guild_name='Tree', 
                                   axis_name='% Frequency', yrange=c(-5,100))

# Quadrat Richness
gram_qrich_g<-plotGuildsByMetrics(df=qrich_guild, guild_name='Graminoid', 
                                  axis_name='# Species', yrange=c(-0.1,2))

herb_qrich_g<-plotGuildsByMetrics(df=qrich_guild, guild_name='Forb', 
                                  axis_name=NULL, yrange=c(-0.1,2))

shrub_qrich_g<-plotGuildsByMetrics(df=qrich_guild, guild_name='Shrub', 
                                   axis_name=NULL, yrange=c(-0.1,2))

tree_qrich_g<-plotGuildsByMetrics(df=qrich_guild, guild_name='Tree', 
                                  axis_name='# Species', yrange=c(-0.1,2))


# Plot frequency
gram_pfreq_g<-plotFreqByGuilds(df=pfreq_guild,guild_name='Graminoid',
                                  axis_name='% of Plots')+
  scale_y_continuous(limits=c(-5,120),breaks=c(0,25,50,75,100),labels=c(0,25,50,75,100))

herb_pfreq_g<-plotFreqByGuilds(df=pfreq_guild,guild_name='Forb',
                                  axis_name=NULL)+
  scale_y_continuous(limits=c(-5,120),breaks=c(0,25,50,75,100),labels=c(0,25,50,75,100))

shrub_pfreq_g<-plotFreqByGuilds(df=pfreq_guild,guild_name='Shrub',
                                  axis_name=NULL)+
  scale_y_continuous(limits=c(-5,120),breaks=c(0,25,50,75,100),labels=c(0,25,50,75,100))

tree_pfreq_g<-plotFreqByGuilds(df=pfreq_guild,guild_name='Tree',
                                  axis_name='% of Plots')+
  scale_y_continuous(limits=c(-5,120),breaks=c(0,25,50,75,100),labels=c(0,25,50,75,100))



