#-----------------------------------------------
# Invasive trend analysis: Plotting Results
#-----------------------------------------------
library(tidyverse)
#library(colorRamps)
library(cowplot)
library(directlabels)
#library(randomcoloR)

source('./scripts/functions_for_PLOTTING.R')
ppi<-300

#-----------------------------------------------
# Average % Cover - Total
#-----------------------------------------------
avgcov_total<-read.csv("./results/results_avecov-total-response_NP.csv")[,-1]

avgcov_total<- avgcov_total %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

avgcov_total$park<-reorder(avgcov_total$park,-avgcov_total$lat.rank)

plot_avgcov_t<-plotCoverParkTotal(avgcov_total)
plot_avgcov_t

tiff(file='./results/figures/avgcov_total_NP.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
plot_avgcov_t
dev.off()

#-----------------------------------------------
# Quadrat % Frequency - Total
#-----------------------------------------------
qfreq_total<-read.csv("./results/results_qfreq-total-response_NP.csv")

qfreq_total<- qfreq_total %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

qfreq_total$park<-reorder(qfreq_total$park,-qfreq_total$lat.rank)

plot_qfreq_t<-plotQFreqParkTotal(qfreq_total)

tiff(file='./results/figures/qfreq_total_NP.tiff',units='px',width=12.2*ppi,height=9*ppi,res=300)
plot_qfreq_t
dev.off()

#-----------------------------------------------
# Quadrat Richness - Total
#-----------------------------------------------
qrich_total<-read.csv("./results/results_qrich-total-response_NP.csv")

qrich_total<- qrich_total %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

qrich_total$park<-reorder(qrich_total$park,-qrich_total$lat.rank)

plot_qrich_t<-plotQRichParkTotal(qrich_total)

tiff(file='./results/figures/qrich_total_NP.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
plot_qrich_t
dev.off()

#-----------------------------------------------
# Plot Frequency - Total
#-----------------------------------------------
pfreq_total_coefs<-read.csv('./results/results_PFreq-total-coefs.csv')
pfreq_total_slopes<-pfreq_total_coefs %>% filter(coef=='Slope') %>% droplevels()

df<-read.csv("./data/NETN-MIDN-ERMN-NCRN_total_invasives.csv")#[,-c(1,2)]
df_pf<- df %>% arrange(park,plot_name,cycle) %>% select(park,plot_name,cycle,lat.rank,plot.freq)
df_pf2<-df_pf %>% filter(!(park %in% c('SAHI','WOTR'))) %>% droplevels()
df_pf3<-df_pf2 %>% group_by(park,cycle) %>% 
  summarise(plot.freq=sum(plot.freq), num.plots=n(),pfreq=round(((plot.freq/num.plots)*100),2), lat.rank=first(lat.rank))

pfreq_total_comb<-merge(df_pf3,pfreq_total_slopes, by=c('park'),all.x=T)
pfreq_total_comb$sign[is.na(pfreq_total_comb$sign)]<-0

pfreq_total_comb<- pfreq_total_comb %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle),sign=as.factor(sign), park=reorder(park, -lat.rank))

plot_pfreq_t<-plotFreqParkTotal(pfreq_total_comb)

tiff(file='./results/figures/pfreq_total_sameY.tiff',units='px',width=12.5*ppi,height=9*ppi,res=300)
plot_pfreq_t
dev.off()

#-----------------------------------------------
# Average % Cover - by Guild
#-----------------------------------------------
avgcov_guild<-read.csv("./results/results_avecov-by_guild-response_NP.csv")

avgcov_guild<- avgcov_guild %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

avgcov_guild$park<-reorder(avgcov_guild$park,-avgcov_guild$lat.rank)

plot_avgcov_g<-plotCoverParkGuild(avgcov_guild)

tiff(file='./results/figures/avgcov_guild_NP.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
plot_avgcov_g
dev.off()

#-----------------------------------------------
# Quadrat % Frequency - by Guild
#-----------------------------------------------
qfreq_guild<-read.csv("./results/results_qfreq-by_guild-response_NP.csv")

qfreq_guild<- qfreq_guild %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

qfreq_guild$park<-reorder(qfreq_guild$park,-qfreq_guild$lat.rank)

plot_qfreq_g<-plotQFreqParkGuild(qfreq_guild)

tiff(file='./results/figures/qfreq_guild_NP.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
plot_qfreq_g
dev.off()

#----------------------------------
# Plot frequency by Guild
#----------------------------------
pfreq_guild_coefs<-read.csv('./results/results_pfreq-by_guild-coefs.csv')
pfreq_guild_slopes<-pfreq_guild_coefs %>% filter(coef=='Slope') %>% droplevels()

df<-read.csv("./data/NETN-MIDN-ERMN-NCRN_guild_invasives.csv")#[,-c(1,2)]
df_pf<- df %>% arrange(park,plot_name,cycle,guild) %>% select(park,plot_name,cycle,guild,lat.rank,plot.freq)
df_pf2<-df_pf %>% filter(park!='SAHI' & park!='WOTR') %>% group_by(park,guild,cycle) %>% 
  summarise(plot.freq=sum(plot.freq), num.plots=n(),pfreq=round(((plot.freq/num.plots)*100),2), lat.rank=first(lat.rank))
head(df_pf2)

pfreq_guild_comb<-merge(df_pf2,pfreq_guild_slopes, by=c('park','guild'),all.x=T)
pfreq_guild_comb$sign[is.na(pfreq_guild_comb$sign)]<-0

pfreq_guild_comb<- pfreq_guild_comb %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle),sign=as.factor(sign), park=reorder(park, -lat.rank))

pfreq_guild_comb$guild<-fct_relevel(pfreq_guild_comb$guild,c('Shrub','Herbaceous','Graminoid', 'Tree'))

levels(pfreq_guild_comb$guild)

plot_pfreq_g<-plotFreqParkGuild(pfreq_guild_comb)

tiff(file='./results/figures/pfreq_guild_sameY.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
plot_pfreq_g
dev.off()

#-----------------------------------
# Average Cover Species Coef Plots
#-----------------------------------
AC_S_coef<-read.csv("./results/results_avecov-by_species-coefs_NP.csv")

AC_S_sign<-AC_S_coef %>% filter(sign==1 & coef=='Slope') %>% 
  mutate(species= as.factor(ifelse(species=='Lonicera'| species=='Lonicera morrowii','Lonicera spp. (Exotic)', paste(species)))) %>% 
  droplevels() %>% group_by(species) %>% mutate(numsig=sum(sign)) %>% ungroup() %>%
  mutate(species= fct_reorder(species, numsig, .desc=T)) %>% arrange(species)  
 
head(AC_S_sign)
sort(unique(AC_S_sign$species))#14

            # MICVIM,   BERTHU,   LONJAP,   ALLPET,    ROSMUL,   LONEXO,   AILALT,   CELORB,  RUBPHO,  ACEPLA, 
colrkeep<- c("#ffe400","#cc0000","#cc00cc","#00cc00","#ff0000","#ffa366","#0000ff","#e65c00","#cc0099","#0680f9", 
            # ELAUMB, GLEHED, HEDHEL, PERLON
            "#ff66cc", "#006600", "#ff9933", "#339933")

coefPlot(AC_S_sign, ylabel='Average Cover/ Cycle') # Shows species that are significant in at least 2 parks.

tiff(file='./results/figures/avgcov_species_slopes.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
coefPlot(AC_S_sign, ylabel='Average Cover/ Cycle')
dev.off()


#-----------------------------------
# Quad Freq Species Coef Plots
#-----------------------------------
QF_S_coef<-read.csv("./results/results_qfreq-by_species-coefs_NP.csv")

QF_S_sign<-QF_S_coef %>% filter(sign==1 & coef=='Slope') %>% droplevels() %>% group_by(species) %>% mutate(numsig=sum(sign)) %>% ungroup() %>%
  mutate(species= fct_reorder(species, numsig, .desc=T)) %>% arrange(species)  

length(unique(QF_S_sign$species)) #11
sort(unique(QF_S_sign$species))
             #MICVIM,   ALLPET,    LONJAP,  AILALT,   ROSMUL,     RUBPHO,    BERTHU,  CELORB,
colrkeep<-c("#ffe400", "#00cc00", "#cc00cc","#0000ff","#ff0000", "#cc0099", "#cc0000","#e65c00",
            # ACEPLA,    CARIMP,   EUOFOR
            "#0680f9",  "#66ff33", "#ff0066")

coefPlot(QF_S_sign, yrange=c(-20,55), ylabel='Quadrat Frequency/ Cycle') # Shows species that are significant in at least 2 parks.


tiff(file='./results/figures/qfreq_species_slopes.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
coefPlot(QF_S_sign, yrange=c(-20,55),ylabel='Quadrat Frequency/ Cycle')
dev.off()


#-----------------------------------
# Plot Freq Species Coef Plots
#-----------------------------------
# Species by park plots for invasive cover trends
PF_S_coef<-read.csv("./results/results_PFreq-by_species-coefs.csv")
names(PF_S_coef)

PF_S_sign<-PF_S_coef %>% filter(sign==1 & coef=='Slope') %>% group_by(species) %>% mutate(numsig=sum(sign)) %>% ungroup() %>%
  mutate(species= fct_reorder(species, numsig, .desc=T)) %>% arrange(species)  %>% ungroup() %>% droplevels()


             # CELORB,   MICVIM,    ROSMUL,   RUBPHO,    BERTHU,    EUOALA,     AILALT      ALLPET,  LONEXO,     
colrkeep<- c("#e65c00", "#ffe400", "#ff0000", "#cc0099", "#cc0000", "#bf00ff", "#0000ff", "#00cc00", "#ffa366",
             #ACEPLA,   BERVUL     CARIMP      LONJAP    PERLONG,   PHOVIL,   PRUAVI
             "#0680f9", "#FF00FF", "#66ff33", "#cc00cc", "#339933", "#1E90FF", "#4674b9")


PFreqPlot(PF_S_sign, ylabel='by Species')

tiff(file='./results/figures/Plot_freq_species_odd.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
PFreqPlot(PF_S_sign, ylabel='by Species')
dev.off()

#list.files("./results")

