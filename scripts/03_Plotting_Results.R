#-----------------------------------------------
# Invasive trend analysis: Plotting Results
#-----------------------------------------------
library(tidyverse)

source('./scripts/functions_for_PLOTTING.R')
ppi<-300

#-----------------------------------------------
# Average % Cover - Total
#-----------------------------------------------
avgcov_total<-read.csv("./results/results_avecov-total-response.csv")[,-1]

avgcov_total<- avgcov_total %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

avgcov_total$park<-reorder(avgcov_total$park,-avgcov_total$lat.rank)

plot_avgcov_t<-plotCoverParkTotal(avgcov_total)

tiff(file='./results/figures/avgcov_total_sameY.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
plot_avgcov_t
dev.off()

#-----------------------------------------------
# Quadrat % Frequency - Total
#-----------------------------------------------
qfreq_total<-read.csv("./results/results_qfreq-total-response.csv")

qfreq_total<- qfreq_total %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

qfreq_total$park<-reorder(qfreq_total$park,-qfreq_total$lat.rank)

plot_qfreq_t<-plotQFreqParkTotal(qfreq_total)

tiff(file='./results/figures/qfreq_total_sameY.tiff',units='px',width=12.2*ppi,height=9*ppi,res=300)
plot_qfreq_t
dev.off()

#-----------------------------------------------
# Quadrat Richness - Total
#-----------------------------------------------
qrich_total<-read.csv("./results/results_qrich-total-response.csv")

qrich_total<- qrich_total %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

qrich_total$park<-reorder(qrich_total$park,-qrich_total$lat.rank)

plot_qrich_t<-plotQRichParkTotal(qrich_total)

tiff(file='./results/figures/qrich_total_sameY.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
plot_qrich_t
dev.off()

#-----------------------------------------------
# Plot Frequency - Total
#-----------------------------------------------
pfreq_total_coefs<-read.csv('./results/results_PFreq-total-coefs.csv')
pfreq_total_slopes<-pfreq_total_coefs %>% filter(coef=='Slope') %>% droplevels()

df<-read.csv("./data/NETN-MIDN-ERMN-NCRN_total_invasives.csv")#[,-c(1,2)]
df_pf<- df %>% arrange(park,plot_name,cycle) %>% select(park,plot_name,cycle,lat.rank,plot.freq)
df_pf2<-df_pf %>% group_by(park,cycle) %>% 
  summarise(plot.freq=sum(plot.freq), num.plots=n(),pfreq=round(((plot.freq/num.plots)*100),2), lat.rank=first(lat.rank))
head(df_pf2)

pfreq_total_comb<-merge(df_pf2,pfreq_total_slopes, by=c('park'),all.x=T)
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
avgcov_guild<-read.csv("./results/results_avecov-by_guild-response.csv")

avgcov_guild<- avgcov_guild %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

avgcov_guild$park<-reorder(avgcov_guild$park,-avgcov_guild$lat.rank)

plot_avgcov_g<-plotCoverParkGuild(avgcov_guild)

tiff(file='./results/figures/avgcov_guild_sameY.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
plot_avgcov_g
dev.off()

#-----------------------------------------------
# Quadrat % Frequency - by Guild
#-----------------------------------------------
qfreq_guild<-read.csv("./results/results_qfreq-by_guild-response.csv")

qfreq_guild<- qfreq_guild %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

qfreq_guild$park<-reorder(qfreq_guild$park,-qfreq_guild$lat.rank)

plot_qfreq_g<-plotQFreqParkGuild(qfreq_guild)

tiff(file='./results/figures/qfreq_guild_sameY.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
plot_qfreq_g
dev.off()

#-----------------------------------------------
# Quadrat Richness - by Guild
#-----------------------------------------------
qrich_guild<-read.csv("./results/results_qrich-by_guild-response.csv")

qrich_guild<- qrich_guild %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

qrich_guild$park<-reorder(qrich_guild$park,-qrich_guild$lat.rank)


plot_qrich_g<-plotQRichParkGuild(qrich_guild)

tiff(file='./results/figures/qrich_guild_sameY.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
plot_qrich_g
dev.off()

#----------------------------------
# Plot frequency by guild
#----------------------------------
pfreq_guild_coefs<-read.csv('./results/results_pfreq-by_guild-coefs.csv')
pfreq_guild_slopes<-pfreq_guild_coefs %>% filter(coef=='Slope') %>% droplevels()

df<-read.csv("./data/NETN-MIDN-ERMN-NCRN_guild_invasives.csv")#[,-c(1,2)]
df_pf<- df %>% arrange(park,plot_name,cycle,guild) %>% select(park,plot_name,cycle,guild,lat.rank,plot.freq)
df_pf2<-df_pf %>% group_by(park,guild,cycle) %>% 
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

#----------------------------------------------
# Combining data for mapping
#----------------------------------------------

#------------Average % Cover
head(avgcov_total)
head(avgcov_guild)

avgcov_t3<-avgcov_total %>% filter(cycle==3) %>% mutate(totcov=mean, totsign=sign) %>% 
  select(park,lat.rank,totcov,totsign)

avgcov_g3<-avgcov_guild %>% filter(cycle==3) %>% mutate(guildcov=mean) %>% 
  select(park,guild,guildcov) %>% spread(guild, guildcov, fill=0)  

avgcov_g3sign<-avgcov_guild %>% filter(cycle==3) %>% mutate(guildsign=sign) %>% 
  select(park,guild,guildsign) %>% spread(guild,guildsign,fill=0)

View(avgcov_g3sign)

avgcov_c3<-merge(avgcov_t3, avgcov_g3, by='park')
avgcov_c3b<-merge(avgcov_c3,avgcov_g3sign, by='park')
names(avgcov_c3b)

colnames(avgcov_c3b)<-c('park','lat.rank','totcov','totsign','Graminoid','Herbaceous','Shrub','Tree',
 'gramcovsign','herbcovsign', 'shrubcovsign','treecovsign')

avgcov_c3b<- avgcov_c3b %>% mutate(totcov.rank=rank(desc(totcov)),
                                 gramcov.rank=rank(desc(Graminoid)),
                                 shrubcov.rank=rank(desc(Shrub)),
                                 herbcov.rank=rank(desc(Herbaceous)),
                                 treecov.rank=rank(desc(Tree)),
                                 gramcov.HL=ifelse(gramcov.rank>30,"L",ifelse(gramcov.rank<6,"H",NA)),
                                 shrubcov.HL=ifelse(shrubcov.rank>34,"L",ifelse(shrubcov.rank<6,"H",NA)),
                                 herbcov.HL=ifelse(herbcov.rank>34,"L",ifelse(herbcov.rank<6,"H",NA)),
                                 treecov.HL=ifelse(treecov.rank>34,"L",ifelse(treecov.rank<6,"H",NA))
                                 )
head(avgcov_c3b)

write.csv(avgcov_c3b,'./results/avgcover_ranks_for_table.csv', row.names=F)

findSmallNZ(avgcov_c3,"totcov")

avgcov_c3[,c(3,5:8)][avgcov_c3[,c(3,5:8)] < 0] <- 0

avgcov_c3<- avgcov_c3 %>% mutate(totcovsq=sqrt(totcov+0.0009628),totcovlog=log(totcov+0.0009628))
head(avgcov_c3)

write.csv(avgcov_c3,'./results/for_mapping/avgcov_for_GIS.csv')

