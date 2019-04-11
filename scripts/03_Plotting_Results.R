#-----------------------------------------------
# Invasive trend analysis: Plotting Results
#-----------------------------------------------
library(tidyverse)
library(colorRamps)
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

#tiff(file='./results/figures/avgcov_total_NP.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
#plot_avgcov_t
#dev.off()

#-----------------------------------------------
# Quadrat % Frequency - Total
#-----------------------------------------------
qfreq_total<-read.csv("./results/results_qfreq-total-response_NP.csv")

qfreq_total<- qfreq_total %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

qfreq_total$park<-reorder(qfreq_total$park,-qfreq_total$lat.rank)

plot_qfreq_t<-plotQFreqParkTotal(qfreq_total)

#tiff(file='./results/figures/qfreq_total_NP.tiff',units='px',width=12.2*ppi,height=9*ppi,res=300)
#plot_qfreq_t
#dev.off()

#-----------------------------------------------
# Quadrat Richness - Total
#-----------------------------------------------
qrich_total<-read.csv("./results/results_qrich-total-response_NP.csv")

qrich_total<- qrich_total %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

qrich_total$park<-reorder(qrich_total$park,-qrich_total$lat.rank)

plot_qrich_t<-plotQRichParkTotal(qrich_total)

#tiff(file='./results/figures/qrich_total_NP.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
#plot_qrich_t
#dev.off()

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

#tiff(file='./results/figures/pfreq_total_sameY.tiff',units='px',width=12.5*ppi,height=9*ppi,res=300)
#plot_pfreq_t
#dev.off()

#-----------------------------------------------
# Average % Cover - by Guild
#-----------------------------------------------
avgcov_guild<-read.csv("./results/results_avecov-by_guild-response_NP.csv")

avgcov_guild<- avgcov_guild %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

avgcov_guild$park<-reorder(avgcov_guild$park,-avgcov_guild$lat.rank)

plot_avgcov_g<-plotCoverParkGuild(avgcov_guild)

#tiff(file='./results/figures/avgcov_guild_NP.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
#plot_avgcov_g
#dev.off()

#-----------------------------------------------
# Quadrat % Frequency - by Guild
#-----------------------------------------------
qfreq_guild<-read.csv("./results/results_qfreq-by_guild-response_NP.csv")

qfreq_guild<- qfreq_guild %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

qfreq_guild$park<-reorder(qfreq_guild$park,-qfreq_guild$lat.rank)

plot_qfreq_g<-plotQFreqParkGuild(qfreq_guild)

#tiff(file='./results/figures/qfreq_guild_NP.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
#plot_qfreq_g
#dev.off()

#-----------------------------------------------
# Quadrat Richness - by Guild
#-----------------------------------------------
qrich_guild<-read.csv("./results/results_qrich-by_guild-response_NP.csv")

qrich_guild<- qrich_guild %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

qrich_guild$park<-reorder(qrich_guild$park,-qrich_guild$lat.rank)

plot_qrich_g<-plotQRichParkGuild(qrich_guild)

#tiff(file='./results/figures/qrich_guild_NP.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
#plot_qrich_g
#dev.off()

#----------------------------------
# Plot frequency by guild
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

#tiff(file='./results/figures/pfreq_guild_sameY.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
#plot_pfreq_g
#dev.off()

#-----------------------------------------------
# Average % Cover - by Species
#-----------------------------------------------
avgcov_species<-read.csv("./results/results_avecov-by_species-response_NP.csv")

avgcov_species<- avgcov_species %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

avgcov_species$park<-reorder(avgcov_species$park,-avgcov_species$lat.rank)

avgcov_species_sign<-avgcov_species %>% filter(sign==1) %>% droplevels()


plotSpeciesTrends<-function(df, species_name=NULL, y_axis=NULL, yrange=c(0,10)){
  parkcols1<-data.frame(park=levels(df$park),
             parkcolor=primary.colors(nlevels(df$park), steps=3, no.white = TRUE))
  
  df1<-df %>% filter(species==species_name) %>% arrange(park) %>% droplevels()
  parks<-levels(df1$park)
  
  parkcols2<-parkcols1 %>% filter(park %in% parks) %>% arrange(park) %>% droplevels()
  print(parkcols2)
  
  parkcolors<-as.character(parkcols2[,2])

  print(ggplot(df1,aes(x=cycle2,y=mean, group=park, colour=park))+
          geom_line(aes(x=cycle2,y=mean),lwd=1,alpha=0.8,na.rm=T)+
          geom_errorbar(aes(ymin=lower, ymax=upper, x=cycle2), width=0.1,size=1, na.rm=TRUE)+
          geom_point(aes(x=cycle,y=mean),stroke=1,na.rm=T)+
          theme(axis.text=element_text(size=11),axis.title=element_text(size=12),
                plot.margin=unit(c(0.4,0.4,0.5,0.3),'lines'),
                plot.title=element_text(hjust=0.5, size=12, margin=margin(t=10,b=5)),
                panel.background=element_rect(fill='white',color='black'),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                axis.line = element_blank(), 
                legend.position='none') + 
          scale_x_discrete(breaks=c(1,2,3))+ylim(yrange)+
          scale_colour_manual(values=c(parkcolors))+
          labs(x='Cycle', y=y_axis, title=species_name)+
          geom_dl(aes(label=park), method=list(dl.trans(x=x+0.25),dl.combine("last.points"), cex=0.8, colour='black'))
          )
  }


levels(avgcov_species_sign$species) 


ACEPLA<-plotSpeciesTrends(avgcov_species_sign, species_name='Acer platanoides',
                                 y_axis='Avg. Quadrat % Cover',yrange=c(-1,10))

AILALT<-plotSpeciesTrends(avgcov_species_sign, species_name='Ailanthus altissima',
                           y_axis='Avg. Quadrat % Cover',yrange=c(-1,10))

ALLPET<-plotSpeciesTrends(avgcov_species_sign, species_name='Alliaria petiolata',
                          y_axis='Avg. Quadrat % Cover',yrange=c(-1,5))

BERTHU<-plotSpeciesTrends(avgcov_species_sign, species_name='Berberis thunbergii',
                          y_axis='Avg. Quadrat % Cover',yrange=c(-1,40))


#tiff(file='./results/figures/avgcov_species_NP.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
#plot_avgcov_g
#dev.off()

#-----------------------------------------------
# Quadrat % Frequency - by Species
#-----------------------------------------------
qfreq_species<-read.csv("./results/results_qfreq-by_species-response_NP.csv")
head(qfreq_species)

qfreq_species<- qfreq_species %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))


qfreq_species$park<-reorder(qfreq_species$park,-qfreq_species$lat.rank)
names(qfreq_species)

plot_qfreq_g<-plotQFreqParkSpecies(qfreq_species)

nlevels(qfreq_species$species)

plotQFreqParkSpecies<-function(df){ 
  print(ggplot(df, aes(x=cycle2, y=mean, group=species))+ 
          facet_wrap(~park,ncol=5,scales='free')+
          geom_errorbar(aes(ymin=lower, ymax=upper, x=cycle2,
                            colour=factor(species)), width=0.1,size=1, na.rm=TRUE)+
          geom_line(aes(y=mean, x=cycle2,colour=factor(species), linetype=sign), na.rm=TRUE)+
          geom_point(aes(y=mean, x=cycle2,colour=factor(species), shape=sign), 
                     size=1.8, stroke=1.5, fill='white', na.rm=TRUE)+
          scale_shape_manual(values=c(21,19))+
          scale_fill_manual(values=c('white'))+
          scale_linetype_manual(values=c('dashed','solid'))+ theme_bw()+
          scale_color_manual(values = colramp)+ 
          theme(axis.text=element_text(size=11),axis.title=element_text(size=12),
                plot.margin=unit(c(0.4,0.4,0.5,0.3),'lines'),
                plot.title=element_text(hjust=0.5, size=12, margin=margin(t=10,b=-15)),
                panel.background=element_blank(),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                axis.line = element_blank())+#, 
          #  legend.position='bottomright') + 
          labs(x='Cycle', y='Quadrat % Frequency')+
          scale_x_discrete(breaks=c(1,2,3))+
          scale_y_continuous(limits=c(-10,100),breaks=c(0,20,40,60,80,100),labels=c(0,20,40,60,80,100)))
} 


#tiff(file='./results/figures/qfreq_species_NP.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
#plot_qfreq_g
#dev.off()

#-----------------------------------------------
# Quadrat Richness - by Species
#-----------------------------------------------
qrich_species<-read.csv("./results/results_qrich-by_species-response_NP.csv")

qrich_species<- qrich_species %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

qrich_species$park<-reorder(qrich_species$park,-qrich_species$lat.rank)

plot_qrich_g<-plotQRichParkSpecies(qrich_species)

#tiff(file='./results/figures/qrich_species_NP.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
#plot_qrich_g
#dev.off()

#----------------------------------
# Plot frequency by species
#----------------------------------
pfreq_species_coefs<-read.csv('./results/results_pfreq-by_species-coefs.csv')
pfreq_species_slopes<-pfreq_species_coefs %>% filter(coef=='Slope') %>% droplevels()

df<-read.csv("./data/NETN-MIDN-ERMN-NCRN_species_invasives.csv")#[,-c(1,2)]
df_pf<- df %>% arrange(park,plot_name,cycle,species) %>% select(park,plot_name,cycle,species,lat.rank,plot.freq)
df_pf2<-df_pf %>% filter(park!='SAHI' & park!='WOTR') %>% group_by(park,species,cycle) %>% 
  summarise(plot.freq=sum(plot.freq), num.plots=n(),pfreq=round(((plot.freq/num.plots)*100),2), lat.rank=first(lat.rank))
head(df_pf2)

pfreq_species_comb<-merge(df_pf2,pfreq_species_slopes, by=c('park','species'),all.x=T)
pfreq_species_comb$sign[is.na(pfreq_species_comb$sign)]<-0

pfreq_species_comb<- pfreq_species_comb %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle),sign=as.factor(sign), park=reorder(park, -lat.rank))

pfreq_species_comb$species<-fct_relevel(pfreq_species_comb$species,c('Shrub','Herbaceous','Graminoid', 'Tree'))

levels(pfreq_species_comb$species)

plot_pfreq_g<-plotFreqParkSpecies(pfreq_species_comb)

#tiff(file='./results/figures/pfreq_species_sameY.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
#plot_pfreq_g
#dev.off()

