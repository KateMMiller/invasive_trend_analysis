#-----------------------------------------------
# Invasive trend analysis: Plotting Results
#-----------------------------------------------
library(tidyverse)
library(colorRamps)
library(cowplot)
library(directlabels)
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

#tiff(file='./results/figures/pfreq_guild_sameY.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
#plot_pfreq_g
#dev.off()

#-----------------------------------------------
# Average % Cover - by Species
#-----------------------------------------------
AC_S_coef<-read.csv("./results/results_avecov-by_species-coefs_NP.csv")
avgcov_species<-read.csv("./results/results_avecov-by_species-response_NP.csv")

AC_S_slope<-AC_S_coef %>% left_join(.,avgcov_species[,c('park','lat.rank')], by="park") %>% 
  filter(coef=='Slope') %>% droplevels %>% unique() %>% arrange(species,park)

AC_S_slope$sign<-as.factor(AC_S_slope$sign)

#AC_S_slope$park<-reorder(AC_S_slope$park,-AC_S_slope$lat.rank)
View(AC_S_slope)

# Park-level plots that are combined via cowplot
avgcov_species<-read.csv("./results/results_avecov-by_species-response_NP.csv")

avgcov_species<- avgcov_species %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

avgcov_species$park<-reorder(avgcov_species$park,-avgcov_species$lat.rank)

head(avgcov_species)


avgcov_species_sign<-avgcov_species %>% filter(sign==1) %>% droplevels()
nlevels(avgcov_species_sign$species)

colramp=primary.colors(nlevels(avgcov_species_sign$species),steps=3,no.white = T)
colramp<-c('#009933', '#ff9900', '#00ccff', '#cc0000', '#00ffff', '#ffbf00', '#7733ff',
           '#ff99ff', '#33cc33', '#00ace6', '#ff0080',"#e6e600", "#8080FF", "#FF80FF", "#80FFFF","#0000FF")
names(colramp)<-as.character(levels(avgcov_species_sign$species))
colramp

symb<-c(21,23,24,25,21,23,24,25,21,23,24,21,23,24,25,21)
names(symb)<-names(colramp)

levels(avgcov_species_sign$park)
table(avgcov_species_sign$species,avgcov_species_sign$park)

SARA<-plotCoverParkSpecies(avgcov_species_sign, park='SARA', yrange=c(-1,25))
MIMA<-plotCoverParkSpecies(avgcov_species_sign, park='MIMA', yrange=c(-1,5))
ROVA<-plotCoverParkSpecies(avgcov_species_sign, park='ROVA', yrange=c(-1,5))
WEFA<-plotCoverParkSpecies(avgcov_species_sign, park='WEFA', yrange=c(-1,5))
MORR<-plotCoverParkSpecies(avgcov_species_sign, park='MORR', yrange=c(-1,40))
HOFU<-plotCoverParkSpecies(avgcov_species_sign, park='HOFU', yrange=c(-1,20))
VAFO<-plotCoverParkSpecies(avgcov_species_sign, park='VAFO', yrange=c(-1,40))
GETT<-plotCoverParkSpecies(avgcov_species_sign, park='GETT', yrange=c(-1,20))
CATO<-plotCoverParkSpecies(avgcov_species_sign, park='CATO', yrange=c(-1,10))
ANTI<-plotCoverParkSpecies(avgcov_species_sign, park='ANTI', yrange=c(-7,40))
MONO<-plotCoverParkSpecies(avgcov_species_sign, park='MONO', yrange=c(-5,50))
CHOH<-plotCoverParkSpecies(avgcov_species_sign, park='CHOH', yrange=c(-1,30))
HAFE<-plotCoverParkSpecies(avgcov_species_sign, park='HAFE', yrange=c(-1,20))
ROCR<-plotCoverParkSpecies(avgcov_species_sign, park='ROCR', yrange=c(-1,30))
GWMP<-plotCoverParkSpecies(avgcov_species_sign, park='GWMP', yrange=c(-1,10))
MANA<-plotCoverParkSpecies(avgcov_species_sign, park='MANA', yrange=c(-1,40))
PRWI<-plotCoverParkSpecies(avgcov_species_sign, park='PRWI', yrange=c(-1,5))
THST<-plotCoverParkSpecies(avgcov_species_sign, park='THST', yrange=c(-1,30))
GEWA<-plotCoverParkSpecies(avgcov_species_sign, park='GEWA', yrange=c(-5,20))
RICH<-plotCoverParkSpecies(avgcov_species_sign, park='RICH', yrange=c(-1,20))
APCO<-plotCoverParkSpecies(avgcov_species_sign, park='APCO', yrange=c(-1,20))
PETE<-plotCoverParkSpecies(avgcov_species_sign, park='PETE', yrange=c(-1,15))

parkgrid<-plot_grid(SARA, MIMA, ROVA, WEFA, MORR, HOFU, VAFO, GETT, CATO, ANTI, MONO, CHOH, 
                    HAFE, ROCR, GWMP, MANA, PRWI, THST, GEWA, RICH, APCO, PETE, ncol=5)
parkgrid

spp.leg<-function(df){ 
  ggplot(df, aes(x=cycle2, y=mean, group=species, shape=species, fill=species))+
    geom_line(aes(y=mean, x=cycle2, colour=species), lwd=1)+
    geom_point(aes(y=mean, x=cycle2, fill=species, shape=species), stroke=1, size=2,colour='black')+
    scale_shape_manual(values= symb)+
    scale_fill_manual(values = colramp)+ 
    scale_color_manual(values = colramp)+
    guides(shape=guide_legend(override.aes=list(size=2)))
}
AC_S_leg<-spp.leg(df=avgcov_species_sign)

leg.r<-cowplot::get_legend(AC_S_leg+theme(legend.direction='horizontal',legend.justification='left',
                                          legend.box.just='bottom', legend.key.size=unit(0.5,'in'), 
                                          legend.title=element_blank(), 
                                          legend.text=element_text(size=12, face='italic')))
finalgrid<-plot_grid(parkgrid,leg.r,rel_heights=c(3,0.5),nrow=2)  
finalgrid

#tiff(file='./results/figures/avgcov_species_NP.tiff',units='px',width=12*ppi,height=9*ppi,res=300)
#plot_avgcov_g
#dev.off()

#-----------------------------------------------
# Quadrat % Frequency - by Species
#-----------------------------------------------
qfreq_species<-read.csv("./results/results_qfreq-by_species-response_NP100.csv")

qfreq_species<- qfreq_species %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))

qfreq_species$park<-reorder(qfreq_species$park,-qfreq_species$lat.rank)

qfreq_species_sign<-qfreq_species %>% filter(sign==1) %>% droplevels()
levels(qfreq_species_sign$species)

#colramp=primary.colors(nlevels(qfreq_species_sign$species),steps=3,no.white = T)
colramp<-c('#009933', '#ff9900', '#00ccff', '#cc0000', "#b45a41", '#00ffff', '#ffbf00', '#7733ff',
           '#ff99ff', '#33cc33', '#00ace6', '#ff0080',"#e6e600", "#8080FF", "#FF80FF", "#80FFFF","#0000FF")
names(colramp)<-c("Acer platanoides","Ailanthus altissima","Alliaria petiolata","Berberis thunbergii", "Cardamine impatiens",
                  "Celastrus orbiculatus", "Elaeagnus", "Euonymus", "Euonymus fortunei", "Hedera helix", "Lonicera - Exotic",
                  "Lonicera japonica", "Microstegium vimineum", "Morus alba", "Robinia pseudoacacia",
                  "Rosa multiflora","Rubus phoenicolasius")

symb<-c(21,23,24,25,23,21,23,24,25,21,23,24,21,23,24,25,21)
names(symb)<-names(colramp)

levels(qfreq_species_sign$park)

SARA<-plotCoverParkSpecies(qfreq_species_sign, park='SARA', yrange=c(-7,50), yaxis='Quad % Freq.')
MIMA<-plotCoverParkSpecies(qfreq_species_sign, park='MIMA', yrange=c(-7,50))
ROVA<-plotCoverParkSpecies(qfreq_species_sign, park='ROVA', yrange=c(-7,50))
WEFA<-plotCoverParkSpecies(qfreq_species_sign, park='WEFA', yrange=c(-7,50))
MORR<-plotCoverParkSpecies(qfreq_species_sign, park='MORR', yrange=c(-7,80))
HOFU<-plotCoverParkSpecies(qfreq_species_sign, park='HOFU', yrange=c(-7,50), yaxis='Quad % Freq.')
VAFO<-plotCoverParkSpecies(qfreq_species_sign, park='VAFO', yrange=c(-7,50))
GETT<-plotCoverParkSpecies(qfreq_species_sign, park='GETT', yrange=c(-7,80))
CATO<-plotCoverParkSpecies(qfreq_species_sign, park='CATO', yrange=c(-7,50))
ANTI<-plotCoverParkSpecies(qfreq_species_sign, park='ANTI', yrange=c(-7,80))
MONO<-plotCoverParkSpecies(qfreq_species_sign, park='MONO', yrange=c(-7,50), yaxis='Quad % Freq.')
CHOH<-plotCoverParkSpecies(qfreq_species_sign, park='CHOH', yrange=c(-7,80))
HAFE<-plotCoverParkSpecies(qfreq_species_sign, park='HAFE', yrange=c(-7,50))
ROCR<-plotCoverParkSpecies(qfreq_species_sign, park='ROCR', yrange=c(-7,50))
GWMP<-plotCoverParkSpecies(qfreq_species_sign, park='GWMP', yrange=c(-7,50))
NACE<-plotCoverParkSpecies(qfreq_species_sign, park='NACE', yrange=c(-7,50), yaxis='Quad % Freq.')
MANA<-plotCoverParkSpecies(qfreq_species_sign, park='MANA', yrange=c(-7,80))
PRWI<-plotCoverParkSpecies(qfreq_species_sign, park='PRWI', yrange=c(-1,10))
THST<-plotCoverParkSpecies(qfreq_species_sign, park='THST', yrange=c(-7,105))
GEWA<-plotCoverParkSpecies(qfreq_species_sign, park='GEWA', yrange=c(-7,50))
RICH<-plotCoverParkSpecies(qfreq_species_sign, park='RICH', yrange=c(-7,50), yaxis='Quad % Freq.')
APCO<-plotCoverParkSpecies(qfreq_species_sign, park='APCO', yrange=c(-7,50))
PETE<-plotCoverParkSpecies(qfreq_species_sign, park='PETE', yrange=c(-7,50))

parkgrid<-plot_grid(SARA, MIMA, ROVA, WEFA, MORR, HOFU, VAFO, GETT, CATO, ANTI, MONO, CHOH, 
                    HAFE, ROCR, GWMP, NACE, MANA, PRWI, THST, GEWA, RICH, APCO, PETE, ncol=5)
parkgrid

spp.leg<-function(df){ 
  ggplot(df, aes(x=cycle2, y=mean, group=species, shape=species, fill=species))+
    geom_line(aes(y=mean, x=cycle2, colour=species), lwd=1)+
    geom_point(aes(y=mean, x=cycle2, fill=species, shape=species), stroke=1, size=2,colour='black')+
    scale_shape_manual(values= symb)+
    scale_fill_manual(values = colramp)+ 
    scale_color_manual(values = colramp)+
    guides(shape=guide_legend(override.aes=list(size=2), ncol=5))
}

QF_S_leg<-spp.leg(df=qfreq_species_sign)
QF_S_leg
leg.r<-cowplot::get_legend(QF_S_leg+theme(legend.direction='vertical',legend.justification='left',
                                          legend.box.just='bottom', legend.key.size=unit(0.75,'in'), 
                                          legend.title=element_blank(), 
                                          legend.text=element_text(size=12, face='italic')))

finalgrid<-plot_grid(parkgrid,leg.r,rel_heights=c(3,0.25),rel_widths=c(1,1.2),nrow=2)  
finalgrid

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

