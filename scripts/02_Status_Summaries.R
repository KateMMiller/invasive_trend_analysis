#----------------------------------
# Invasive trend analysis: 4-year status summaries
#----------------------------------
library(tidyverse)

invdf<-read.csv('./data/NETN-MIDN-ERMN-NCRN_species_invasives_status.csv')
inv3<-invdf %>% filter(cycle==3) %>% droplevels()
table(inv3$species, inv3$plot.freq) # number of species on list that aren't present in C3 plots

# Lump difficult to ID species
sort(unique(inv3$species))
Lonicera_spp<-c('Lonicera', 'Lonicera maackii', 'Lonicera morrowii', 'Lonicera tatartica')
Ligustrum_spp<-c('Ligustrum', 'Ligustrum obtusifolium','Ligustrum ovalifolium','Ligustrum vulgare')
Wisteria_spp<-c('Wisteria','Wisteria floribunda','Wisteria sinensis')
Elaeagnus_spp<-c('Elaeagnus','Elaeagnus angustifolia', 'Elaeagnus pungens','Elaeagnus umbellata')

inv3<-inv3 %>% mutate(sppgroup= case_when(species %in% Lonicera_spp ~ 'Lonicera',
                                          species %in% Ligustrum_spp ~ 'Ligustrum',
                                          species %in% Wisteria_spp ~ 'Wisteria',
                                          species %in% Elaeagnus_spp ~ 'Elaeagnus',
                                          TRUE ~ paste0(species)))

length(unique(inv3$plot_name)) #1479 plots

park_info<-unique(read.csv('./data/NETN-MIDN-ERMN-NCRN_total_invasives.csv')[,c('park','lat.rank')])

inv_park<-inv3 %>% group_by(network, park, sppgroup) %>%  
  summarise(numplots=sum(plot.freq),plot.pfreq=round(sum(plot.freq,na.rm=T)/n()*100,2), 
            avg.cover=round(mean(avg.cover, na.rm=T),2), qpct.freq=round(mean(qpct.freq,na.rm=T),2)) %>% 
  ungroup()

invlist<-read.csv('./data/Invasive_List.csv')
names(invlist)

inv_park2<-merge(inv_park, invlist[,c('Latin_Name','Common','Guild')], by.x='sppgroup',by.y='Latin_Name', all.x=T)

# Summary tables for each metric to be included in supplemental material
inv_park_pf<-inv_park2 %>% select (park, sppgroup, Common, Guild, plot.pfreq) %>% spread(park, plot.pfreq, fill=0)
write.csv(inv_park_pf, "./results/status/Species_plot_frequency_by_park.csv", row.names=F)

inv_park_ac<-inv_park2 %>% select (park, sppgroup, Common, Guild, avg.cover) %>% spread(park, avg.cover, fill=0)
write.csv(inv_park_ac, "./results/status/Species_average_cover_by_park.csv", row.names=F)

inv_park_qf<-inv_park2 %>% select (park, sppgroup, Common, Guild, qpct.freq) %>% spread(park, qpct.freq, fill=0)
write.csv(inv_park_qf, "./results/status/Species_quad_frequency_by_park.csv", row.names=F)

# Status table with species present in each park 
inv_park3<-inv_park2 %>% mutate(numplots=ifelse(numplots==0,NA,numplots)) %>% 
  filter(!is.na(numplots)) %>% arrange(park,sppgroup) %>% 
  select(network, park, sppgroup, Common, Guild, plot.pfreq, avg.cover, qpct.freq, numplots)

inv_park4<-merge(inv_park3, park_info, by='park',all.x=T) %>% select(lat.rank, everything()) %>% arrange(-lat.rank)
write.csv(inv_park4, './results/status/park-level_species_summaries.csv',row.names=F)

# Top_5 based on most frequent on the plot, then highest cover, then highest quad frequency if there's a tie
inv_park_top5<- inv_park4 %>% group_by(network, park) %>% arrange(-lat.rank, -plot.pfreq, -avg.cover, -qpct.freq) %>% 
  slice(1:5) %>% arrange(-lat.rank,sppgroup)
head(inv_park_top5)

write.csv(inv_park_top5, './results/status/park-level_species_top5_summaries.csv', row.names=F)

# Summarize species-level trends
spp_max<-inv3 %>% group_by(sppgroup) %>% summarise(max_cov=round(max(avg.cover),1), max_qfreq=round(max(qpct.freq),1))

inv4<-inv3
inv4[,c(7:10)][inv4[,c(7:10)]==0]<-NA

spp_avgP<-inv4 %>% group_by(sppgroup) %>% summarise(avg_covP=round(mean(avg.cover, na.rm=T),1),
                                                    avg_qpctP=round(mean(qpct.freq,na.rm=T),1))

spp_sum<-inv_park3 %>% group_by(sppgroup) %>% 
  summarise(numparks=length(unique(park)), numplots=sum(numplots), 
            pct.plots=round((numplots/1470)*100,1), pct.parks=round((numparks/39)*100,1)) %>% 
            # where present in a plot- average cover and quad frequency
  arrange(-numparks,-numplots, sppgroup) %>% ungroup()

comb_spp<-merge(spp_sum,spp_max, by='sppgroup', all.x=T)
comb_spp2<-merge(comb_spp,spp_avgP, by='sppgroup',all.x=T)

comb_spp3<-merge(comb_spp2, invlist[,c('Latin_Name','Common','Guild')], by.x='sppgroup',by.y='Latin_Name', all.x=T) %>% 
  select(sppgroup,Common, everything())


write.csv(comb_spp3, './results/status/species-level_summaries.csv', row.names=F)

# Summarize invasion levels by park
list.files('./data/')
totinv<-read.csv('./data/NETN-MIDN-ERMN-NCRN_total_invasives.csv')
head(totinv)

guildinv<-read.csv('./data/NETN-MIDN-ERMN-NCRN_guild_invasives.csv')
head(guildinv)
View(totinv)
parktot<-totinv %>% filter(cycle==3) %>% group_by(network, park, lat.rank) %>% 
  summarise(plot.freq=sum(plot.freq), plot.pfreq=round(100*sum(plot.freq)/n(),2), avg.cover=round(mean(avg.cover),2), quad.pfreq=round(mean(qpct.freq),2), totplots=n()) %>% 
  select(lat.rank, everything()) %>% arrange(-plot.pfreq, -avg.cover, -quad.pfreq)

write.csv(parktot, './results/status/park-level_summaries.csv', row.names=F)

View(parktot)
