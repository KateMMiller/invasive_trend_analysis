#------------------------
# Combine network datasets
#------------------------
library(tidyverse)
#-----------------
# Total Invasives
#-----------------
#---NETN Total Invasives
netn_totinv<-read.csv("./data/NETN/NETN_invasive_total_data.csv")
netn_totinv<-netn_totinv %>% mutate(network='NETN') %>% 
  select(network,Unit_Code:Y_Coord,Year,cycle:numPlotSpp)
colnames(netn_totinv)<-c("network","park","plot_name","plot_number","x_coord",
  "y_coord","year","cycle","plot.freq","avg.quad.r",
  "avg.cover",'quad.freq','qpct.freq','num.plot.spp')

# shift cycles for NETN, so 2006 is dropped and 2018 is part of cycle 3
netn_totinv<- netn_totinv %>% mutate(cycle= case_when(year<=2010~1, 
  between(year,2011,2014)~2, 
  between(year,2015,2018)~3)) 

#---MIDN Total Invasives
midn_totinv<-read.csv("./data/MIDN/MIDN_invasive_total_data.csv")
midn_totinv<-midn_totinv %>% mutate(network='MIDN') %>% 
  select(network,Unit_Code:Y_Coord,Year,cycle:numPlotSpp)
colnames(midn_totinv)<-c("network","park","plot_name","plot_number","x_coord",
  "y_coord","year","cycle","plot.freq","avg.quad.r",
  "avg.cover",'quad.freq','qpct.freq','num.plot.spp')

# in MIDN shift COLO so cycle 1 is 2 and cycle 2 is 3 so timelines match better
midn_totinv<- midn_totinv %>% mutate(cycle=case_when(park=='COLO' & cycle==2 ~ 3,
  park=='COLO' & cycle==1 ~ 2,
  park!='COLO' & cycle==1 ~ 1,
  park!='COLO' & cycle==2 ~ 2,
  park!='COLO' & cycle==3 ~ 3)) 

#---NCRN Total Invasives
ncrn_totinv<-read.csv("./data/NCRN/NCRN_total_invasives.csv")[,-1]

ncrn_totinv<-ncrn_totinv %>% mutate(plot.freq=ifelse(num.plot.spp>0,1,0), 
  plot_number=str_sub(plot_name,start=-4)) %>% 
  select(network,park,plot_name,plot_number,x_coord,y_coord,year,cycle,plot.freq,
    avg.quad.r,avg.cover,quad.freq,qpct.freq,num.plot.spp)

#---ERMN Total Invasives
ermn_totinv<-read.csv("./data/ERMN/ERMN_total_Invasives20190528.csv")
names(ermn_totinv)[names(ermn_totinv)=='ave.quad.r']<-'avg.quad.r'

# combine datasets
names(netn_totinv); names(ermn_totinv); names(midn_totinv); names(ncrn_totinv)

comb_totinv<-rbind(netn_totinv, midn_totinv, ermn_totinv, ncrn_totinv)

table(comb_totinv$park,comb_totinv$cycle)

lat.order<-comb_totinv %>% group_by(park) %>% 
  summarise(mean.lat=mean(y_coord,na.rm=T)) %>% arrange(-mean.lat) 

lat.order$lat.rank <- NA
lat.order$lat.rank<-rank(lat.order$mean.lat)

comb_totinv<-comb_totinv %>% left_join(.,lat.order, by='park') 

comb_totinv<-comb_totinv %>% arrange(desc(network),plot_name,cycle)
comb_totinv2<-comb_totinv %>% mutate(qpct.freq=ifelse(network!='NCRN',qpct.freq*100,qpct.freq))

write.csv(comb_totinv2,'./data/NETN-MIDN-ERMN-NCRN_total_invasives.csv', row.names=F)

#-----------------
# Invasives by guild
#-----------------
#---NETN Invasives by guild
netn_guild<-read.csv("./data/NETN/NETN_invasive_guild_data.csv")
names(netn_guild)

netn_guild<-netn_guild %>% mutate(network='NETN') %>% 
  select(network,Unit_Code:Y_Coord,Year,cycle:numPlotSpp)
colnames(netn_guild)<-c("network","park","plot_name","plot_number","x_coord",
  "y_coord","year","cycle",'guild',"plot.freq","avg.quad.r",
  "avg.cover",'quad.freq','qpct.freq','num.plot.spp')

# shift cycles for NETN, so 2006 is dropped and 2018 is part of cycle 3
netn_guild<- netn_guild %>% mutate(cycle= case_when(year<=2010~1, 
  between(year,2011,2014)~2, 
  between(year,2015,2018)~3)) 

#---MIDN Invasives by guild
midn_guild<-read.csv("./data/MIDN/MIDN_invasive_guild_data.csv")
names(midn_guild)

midn_guild<-midn_guild %>% mutate(network='MIDN') %>% 
  select(network,Unit_Code:Y_Coord,Year,cycle:numPlotSpp)
colnames(midn_guild)<-c("network","park","plot_name","plot_number","x_coord",
  "y_coord","year","cycle",'guild',"plot.freq","avg.quad.r",
  "avg.cover",'quad.freq','qpct.freq','num.plot.spp')

# in MIDN shift COLO so cycle 1 is 2 and cycle 2 is 3 so timelines match better
table(midn_guild$park, midn_guild$cycle)
midn_guild<- midn_guild %>% mutate(cycle=case_when(park=='COLO' & cycle==2 ~ 3,
  park=='COLO' & cycle==1 ~ 2,
  park!='COLO' & cycle==1 ~ 1,
  park!='COLO' & cycle==2 ~ 2,
  park!='COLO' & cycle==3 ~ 3)) 

#---ERMN Invasives by guild
ermn_guild<-read.csv("./data/ERMN/ERMN_Guild_Invasives20190528.csv")
names(ermn_guild)[names(ermn_guild)=="ave.quad.r"]<-'avg.quad.r'

#---NCRN Invasives by guild
ncrn_guild<-read.csv("./data/NCRN/NCRN_guild_invasives.csv")[,-1]
  # ncrn_guild has two levels for shrubs- one for quads and one for microplots.
  # the following steps calc. plot frequency using both shrub guilds, filters
  # out the unwanted shrub-micro data, and cleans up factor level names
  # and field names, etc to be able to incorporate with rest of networks' data.

ncrn_guild<-ncrn_guild %>% mutate(plot_number=str_sub(plot_name,start=-4)) %>% 
  select(network,park,plot_name,plot_number,x_coord,y_coord,year,cycle,guild,
    avg.quad.r,avg.cover,quad.freq,qpct.freq,num.plot.spp)
#View(ncrn_guild)

# Pull out shrub data- if num.plot.spp>0 for shrubs-quad or shrubs-micro, plot.freq.shrub=1.
# Also combine # species in shrubs-micro and shrubs-quad
ncrn_shrubs<-ncrn_guild %>% filter(guild %in% c('Shrubs-Micro','Shrubs-Quad')) %>% droplevels() %>% 
  group_by(plot_name,year,cycle) %>% 
  summarise(plot.freq.shrub=ifelse(sum(num.plot.spp)>0,1,0), num.plot.spp.shrub=sum(num.plot.spp,na.rm=T)) %>% 
  mutate(guild=as.factor('Shrub')) %>% as.data.frame()

ncrn_guild2<-ncrn_guild %>% filter(guild!="Shrubs-Micro") %>% droplevels() %>% 
  mutate(guild = as.factor(case_when(guild=='Graminoids'~'Graminoid',
                                     guild=='Herbaceous'~'Herbaceous',
                                     guild=='Shrubs-Quad'~'Shrub',
                                     guild=='Trees'~'Tree')))

ncrn_guild3<-merge(ncrn_guild2,ncrn_shrubs, by=c('plot_name','year','cycle','guild'),all.x=T)
ncrn_guild3$num.plot.spp.shrub[is.na(ncrn_guild3$num.plot.spp.shrub)]<-0
ncrn_guild3<-ncrn_guild3 %>% mutate(num.plot.spp = num.plot.spp+num.plot.spp.shrub) %>% select(-num.plot.spp.shrub)

ncrn_guild4<-ncrn_guild3 %>% mutate(plot.freq=ifelse(guild=='Shrub',plot.freq.shrub,ifelse(num.plot.spp>0,1,0)))
head(ncrn_guild4)

ncrn_guild5<- ncrn_guild4 %>% select(network,park,plot_name,plot_number,x_coord,y_coord,year,cycle,guild,plot.freq,
  avg.quad.r,avg.cover,quad.freq,qpct.freq,num.plot.spp) 

names(ermn_guild); names(netn_guild); names(midn_guild); names(ncrn_guild5)

comb_guild<-rbind(netn_guild,midn_guild,ermn_guild,ncrn_guild5)

comb_guild<-comb_guild %>% left_join(.,lat.order, by='park')

comb_guild<-comb_guild %>% mutate(qpct.freq=ifelse(network!='NCRN',qpct.freq*100,qpct.freq))

comb_guild2<-comb_guild %>% arrange(desc(network),plot_name,cycle) 
#View(comb_guild2)

write.csv(comb_guild2,'./data/NETN-MIDN-ERMN-NCRN_guild_invasives.csv', row.names=F)

#-----------------
# Invasives by species
#-----------------
invlist<-read.csv("./data/Invasive_List.csv")
invlist_MIDN<-invlist %>% filter(MIDN==1) %>% droplevels() # only includes species on indicator list since 2007
invlist_NCRN<-invlist %>% filter(NCRN==1) %>% droplevels() # only includes species on indicator list since 2007

#---NETN Invasives by species
netn_species<-read.csv("./data/NETN/NETN_invasive_species_data.csv")

netn_species<-netn_species %>% mutate(network='NETN') %>% 
  select(network,Unit_Code:qpct.freq)

colnames(netn_species)<-c("network","park","plot_name","cycle",'species',"plot.freq",
                        "avg.cover",'quad.freq','qpct.freq')

netn_species_final<-merge(netn_species,comb_totinv[,c('plot_name','cycle','lat.rank')], 
                          by=c('plot_name', 'cycle'), all.x=T)
netn_species_final<-netn_species_final %>% 
  select(network, park, plot_name, lat.rank, species, everything())

head(netn_species_final)

netn_check<-netn_species_final %>% group_by(plot_name, species) %>% 
  summarise(numplots=length(cycle)) %>% filter(numplots>3)

nrow(netn_check)
#---MIDN Invasives by species
midn_species<-read.csv("./data/MIDN/MIDN_invasive_species_data.csv")
names(midn_species)

midn_species<-midn_species %>% mutate(network='MIDN') %>% 
  select(network,Unit_Code:qpct.freq)

colnames(midn_species)<-c("network","park","plot_name","cycle",'species',"plot.freq",
                          "avg.cover",'quad.freq','qpct.freq')

midn_species_final<-merge(midn_species,comb_totinv[,c('plot_name','cycle','lat.rank')], 
                          by=c('plot_name', 'cycle'), all.x=T)

# species list for 4-year status
midn_species_final<-midn_species_final %>% 
  select(network, park, plot_name, lat.rank, species, everything())

# species list for trend analysis
midn_species_trends <- midn_species_final %>% filter(species %in% invlist_MIDN$Latin_Name) %>% droplevels()
#View(midn_species_trends)
nrow(midn_species_final)
nrow(midn_species_trends)

#---ERMN Invasives by species
ermn_species<-read.csv('./data/ERMN/ERMN_Sp_Invasives20190528.csv')
head(ermn_species)

ermn_species2<-merge(ermn_species,comb_totinv[,c('plot_name','cycle','lat.rank')], by=c('plot_name','cycle'),all.x=T)

ermn_species2<-ermn_species2 %>% select(network,park,plot_name,lat.rank,Latin_name,cycle,plot.freq,avg.cover,quad.freq,qpct.freq)
names(ermn_species2)[names(ermn_species2)=="Latin_name"]<-'species'

invspp<-invlist$Latin_Name
ermn_unmatch<-ermn_species2 %>% filter(!(species %in% invspp))
sort(unique(ermn_unmatch$species)) #all species accounted for now.
ermn_species3<- merge(ermn_species2, invlist, by.x='species',by.y="Latin_Name",all.x=T)
names(ermn_species3) # Persicaria longiseta is duplicated because two synonyms- need to fix

ermn_species4<- ermn_species3 %>% mutate(species=ifelse(Accepted=='Y', paste0(species), paste0(Accepted.Name))) %>% 
  select(network, park, plot_name, lat.rank, species, cycle, plot.freq, avg.cover, quad.freq, qpct.freq) # replaced old with new names

ermn_species_final<-ermn_species4 %>% group_by(network, park, plot_name, lat.rank, species, cycle) %>% 
                                      summarise(plot.freq=sum(plot.freq), avg.cover=sum(avg.cover), 
                                                quad.freq=sum(quad.freq), qpct.freq=sum(qpct.freq)) %>% ungroup()

ermn_check<-ermn_species_final %>% group_by(plot_name, species) %>% 
  summarise(numplots=length(plot.freq)) %>% filter(numplots>3)
nrow(ermn_check) #0

#---NCRN Invasives by species
ncrn_species<-read.csv("./data/NCRN/NCRN_species_invasives.csv")[,-1]
nrow(ncrn_species) #280140
nrow(unique(ncrn_species)) #56020- each record is duplicated 5 times in the data

ncrn_unmatch<-ncrn_species %>% filter(!(species %in% invspp))
sort(unique(ncrn_unmatch$species)) #all species accounted for.

ncrn_species2<-unique(ncrn_species)
nrow(ncrn_species2) #56020
head(ncrn_species2)

# Combine all datasets
ncrn_species3<-merge(ncrn_species2,comb_totinv[,c('plot_name','cycle','lat.rank')], 
                          by=c('plot_name', 'cycle'), all.x=T)


ncrn_species4<- merge(ncrn_species3, invlist, by.x='species',by.y="Latin_Name",all.x=T)

# species for 4-year status
ncrn_species5<- ncrn_species4 %>% mutate(species=ifelse(Accepted=='Y', paste0(species), paste0(Accepted.Name))) %>% 
  select(network, park, plot_name, lat.rank, species, cycle, plot.freq, avg.cover, quad.freq, qpct.freq) # replaced old with new names

View(ncrn_species5)
ncrn_species_final<-ncrn_species5 %>% group_by(network, park, plot_name, lat.rank, species, cycle) %>% 
  summarise(plot.freq=sum(plot.freq), avg.cover=sum(avg.cover), 
            quad.freq=sum(quad.freq), qpct.freq=sum(qpct.freq)) %>% ungroup()

ncrn_check<-ncrn_species_final %>% group_by(plot_name, species) %>% 
  summarise(numplots=length(plot.freq)) %>% filter(numplots>3)
nrow(ncrn_check) #0

# species list for trend analysis
ncrn_species_trends <- ncrn_species_final %>% filter(species %in% invlist_NCRN$Latin_Name) %>% droplevels()
#View(ncrn_species_trends)

# Combine network data
names(netn_species_final);names(midn_species_final);names(ermn_species_final);names(ncrn_species_final);

comb_species<-rbind(netn_species_final, midn_species_final, ermn_species_final, ncrn_species_final)

table(comb_species$network)

table(comb_species$park,comb_species$cycle)

comb_species2<-comb_species %>% arrange(desc(network),plot_name,cycle) 

write.csv(comb_species2,'./data/NETN-MIDN-ERMN-NCRN_species_invasives_status.csv', row.names=F)

# Combine network data
comb_species_trends<-rbind(netn_species_final, midn_species_trends, ermn_species_final, ncrn_species_trends)

comb_species_check<-comb_species_trends %>% group_by(plot_name, species) %>% 
  summarise(numplots=length(plot.freq)) %>% filter(numplots>3)

nrow(comb_species_check)#0

write.csv(comb_species_trends,'./data/NETN-MIDN-ERMN-NCRN_species_invasives.csv', row.names=F)

levels(comb_species_trends$species)
