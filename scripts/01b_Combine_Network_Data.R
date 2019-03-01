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
ermn_totinv<-read.csv("./data/ERMN/ERMN_total_Invasives20190128.csv")
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

# Need to add NAs for all plots in C1 for COLO, to make plotting easier
COLO.c2<-comb_totinv %>% filter(park=='COLO' & cycle==2) %>% droplevels() 
# taking COLO C2 and turn it into C1 with NAs
COLO.c1<-COLO.c2 %>% mutate(cycle=1,year=year-4)
names(COLO.c1)
COLO.c1[,c(9:14)][!is.na(COLO.c1[,c(9:14)])]<-NA
comb_totinv2<-rbind(comb_totinv,COLO.c1) 
names(comb_totinv2)

comb_totinv2<-comb_totinv2 %>% arrange(desc(network),plot_name,cycle)

comb_totinv2<-comb_totinv2 %>% mutate(qpct.freq=ifelse(network!='NCRN',qpct.freq*100,qpct.freq))

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
midn_guild<- midn_guild %>% mutate(cycle=case_when(park=='COLO' & cycle==2 ~ 3,
  park=='COLO' & cycle==1 ~ 2,
  park!='COLO' & cycle==1 ~ 1,
  park!='COLO' & cycle==2 ~ 2,
  park!='COLO' & cycle==3 ~ 3)) 

#---ERMN Invasives by guild
ermn_guild<-read.csv("./data/ERMN/ERMN_Guild_Invasives20190128.csv")
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

# Need to add NAs for all plots in C1 for COLO, to make plotting easier
COLO.c2<-comb_guild %>% filter(park=='COLO' & cycle==2) %>% droplevels() 
# taking COLO C2 and turn it into C1 with NAs
COLO.c1<-COLO.c2 %>% mutate(cycle=1,year=year-4)
COLO.c1[,c(10:15)][!is.na(COLO.c1[,c(10:15)])]<-NA
comb_guild2<-rbind(comb_guild,COLO.c1) 

comb_guild2<-comb_guild2 %>% mutate(qpct.freq=ifelse(network!='NCRN',qpct.freq*100,qpct.freq))

comb_guild2<-comb_guild2 %>% arrange(desc(network),plot_name,cycle) 
#View(comb_guild2)

write.csv(comb_guild2,'./data/NETN-MIDN-ERMN-NCRN_guild_invasives.csv', row.names=F)
