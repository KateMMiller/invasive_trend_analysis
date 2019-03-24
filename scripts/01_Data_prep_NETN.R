#-----------------------------------
# Code for invasive trend analysis
#-----------------------------------
library(forestNETN)
options(scipen=100,digits=6)
#----------------------------------
# Function to remove ACAD.029.2010 
#----------------------------------
rmRejected<-function(df){
  df2<-subset(df,!(Plot_Name=="ACAD-029" & Year==2010))
  print(paste('Original # Visits:',length(unique(df$Event_ID)),sep=''))
  print(paste('New # Visits:',length(unique(df2$Event_ID)),sep=''))
  return(df2)}

#-----------------------------
# Connect to backend forest database and import data and lookup tables for the queries below
importData()
quadsamp$numHerbPlots<-apply(quadsamp[,c(15:22)], 1,sum)

park.plots2<-joinLocEvent(from=2007, to=2018, QAQC=F,rejected=F,locType='VS',output='short') 
park.plots1<-merge(park.plots2, quadsamp[,c("Event_ID","numHerbPlots")], by="Event_ID", all.x=T)
park.plots<-rmRejected(park.plots1)

#---------------------------
# Summarise data by species for each park
#---------------------------
quads1<-joinQuadData(speciesType='invasive',park='all',from=2007,to=2018,QAQC=F)
quads1$Latin_Name<-as.factor(quads1$Latin_Name)
quads1<-rmRejected(quads1)
quads1<- quads1 %>% mutate(cycle= case_when(Year<=2010~1, 
                                                      between(Year,2011,2014)~2, 
                                                      between(Year,2015,2018)~3),
                           Latin_Name=ifelse(is.na(Latin_Name),'noinvspp',paste(Latin_Name)))

quadscov<-quads1 %>% select(Event_ID,Unit_Code,Plot_Name,cycle,Latin_Name,avg.cover) %>% 
  spread(Latin_Name,avg.cover,fill=0)

quadscov1<-quadscov %>% mutate(Euonymus=`Euonymus alatus`+ Euonymus + `Euonymus fortunei`, 
                               Ligustrum= Ligustrum + `Ligustrum obtusifolium`+ `Ligustrum vulgare`,
                               `Lonicera - Exotic`= `Lonicera - Exotic`+ `Lonicera morrowii`,
                               Vincetoxicum= `Vincetoxicum nigrum` + Vincetoxicum) %>% 
                        select(-`Euonymus atropurpureus`, -`Euonymus alatus`, -`Euonymus fortunei`,
                               -`Ligustrum obtusifolium`, -`Ligustrum vulgare`, - `Lonicera morrowii`, 
                               -`Persicaria longiseta`, -`Persicaria posumbu`, -`Vincetoxicum nigrum`)


quadsfreq<-quads1 %>% select(Event_ID,Unit_Code,Plot_Name,cycle,Latin_Name,avg.freq) %>% 
  spread(Latin_Name,avg.freq,fill=0)

quadsfreq1<-quadsfreq %>% mutate(Euonymus=`Euonymus alatus`+ Euonymus + `Euonymus fortunei`, 
                                 Ligustrum= Ligustrum + `Ligustrum obtusifolium`+ `Ligustrum vulgare`,
                                 `Lonicera - Exotic`= `Lonicera - Exotic`+ `Lonicera morrowii`,
                                 Vincetoxicum= `Vincetoxicum nigrum` + Vincetoxicum) %>% 
                          select(-`Euonymus atropurpureus`, -`Euonymus alatus`, -`Euonymus fortunei`,
                                 -`Ligustrum obtusifolium`, -`Ligustrum vulgare`, - `Lonicera morrowii`, 
                                 -`Persicaria longiseta`, -`Persicaria posumbu`, -`Vincetoxicum nigrum`)

quadscov2<-quadscov1 %>% gather("Latin_Name","avgcov", -(Event_ID:cycle))
quadsfreq2<-quadsfreq1 %>% gather("Latin_Name","avgfreq",-(Event_ID:cycle))
nrow(quadsfreq2)
quads2<-merge(quadscov2,quadsfreq2,by=c("Event_ID","Unit_Code","Plot_Name","cycle","Latin_Name"), all.x=T, all.y=T)
names(quads2)
table(complete.cases(quads2$avgcov))
table(complete.cases(quads2$avgfreq))


netnspp<-makeSppList('invasive',from=2007, to=2018)
netnspp$Latin_Name[is.na(netnspp$Latin_Name)]<-'noinvspp'
table(complete.cases(netnspp$Latin_Name))
netnspp1<-rmRejected(netnspp)
netnspp1<- netnspp1 %>% mutate(cycle= case_when(Year<=2010~1, 
                                            between(Year,2011,2014)~2, 
                                            between(Year,2015,2018)~3),
                               present=1) %>% 
                        select(Event_ID,Unit_Code,Plot_Name,cycle,Latin_Name, present)

netnspp2<-netnspp1 %>% spread(Latin_Name, present, fill=0)
netnspp2<-netnspp2 %>% mutate(Euonymus=`Euonymus alatus`+ Euonymus + `Euonymus fortunei`, 
                              Ligustrum= Ligustrum + `Ligustrum obtusifolium`+ `Ligustrum vulgare`,
                              `Lonicera - Exotic`= `Lonicera - Exotic`+ `Lonicera morrowii`,
                              Vincetoxicum= `Vincetoxicum nigrum` + Vincetoxicum) %>% 
                       select(-`Euonymus atropurpureus`, -`Euonymus alatus`, -`Euonymus fortunei`,
                              -`Ligustrum obtusifolium`, -`Ligustrum vulgare`, - `Lonicera morrowii`, 
                              -`Persicaria longiseta`, -`Persicaria posumbu`, -`Vincetoxicum nigrum`)
netnspp3<-netnspp2 %>% gather('Latin_Name','plotfreq',-(Event_ID:cycle))

sppcomb<-merge(netnspp3, quads2, by=c('Event_ID','Unit_Code','Plot_Name','cycle','Latin_Name'), all.x=T,all.y=T)
#View(sppcomb)

sppcomb$avgcov[is.na(sppcomb$avgcov)]<-0
sppcomb$avgfreq[is.na(sppcomb$avgfreq)]<-0

head(sppcomb)
spp_comb_final<-sppcomb %>% mutate(plot.freq=plotfreq, avg.cover=avgcov,quad.freq=avgfreq) %>% 
  select(-Event_ID,-plotfreq,-avgcov, -avgfreq) %>% arrange(Plot_Name,cycle)
head(spp_comb_final)

write.csv(spp_comb_final, './data/NETN/NETN_invasive_species_data.csv')

sppcomb2<-sppcomb %>% filter(Latin_Name!='noinvspp') %>% group_by(Unit_Code,cycle,Latin_Name) %>% 
                      summarise(avgcov=mean(avgcov), avgfreq=mean(avgfreq), 
                                plotfreq=sum(plotfreq), numplots=n())
head(sppcomb2)

write.csv(sppcomb2,'./data/NETN/NETN_park-level_invasive_species_summary.csv', row.names=FALSE)

topspp<-sppcomb2 %>% filter(cycle==3) %>% group_by(Latin_Name) %>% summarise(plotfreq=sum(plotfreq)) %>% arrange(-plotfreq)
#View(topspp)

#---------------------------
# Summarize data by guild
#---------------------------
# Need to set guilds to only count a species once
table(plants$Tree, plants$Shrub)
table(plants$Shrub, plants$Vine)
table(plants$Graminoid,plants$Herbaceous)

plants<-plants %>% mutate(Tree=ifelse(Tree+Shrub>1,0,Tree),
  Shrub=ifelse(Tree+Shrub>1,1,Shrub),Shrub=ifelse(Vine==1,1,Shrub)) 
# Spp like RHACAT that we treat as shrubs, but that have to be on tree list 
# because they're sometimes >10cm DBH, are set to only be shrubs here.

#---------------------------------
# Plot Frequency and Plot Richness
spplist1<-makeSppList(speciesType='invasive')
spplist<-rmRejected(spplist1)
names(spplist)

spplist<-spplist %>% mutate(Tree=ifelse(Tree+Shrub>1,0,Tree),
  Shrub=ifelse(Tree+Shrub>1,1,Shrub))

spplist[,c(13:24,27:33)][is.na(spplist[,c(13:24,27:33)])]<-0
spplist$pres.temp<-ifelse(rowSums(spplist[,c(13:24)])>0,1,0)

spplist2<-spplist %>% group_by(Event_ID,Tree,Shrub,Herbaceous,Graminoid) %>% 
  summarise(numPlotSpp=sum(pres.temp),plotFreq=ifelse(sum(pres.temp)>0,1,0))

spplist3<-merge(park.plots,spplist2,by="Event_ID",all.x=T)

plotFreq<-spplist3 %>% select(Event_ID,Tree:Graminoid,plotFreq) %>% gather(guild,present,Tree:Graminoid) %>%  
  filter(present==1) %>% select(-present) %>% group_by(Event_ID,guild) %>% summarise(plotFreq=ifelse(sum(plotFreq)>0,1,0)) 

head(plotFreq)

numPlotSpp<-spplist3 %>% select(Event_ID,Tree:Graminoid,numPlotSpp) %>% 
  gather(guild,present,Tree:Graminoid) %>% filter(present==1) %>% select(-present) %>% 
  group_by(Event_ID,guild) %>% summarise(numPlotSpp=sum(numPlotSpp))

#---------------------------
# Quadrat richness by guild
quad<-joinQuadData(from=2007, to=2018, QAQC=F,locType='VS',output='short',speciesType='invasive') 
quad1<-rmRejected(quad)

quad1<-quad1 %>% mutate(Tree=ifelse(Tree+Shrub>1,0,Tree),
  Shrub=ifelse(Tree+Shrub>1,1,Shrub),Shrub=ifelse(Vine==1,1,Shrub))

quad.r<-quad1 %>% group_by(Event_ID,Tree,Shrub,Herbaceous,Graminoid) %>% 
  summarise(numHerbPlots=first(numHerbPlots),UC=sum(UC),UR=sum(UR),MR=sum(MR),BR=sum(BR),
    BC=sum(BC),BL=sum(BL),ML=sum(ML),UL=sum(UL))

quad.r<-quad.r %>% mutate(avg.quad.r=(UC+UR+MR+BR+BC+BL+ML+UL)/numHerbPlots) %>% select(-(numHerbPlots:UL))

quad.r2<-quad.r %>% gather(guild,present,Tree:Graminoid) %>% filter(present==1) %>% select(-present)

quad.r2$avg.quad.r[is.na(quad.r2$avg.quad.r)]<-0
#quad.r2 has average quadrat exotic richness
head(quad.r2)
#--------------------------
# Summarise quadrat cover and frequency by guild
guild1<-sumQuadGuilds(speciesType='invasive')
guild<-rmRejected(guild1)

#--------------------------
# Combine metrics
#--------------------------
comb1<-park.plots %>% arrange(Plot_Name,cycle) %>% 
  mutate(Tree=1,Shrub=1,Herbaceous=1,Graminoid=1) %>% 
  gather(guild,present,Tree:Graminoid) %>% filter(present==1) %>% select(-present)

comb2<-merge(comb1,quad.r2,by=c("Event_ID",'guild'),all.x=T)
comb3<-merge(comb2,plotFreq,by=c("Event_ID",'guild'),all.x=T)
comb4<-merge(comb3,guild[,c("Event_ID","guild","avg.cover","avg.freq")],by=c("Event_ID",'guild'),all.x=T)
comb5<-merge(comb4,numPlotSpp,by=c("Event_ID",'guild'),all.x=T)

comb5[,14:18][is.na(comb5[,14:18])]<-0
names(comb5)

comb5<-comb5 %>% mutate(quadFreq=avg.freq*numHerbPlots) %>% select(Event_ID,Location_ID,Unit_Code:cycle,guild,
  plotFreq, avg.quad.r, avg.cover, quadFreq, avg.freq, numPlotSpp)

colnames(comb5)<-c("Event_ID","Location_ID","Unit_Code","Plot_Name","Plot_Number","X_Coord","Y_Coord",    
  "Panel","Year","Event_QAQC","cycle","guild","plot.freq","avg.quad.r","avg.cover","quad.freq","qpct.freq","numPlotSpp")

comb5<-comb5 %>% arrange(Plot_Name,cycle)

write.csv(comb5,'./data/NETN/NETN_invasive_guild_data.csv',row.names=F)

#-------------------------------------
# Summarize total invasive data
#-------------------------------------
# Plot Frequency and Plot Richness of all invasives
spplist1<-makeSppList(speciesType='invasive')
spplist<-rmRejected(spplist1)

names(spplist)
spplist[,c(13:24,27:33)][is.na(spplist[,c(13:24,27:33)])]<-0
spplist$pres.temp<-ifelse(rowSums(spplist[,c(13:24)])>0,1,0)

plotFreq<-spplist %>% group_by(Event_ID) %>% 
  summarise(numPlotSpp=sum(pres.temp),plotFreq=ifelse(sum(pres.temp)>0,1,0))

head(plotFreq)
#---------------------------
# Quadrat richness and average cover of all invasives
quad<-joinQuadData(from=2007, to=2018, QAQC=F,locType='VS',output='short',speciesType='invasive') 
quad1<-rmRejected(quad) %>% arrange(Plot_Name)

quad.r<-quad1 %>% group_by(Event_ID) %>% 
  summarise(numHerbPlots=first(numHerbPlots),UC=sum(UC),UR=sum(UR),MR=sum(MR),BR=sum(BR),
    BC=sum(BC),BL=sum(BL),ML=sum(ML),UL=sum(UL), avg.cover=sum(avg.cover))

quad.r2<-quad.r %>% mutate(avg.quad.r=(UC+UR+MR+BR+BC+BL+ML+UL)/numHerbPlots, 
  quadFreq=ifelse(UC>0,1,0)+ifelse(UR>0,1,0)+ifelse(MR>0,1,0)+ifelse(BR>0,1,0)+
    ifelse(BC>0,1,0)+ifelse(BL>0,1,0)+ifelse(ML>0,1,0)+ifelse(UL>0,1,0),
  qPctFreq=quadFreq/numHerbPlots) %>% 
  select(-(numHerbPlots:UL))

head(quad.r2)

quad.r2[,c(2:5)][(is.na(quad.r2[,c(2:5)]))] <-0
head(quad.r2) #quad.r2 has average quadrat exotic richness

#--------------------------
# Combine metrics
#--------------------------
comb1<-merge(park.plots,plotFreq,by='Event_ID',all.x=T) 
comb2<-merge(comb1,quad.r2,by=c("Event_ID"),all.x=T)
head(comb2)

comb2[,13:18][is.na(comb2[,13:18])]<-0
names(comb2)

comb2<-comb2 %>% select(Event_ID:cycle,  plotFreq, avg.quad.r, avg.cover, quadFreq, qPctFreq, numPlotSpp)
colnames(comb2)<-c("Event_ID","Location_ID","Unit_Code","Plot_Name","Plot_Number","X_Coord","Y_Coord",    
  "Panel","Year","Event_QAQC","cycle","plot.freq","avg.quad.r","avg.cover","quad.freq","qpct.freq","numPlotSpp")
comb2<-comb2 %>% arrange(Plot_Name,cycle)
write.csv(comb2,'./data/NETN/NETN_invasive_total_data.csv',row.names=F)

#-----------------------------
# Create invasive species list
#-----------------------------
library(RODBC);library(tidyverse)
db<-odbcConnect("NETNFVM")
plants<-sqlFetch(db,'tlu_plants')
odbcClose(db)

notinv<-c("Abies concolor",'Aesculus hippocastanum','Catalpa bignonioides','Catalpa speciosa',
  'Cladrastis kentukea','Larix decidua','Malus pumila','Picea abies','Pinus nigra','Pinus sylvestris',
  'Prunus avium','Prunus cerasus','Prunus domestica','Pyrus','Salix alba','Ulmus procera')

invtrees<-plants %>% filter(Tree==1 & Exotic==1 & !Latin_Name %in% notinv ) %>% select(Latin_Name) %>% 
  arrange(Latin_Name) %>% droplevels()

# TSN for bamboo species are 42023,18848. They weren't on indicator list, so need to be added
plants2<-plants %>% mutate(Tree=ifelse(Tree+Shrub>1,0,Tree), Shrub=ifelse(Tree+Shrub>1,1,Shrub),
  Shrub=ifelse(Vine==1,1,Shrub)) %>%
  arrange(desc(Tree),desc(Shrub),desc(Herbaceous),desc(Graminoid),Latin_Name)

plants2<-plants2 %>% filter(Indicator_Invasive_NETN==T |(Indicator_MIDN==1 & Exotic==1)|TSN %in% c(42023,18848)|
    (Latin_Name %in% invtrees$Latin_Name)) %>% 
  select(TSN,Latin_Name,Tree,Shrub,Herbaceous,Graminoid) %>% droplevels() %>% 
  arrange(desc(Tree),desc(Shrub),desc(Herbaceous),desc(Graminoid),Latin_Name)

write.csv(plants2,'./data/MIDN-NETN_inv_spp_list.csv', row.names=F)
