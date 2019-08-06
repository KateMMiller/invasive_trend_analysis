#-----------------------------------
# Code for invasive trend analysis
#-----------------------------------
library(forestNETN)
library(tidyverse)
options(scipen=100,digits=6)

invlist<-read.csv("./data/Invasive_List.csv")
head(invlist)
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
importData(odbc='NETN_Field')
quadsamp$numHerbPlots<-apply(quadsamp[,c(15:22)], 1, sum)

park.plots2<-joinLocEvent(from=2007, to=2018, QAQC=F,rejected=F,locType='VS',output='short') 
park.plots1<-merge(park.plots2, quadsamp[,c("Event_ID","numHerbPlots")], by="Event_ID", all.x=T)
park.plots<-rmRejected(park.plots1)

#---------------------------
# Summarise data by species for each park
#---------------------------
# Quadrat data summary by species
quads1<-joinQuadData(speciesType='exotic',park='all',from=2007,to=2018,QAQC=F)
quads1$Latin_Name<-as.factor(quads1$Latin_Name)
quads1<-rmRejected(quads1)
quads_inv<- quads1 %>% filter(Latin_Name %in% invlist$Latin_Name) %>% droplevels()
quads2<-merge(park.plots, quads_inv[,c('Event_ID','numHerbPlots','UC','UR','MR','BR','BC','BL','ML','UL',
                                       'Latin_Name','Tree','Shrub','Vine','Herbaceous','Graminoid','Exotic',
                                       'avg.cover','avg.freq')], by='Event_ID', all.x=T)
quads3<-quads2 %>% mutate(cycle= case_when(Year<=2010~1, 
                                                      between(Year,2011,2014)~2, 
                                                      between(Year,2015,2018)~3),
                           Latin_Name=ifelse(is.na(Latin_Name),'noinvspp',paste(Latin_Name)))

quadscov<-quads3 %>% select(Event_ID,Unit_Code,Plot_Name,cycle,Latin_Name,avg.cover) %>% 
  spread(Latin_Name,avg.cover,fill=0)

names(quadscov)

quadscov1<-quadscov %>% mutate(`Euonymus alatus`=`Euonymus alatus`+ Euonymus, 
                               Ligustrum= Ligustrum + `Ligustrum obtusifolium`+ `Ligustrum vulgare`,
                               `Lonicera - Exotic`= `Lonicera - Exotic`+ `Lonicera morrowii`,
                               Vincetoxicum= `Vincetoxicum nigrum` + Vincetoxicum) %>% 
                        select(-`Euonymus`,
                               -`Ligustrum obtusifolium`, -`Ligustrum vulgare`, - `Lonicera morrowii`, 
                                -`Vincetoxicum nigrum`)


quadsfreq<-quads3 %>% select(Event_ID,Unit_Code,Plot_Name,cycle,Latin_Name,avg.freq) %>% 
  spread(Latin_Name,avg.freq,fill=0)

quadsfreq1<-quadsfreq %>% mutate(`Euonymus alatus`=`Euonymus alatus`+ Euonymus, 
                                 Ligustrum= Ligustrum + `Ligustrum obtusifolium`+ `Ligustrum vulgare`,
                                 `Lonicera - Exotic`= `Lonicera - Exotic`+ `Lonicera morrowii`,
                                 Vincetoxicum= `Vincetoxicum nigrum` + Vincetoxicum) %>% 
                          select(-`Euonymus`,
                                 -`Ligustrum obtusifolium`, -`Ligustrum vulgare`, - `Lonicera morrowii`, 
                                 -`Vincetoxicum nigrum`)

quadscov2<-quadscov1 %>% gather("Latin_Name","avgcov", -(Event_ID:cycle))
quadsfreq2<-quadsfreq1 %>% gather("Latin_Name","avgfreq",-(Event_ID:cycle))
nrow(quadsfreq2)
quads2<-merge(quadscov2,quadsfreq2,by=c("Event_ID","Unit_Code","Plot_Name","cycle","Latin_Name"), all.x=T, all.y=T)
names(quads2)
table(complete.cases(quads2$avgcov))
table(complete.cases(quads2$avgfreq))

# Add plot-level species occurence
netnspp<-makeSppList('all',from=2007, to=2018)
netnspp2<-netnspp %>% filter(Latin_Name %in% invlist$Latin_Name) %>% droplevels()
netnspp3<-merge(park.plots,netnspp2[,c('Event_ID','Latin_Name','Tree','Shrub','Herbaceous','Graminoid','Exotic',
                                       'avg.quad.cover','avg.quad.freq')], by='Event_ID',all.x=T)
netnspp3$Latin_Name[is.na(netnspp3$Latin_Name)]<-'noinvspp'
table(complete.cases(netnspp3$Latin_Name))
netnspp4<-rmRejected(netnspp3)
netnspp5<- netnspp4 %>% mutate(cycle= case_when(Year<=2010~1, 
                                            between(Year,2011,2014)~2, 
                                            between(Year,2015,2018)~3),
                               present=1) %>% 
                        select(Event_ID,Unit_Code,Plot_Name,cycle,Latin_Name, present)

netnspp6<-netnspp5 %>% spread(Latin_Name, present, fill=0)
names(netnspp6)

netnspp7<-netnspp6 %>% mutate(`Euonymus alatus`=`Euonymus alatus`+ Euonymus, 
                              Ligustrum= Ligustrum + `Ligustrum obtusifolium`+ `Ligustrum vulgare`,
                              `Lonicera - Exotic`= `Lonicera - Exotic`+ `Lonicera morrowii`,
                              Vincetoxicum= `Vincetoxicum nigrum` + Vincetoxicum) %>% 
                       select(-`Euonymus`,
                              -`Ligustrum obtusifolium`, -`Ligustrum vulgare`, - `Lonicera morrowii`, 
                              -`Vincetoxicum nigrum`)
netnspp8<-netnspp7 %>% gather('Latin_Name','plotfreq',-(Event_ID:cycle))

netnspp8<-netnspp8 %>% mutate(plotfreq=ifelse(plotfreq>1,1,plotfreq))
head(netnspp8)

sppcomb<-merge(netnspp8, quads2, by=c('Event_ID','Unit_Code','Plot_Name','cycle','Latin_Name'), all.x=T,all.y=T)

sppcomb$avgcov[is.na(sppcomb$avgcov)]<-0
sppcomb$avgfreq[is.na(sppcomb$avgfreq)]<-0
sppcomb$plotfreq[is.na(sppcomb$plotfreq)]<-0

sppcomb<-sppcomb %>% filter(Latin_Name !="<NA>") %>% filter(Latin_Name!='noinvspp')
unique(sppcomb$Latin_Name)

spp_comb2<-sppcomb %>% mutate(plot.freq=plotfreq, avg.cover=avgcov, quad.freq=avgfreq, qpct.freq=avgfreq*100) %>% 
  select(-Event_ID,-plotfreq,-avgcov, -avgfreq) %>% arrange(Plot_Name,cycle)
spp_comb3<- merge(spp_comb2, invlist, by="Latin_Name",all.x=T)
spp_comb_final<- spp_comb3 %>% mutate(species=ifelse(Accepted=='Y', paste0(Latin_Name), paste0(Accepted.Name))) %>% 
  select(Unit_Code, Plot_Name, cycle, species, plot.freq, avg.cover, quad.freq, qpct.freq) # replaced old with new names

write.csv(spp_comb_final, './data/NETN/NETN_invasive_species_data.csv', row.names=FALSE)

sppcomb2<-sppcomb %>% group_by(Unit_Code,cycle,Latin_Name) %>% 
                      summarise(avgcov=mean(avgcov), avgfreq=mean(avgfreq), 
                                plotfreq=sum(plotfreq), numplots=n())
head(sppcomb2)

write.csv(sppcomb2,'./data/NETN/NETN_park-level_invasive_species_summary.csv', row.names=FALSE)

topspp<-sppcomb2 %>% filter(cycle==3) %>% group_by(Latin_Name) %>% summarise(plotfreq=sum(plotfreq)) %>% arrange(-plotfreq)
head(topspp)

#---------------------------
# Summarize data by guild
#---------------------------
# Need to set guilds to only count a species once
table(plants$Tree, plants$Shrub)
table(plants$Shrub, plants$Vine)
table(plants$Graminoid,plants$Herbaceous)

plants<-plants %>% mutate(Tree=ifelse(Tree+Shrub>1,0,Tree),
  Shrub=ifelse(Tree+Shrub>1,1,Shrub)) 
# Spp like RHACAT that we treat as shrubs, but that have to be on tree list 
# because they're sometimes >10cm DBH, are set to only be shrubs here.

#---------------------------------
# Plot Frequency and Plot Richness
prespplist1<-makeSppList('all',from=2007, to=2018)
prespplist2<-prespplist1 %>% filter(Latin_Name %in% invlist$Latin_Name) %>% droplevels()
prespplist3<-merge(park.plots,prespplist2[,c('Event_ID','Latin_Name','Tree','Shrub','Herbaceous','Graminoid','Exotic',
                                       'avg.quad.cover','avg.quad.freq')], by='Event_ID',all.x=T)
prespplist3$Latin_Name[is.na(prespplist3$Latin_Name)]<-'noinvspp'
prespplist4 <- rmRejected(prespplist3)
prespplist5 <- prespplist4 %>% mutate(cycle= case_when(Year<=2010~1, 
                                                between(Year,2011,2014)~2, 
                                                between(Year,2015,2018)~3),
                               present=1) %>% 
  select(Event_ID, Unit_Code, Plot_Name, cycle, Latin_Name, Tree:Graminoid, present)

spplist<-prespplist5 %>% mutate(Tree=ifelse(Tree+Shrub>1,0,Tree),
  Shrub=ifelse(Tree+Shrub>1,1,Shrub), present=ifelse(Latin_Name=="noinvspp",0,1))

sort(unique(spplist$Latin_Name))

spplist2<-spplist %>% group_by(Event_ID,Tree,Shrub,Herbaceous,Graminoid) %>% 
  summarise(numPlotSpp=sum(present),plotFreq=ifelse(sum(present)>0,1,0))

spplist3<-merge(park.plots,spplist2,by="Event_ID",all.x=T)

plotFreq<-spplist3 %>% select(Event_ID,Tree:Graminoid,plotFreq) %>% gather(guild,present,Tree:Graminoid) %>%  
  filter(present==1) %>% select(-present) %>% group_by(Event_ID,guild) %>% summarise(plotFreq=ifelse(sum(plotFreq)>0,1,0)) 

head(plotFreq)

numPlotSpp<-spplist3 %>% select(Event_ID,Tree:Graminoid,numPlotSpp) %>% 
  gather(guild,present,Tree:Graminoid) %>% filter(present==1) %>% select(-present) %>% 
  group_by(Event_ID,guild) %>% summarise(numPlotSpp=sum(numPlotSpp))

#---------------------------
# Quadrat richness by guild
quad<-joinQuadData(from=2007, to=2018, QAQC=F,locType='VS',output='short',speciesType='all') 
quad1<-rmRejected(quad)
names(quad1)

quad2<-quad1 %>% filter(Latin_Name %in% invlist$Latin_Name) %>% droplevels() %>% 
  select(Event_ID, UC:UL,Latin_Name:Shrub,Herbaceous,Graminoid,Exotic,avg.cover,avg.freq )

quad3<-merge(park.plots, quad2, by='Event_ID',all.x=T)

quad4<-quad3 %>% mutate(Tree=ifelse(Tree+Shrub>1,0,Tree),
  Shrub=ifelse(Tree+Shrub>1,1,Shrub))

quad.r<-quad3 %>% group_by(Event_ID,Tree,Shrub,Herbaceous,Graminoid) %>% 
  summarise(numHerbPlots=first(numHerbPlots),UC=sum(UC),UR=sum(UR),MR=sum(MR),BR=sum(BR),
    BC=sum(BC),BL=sum(BL),ML=sum(ML),UL=sum(UL))

quad.r<-quad.r %>% mutate(avg.quad.r=(UC+UR+MR+BR+BC+BL+ML+UL)/numHerbPlots) %>% select(-(numHerbPlots:UL))

quad.r2<-quad.r %>% gather(guild,present,Tree:Graminoid) %>% filter(present==1) %>% select(-present)

quad.r2$avg.quad.r[is.na(quad.r2$avg.quad.r)]<-0
#quad.r2 has average quadrat exotic richness
head(quad.r2)
#--------------------------
# Summarise quadrat cover and frequency by guild
quad<-joinQuadData(from=2007, to=2018, QAQC=F,locType='VS',output='short',speciesType='all') 
quad1<-rmRejected(quad)

quad2<-quad1 %>% filter(Latin_Name %in% invlist$Latin_Name) %>% droplevels() %>% 
  select(Event_ID,Tree,Shrub,Herbaceous,Graminoid, avg.cover, UC, UR, MR, BR, BC, BL, ML, UL)

quad3<-merge(park.plots,quad2, by='Event_ID',all.x=T)
quad4<-quad3 %>% group_by(Event_ID, Tree, Shrub, Herbaceous, Graminoid) %>% 
  summarise(avg.cover=sum(avg.cover),
            UC=ifelse(sum(UC)>0,1,0),UR=ifelse(sum(UR)>0,1,0),MR=ifelse(sum(MR)>0,1,0),BR=ifelse(sum(BR)>0,1,0),
            BC=ifelse(sum(BC)>0,1,0),BL=ifelse(sum(BL)>0,1,0),ML=ifelse(sum(ML)>0,1,0),UL=ifelse(sum(UL)>0,1,0),
            avg.freq=(UC+UR+MR+BR+BC+BL+ML+UL)/first(numHerbPlots)) %>% 
  mutate(guild= case_when(Tree==1 ~'Tree',
                          Shrub==1~'Shrub',
                          Herbaceous==1~'Herbaceous',
                          Graminoid==1~'Graminoid')) %>% ungroup() %>% select(Event_ID,guild,avg.cover,avg.freq)

park.plots2<-park.plots %>% mutate(Graminoid=1,Herbaceous=1,Fern=1,Shrub=1,Tree=1) %>%
  tidyr::gather(key=guild,value=pres,Graminoid:Tree) %>% select(-pres)

quads.comb1<-merge(park.plots2,quad4,by=c("Event_ID","guild"),all.x=T)
names(quads.comb1)

quads.comb1[,14:15][is.na(quads.comb1[,14:15])]<-0

guild<-quads.comb1 %>% select(Location_ID,Event_ID,Unit_Code:cycle,guild,avg.cover:avg.freq) %>%
  arrange(Plot_Name,Year,guild)

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

comb5<-comb5 %>% mutate(quadFreq=avg.freq*numHerbPlots) %>% select(Event_ID,Location_ID,Unit_Code:cycle,guild,
  plotFreq, avg.quad.r, avg.cover, quadFreq, avg.freq, numPlotSpp)
names(comb5)

colnames(comb5)<-c("Event_ID","Location_ID","Unit_Code","Plot_Name","Plot_Number","X_Coord","Y_Coord",    
  "Panel","Year","Event_QAQC","cycle","guild","plot.freq","avg.quad.r","avg.cover","quad.freq","qpct.freq","numPlotSpp")

comb5<-comb5 %>% arrange(Plot_Name,cycle)

write.csv(comb5,'./data/NETN/NETN_invasive_guild_data.csv',row.names=F)

#-------------------------------------
# Summarize total invasive data
#-------------------------------------
# Plot Frequency and Plot Richness of all invasives
prespplist1<-makeSppList('all',from=2007, to=2018)
prespplist2<-prespplist1 %>% filter(Latin_Name %in% invlist$Latin_Name) %>% droplevels() %>% 
  select(Event_ID, Latin_Name, tree.stems:addspp.present)

prespplist3<-merge(park.plots,prespplist2, by='Event_ID',all.x=T)
prespplist3$Latin_Name[is.na(prespplist3$Latin_Name)]<-'noinvspp'
prespplist4 <- rmRejected(prespplist3)
names(prespplist4)
prespplist5 <- prespplist4 %>% mutate(cycle= case_when(Year<=2010~1, 
                                                       between(Year,2011,2014)~2, 
                                                       between(Year,2015,2018)~3),
                                      present=1) %>% 
  select(Event_ID, Unit_Code, Plot_Name, cycle, Latin_Name, present)

spplist<-prespplist5 %>% mutate(present=ifelse(Latin_Name=="noinvspp",0,1))

spplist2<-spplist %>% group_by(Event_ID) %>% 
  summarise(numPlotSpp=sum(present),plotFreq=ifelse(sum(present)>0,1,0))

plotFreq<-merge(park.plots,spplist2,by="Event_ID",all.x=T) %>% arrange(Plot_Name,cycle)
head(plotFreq)

#---------------------------
# Quadrat richness and average cover of all invasives
quad<-joinQuadData(from=2007, to=2018, QAQC=F,locType='VS',output='short',speciesType='all') 
quad1<-rmRejected(quad)
names(quad1)

quad2<-quad1 %>% filter(Latin_Name %in% invlist$Latin_Name) %>% droplevels() %>% 
  select(Event_ID, UC:UL,Latin_Name,avg.cover,avg.freq )

quad3<-merge(park.plots, quad2, by='Event_ID',all.x=T)

quad.r<-quad3 %>% group_by(Event_ID) %>% 
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
comb1<-merge(park.plots,plotFreq[,c('Event_ID','numPlotSpp','plotFreq')],by='Event_ID',all.x=T) 
comb2<-merge(comb1,quad.r2,by=c("Event_ID"),all.x=T)
names(comb2)

comb2[,13:18][is.na(comb2[,13:18])]<-0
names(comb2)

comb2<-comb2 %>% select(Event_ID:cycle,  plotFreq, avg.quad.r, avg.cover, quadFreq, qPctFreq, numPlotSpp)
colnames(comb2)<-c("Event_ID","Location_ID","Unit_Code","Plot_Name","Plot_Number","X_Coord","Y_Coord",    
  "Panel","Year","Event_QAQC","cycle","plot.freq","avg.quad.r","avg.cover","quad.freq","qpct.freq","numPlotSpp")
comb2<-comb2 %>% arrange(Plot_Name,cycle)
write.csv(comb2,'./data/NETN/NETN_invasive_total_data.csv',row.names=F)

