#-----------------------------------
# Code for invasive trend analysis
#-----------------------------------
library(forestMIDN)
library(tidyverse)
options(scipen=100,digits=6)

invlist<-read.csv("./data/Invasive_List.csv")
head(invlist)
#++++++++++++++++++++++++++++++++++
# NOTE: These summaries are only include exotic species that were on the indicator list
# since 2007 to ensure that comparisons across cycles are consistent. 
#++++++++++++++++++++++++++++++++++

# Exotic Species that were added to indicator list after 2007.
invlist_trends<-invlist %>% filter(MIDN == 1) %>% droplevels() # removes species added later

#----------------------------------
# Function to remove PETE-185 and COLO-380
#----------------------------------
rmRejected<-function(df){
  df2<-subset(df,!(Unit_Code=="PETE" & Plot_Number=="185") & 
      !(Unit_Code=="COLO" & Plot_Number=="380"))
  print(paste('Original # Plots:',length(unique(df$Plot_Name)),sep=''))
  print(paste('New # Plots:',length(unique(df2$Plot_Name)),sep=''))
  return(df2)}

#-----------------------------
# Connect to backend forest database and import data and lookup tables for the queries below
importData()
quadsamp$numQuadrats<-apply(quadsamp[,c(3:14)], 1, sum)

park.plots2<-joinLocEvent(from=2007, to=2018, QAQC=F,rejected=F,locType='VS',output='short') 
park.plots1<-merge(park.plots2, quadsamp[,c("Event_ID","numQuadrats")], by="Event_ID", all.x=T)
park.plots<-rmRejected(park.plots1)

#---------------------------
# Summarise data by species for each park
#---------------------------
# Quadrat data summary by species
quads1<-joinQuadData(speciesType='exotic',park='all',from=2007,to=2018,QAQC=F)
quads1$Latin_Name<-as.factor(quads1$Latin_Name)
quads1<-rmRejected(quads1)
head(quads1)

quads_inv <- quads1 %>% filter(Latin_Name %in% invlist$Latin_Name) %>% droplevels() %>% select(Event_ID,numQuadrats:CC,Latin_Name,
                                                                                    Tree,Shrub,Vine,Herbaceous,Graminoid,
                                                                                    Exotic,avg.cover,avg.freq)
head(quads_inv)

quads2<-merge(park.plots, quads_inv, by='Event_ID',all.x=T)

quads3<- quads2 %>% mutate(cycle2=case_when(Unit_Code=='COLO' & cycle=="Cycle2" ~ 3,
                                           Unit_Code=='COLO' & cycle=="Cycle1" ~ 2,
                                           Unit_Code!='COLO' & cycle=="Cycle1" ~ 1,
                                           Unit_Code!='COLO' & cycle=="Cycle2" ~ 2,
                                           Unit_Code!='COLO' & cycle=="Cycle3" ~ 3),
                           cycle=cycle2,
                           Latin_Name=ifelse(is.na(Latin_Name),'noinvspp',paste(Latin_Name))) %>% select(-cycle2) 

quadscov<-quads3 %>% select(Event_ID,Unit_Code,Plot_Name,cycle,Latin_Name,avg.cover) %>% 
  spread(Latin_Name,avg.cover,fill=0)

names(quadscov)

quadscov1<-quadscov %>% mutate(`Lonicera - Exotic`= `Lonicera - Exotic`+ `Lonicera maackii`) %>% select(- `Lonicera maackii`)

quadsfreq<-quads3 %>% select(Event_ID,Unit_Code,Plot_Name,cycle,Latin_Name,avg.freq) %>% 
  spread(Latin_Name,avg.freq,fill=0)

quadsfreq1<-quadsfreq %>% mutate(`Lonicera - Exotic`= `Lonicera - Exotic`+ `Lonicera maackii`) %>% select(- `Lonicera maackii`)

quadscov2<-quadscov1 %>% gather("Latin_Name","avgcov", -(Event_ID:cycle))
quadsfreq2<-quadsfreq1 %>% gather("Latin_Name","avgfreq",-(Event_ID:cycle))
nrow(quadsfreq2)
quads2<-merge(quadscov2,quadsfreq2,by=c("Event_ID","Unit_Code","Plot_Name","cycle","Latin_Name"), all.x=T, all.y=T)

# Add plot-level species occurence
midnspp<-makeSppList('exotic',from=2007, to=2018)
exotpp<-data.frame(table(midnspp$Latin_Name))
midnspp2<-midnspp %>% filter(Latin_Name %in% invlist$Latin_Name) %>% droplevels()
midnspp3<-merge(park.plots,midnspp2[,c('Event_ID','Latin_Name','Tree','Shrub','Herbaceous','Graminoid','Exotic',
                                       'avg.quad.cover','avg.quad.freq')], by='Event_ID',all.x=T)
midnspp3$Latin_Name[is.na(midnspp3$Latin_Name)]<-'noinvspp'
table(complete.cases(midnspp3$Latin_Name))
midnspp4<-rmRejected(midnspp3)
midnspp5<- midnspp4 %>% mutate(cycle2=case_when(Unit_Code=='COLO' & cycle=="Cycle2" ~ 3,
                          Unit_Code=='COLO' & cycle=="Cycle1" ~ 2,
                          Unit_Code!='COLO' & cycle=="Cycle1" ~ 1,
                          Unit_Code!='COLO' & cycle=="Cycle2" ~ 2,
                          Unit_Code!='COLO' & cycle=="Cycle3" ~ 3),
         cycle=cycle2,
         present=1) %>% select(Event_ID,Unit_Code,Plot_Name,cycle,Latin_Name,present)  

midnspp6<-midnspp5 %>% spread(Latin_Name, present, fill=0)
names(midnspp6)

midnspp7<-midnspp6 %>% mutate(Elaeagnus= Elaeagnus + `Elaeagnus angustifolia`+ `Elaeagnus umbellata`,
                              `Euonymus alatus`= `Euonymus alatus`+ Euonymus, 
                              `Lonicera - Exotic`= `Lonicera - Exotic`+ `Lonicera morrowii` + `Lonicera maackii`) %>% 
  select(-`Elaeagnus angustifolia`, `Elaeagnus umbellata`, -Euonymus,
         - `Lonicera morrowii`, -`Lonicera maackii`)

midnspp8<-midnspp7 %>% gather('Latin_Name','plotfreq',-(Event_ID:cycle))
midnspp8<-midnspp8 %>% mutate(plotfreq=ifelse(plotfreq>1,1,plotfreq))

sppcomb<-merge(midnspp8, quads2, by=c('Event_ID','Unit_Code','Plot_Name','cycle','Latin_Name'), all.x=T,all.y=T)

sppcomb$avgcov[is.na(sppcomb$avgcov)]<-0
sppcomb$avgfreq[is.na(sppcomb$avgfreq)]<-0
sppcomb$plotfreq[is.na(sppcomb$plotfreq)] <- 0

sppcomb<-sppcomb %>% filter(Latin_Name !='noinvspp')
spp_comb2<-sppcomb %>% mutate(plot.freq=plotfreq, avg.cover=avgcov, quad.freq=avgfreq, qpct.freq=avgfreq*100) %>% 
  select(-Event_ID,-plotfreq,-avgcov, -avgfreq) %>% arrange(Plot_Name,cycle)

spp_comb3<- merge(spp_comb2, invlist, by="Latin_Name",all.x=T)
spp_comb_final<- spp_comb3 %>% mutate(species=ifelse(Accepted=='Y', paste0(Latin_Name), paste0(Accepted.Name))) %>% 
  select(Unit_Code, Plot_Name, cycle, species, plot.freq, avg.cover, quad.freq, qpct.freq) # replaced old with new names

sort(unique(spp_comb_final$species))

write.csv(spp_comb_final, './data/MIDN/MIDN_invasive_species_data.csv', row.names=FALSE)

sppcomb2<-sppcomb %>% group_by(Unit_Code,cycle,Latin_Name) %>% 
  summarise(avgcov=mean(avgcov), avgfreq=mean(avgfreq), plotfreq=sum(plotfreq), numplots=n())
head(sppcomb2)

write.csv(sppcomb2,'./data/MIDN/MIDN_park-level_invasive_species_summary.csv', row.names=FALSE)

topspp<-sppcomb2 %>% filter(cycle==3) %>% group_by(Latin_Name) %>% summarise(plotfreq=sum(plotfreq)) %>% arrange(-plotfreq)
head(topspp)

#---------------------------
# Summarize data by guild
#---------------------------
# Need to set guilds to only count a species once

plants<-plants %>% mutate(Tree=ifelse(Tree==TRUE & Shrub==TRUE, FALSE, Tree)) %>% filter(Latin_Name %in% invlist_trends$Latin_Name)

# Plot Frequency and Plot Richness
prespplist1<-makeSppList('all',from=2007, to=2018)
prespplist2<-prespplist1 %>% filter(Latin_Name %in% invlist_trends$Latin_Name) %>% droplevels()
prespplist3<-merge(park.plots,prespplist2[,c('Event_ID','Latin_Name','Tree','Shrub','Herbaceous','Graminoid','Exotic',
                                             'avg.quad.cover','avg.quad.freq')], by='Event_ID',all.x=T)
prespplist3$Latin_Name[is.na(prespplist3$Latin_Name)]<-'noinvspp'
prespplist4 <- rmRejected(prespplist3)

prespplist5 <- prespplist4 %>% mutate(cycle2=case_when(Unit_Code=='COLO' & cycle=="Cycle2" ~ 3,
                                                       Unit_Code=='COLO' & cycle=="Cycle1" ~ 2,
                                                       Unit_Code!='COLO' & cycle=="Cycle1" ~ 1,
                                                       Unit_Code!='COLO' & cycle=="Cycle2" ~ 2,
                                                       Unit_Code!='COLO' & cycle=="Cycle3" ~ 3),
                                      cycle=cycle2, present=1) %>% 
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

quad2<-quad1 %>% filter(Latin_Name %in% invlist_trends$Latin_Name) %>% droplevels() %>% 
  select(Event_ID, A2:CC,Latin_Name:Shrub,Herbaceous,Graminoid,Exotic,avg.cover,avg.freq )

quad3<-merge(park.plots, quad2, by='Event_ID',all.x=T)

quad4<-quad3 %>% mutate(Tree=ifelse(Tree+Shrub>1,0,Tree),
                        Shrub=ifelse(Tree+Shrub>1,1,Shrub))

quad.r<-quad3 %>% group_by(Event_ID,Tree,Shrub,Herbaceous,Graminoid) %>% 
  summarise(numQuadrats=first(numQuadrats), 
            A2=sum(A2), A5=sum(A5), A8=sum(A8), AA=sum(AA),
            B2=sum(B2), B5=sum(B5), B8=sum(B8), BB=sum(BB),
            C2=sum(C2), C5=sum(C5), C8=sum(C8), CC=sum(CC))

quad.r<-quad.r %>% mutate(avg.quad.r=(A2+A5+A8+AA+B2+B5+B8+BB+C2+C5+C8+CC)/numQuadrats) %>% select(-(numQuadrats:CC))

quad.r2<-quad.r %>% gather(guild,present,Tree:Graminoid) %>% filter(present==1) %>% select(-present)

quad.r2$avg.quad.r[is.na(quad.r2$avg.quad.r)]<-0
#quad.r2 has average quadrat exotic richness
head(quad.r2)

#plants3 %>% filter(indinv==1) %>% select(Latin_Name) %>% arrange(Latin_Name)

quad.r<-quad2 %>% group_by(Event_ID,Tree,Shrub,Herbaceous,Graminoid) %>% 
  summarise(numQuadrats=first(numQuadrats),A2=sum(A2),A5=sum(A5),A8=sum(A8),AA=sum(AA),
    B2=sum(B2),B5=sum(B5),B8=sum(B8),BB=sum(BB),C2=sum(C2),C5=sum(C5),C8=sum(C8),CC=sum(CC))

quad.r<-quad.r %>% mutate(avg.quad.r=(A2+A5+A8+AA+B2+B5+B8+BB+C2+C5+C8+CC)/numQuadrats) %>% select(-(numQuadrats:CC))

quad.r2<-quad.r %>% gather(guild,present,Tree:Graminoid) %>% filter(present==1) %>% select(-present)
  #quad.r2 has average quadrat exotic richness

quad.r2$avg.quad.r[is.na(quad.r2$avg.quad.r)]<-0
 #quad.r2 has average quadrat exotic richness
head(quad.r2)

#--------------------------
# Summarise quadrat cover and frequency by guild
quad<-joinQuadData(from=2007, to=2018, QAQC=F,locType='VS',output='short',speciesType='all') 
quad1<-rmRejected(quad)

quad2<-quad1 %>% filter(Latin_Name %in% invlist_trends$Latin_Name) %>% droplevels() %>% 
  select(Event_ID,Tree,Shrub,Herbaceous,Graminoid, avg.cover, A2:CC)

quad3<-merge(park.plots,quad2, by='Event_ID',all.x=T)

quad4<-quad3 %>% group_by(Event_ID, Tree, Shrub, Herbaceous, Graminoid) %>% 
  summarise(avg.cover=sum(avg.cover),
            A2=ifelse(sum(A2)>0,1,0),A5=ifelse(sum(A5)>0,1,0),A8=ifelse(sum(A8)>0,1,0),AA=ifelse(sum(AA)>0,1,0),
            B2=ifelse(sum(B2)>0,1,0),B5=ifelse(sum(B5)>0,1,0),B8=ifelse(sum(B8)>0,1,0),BB=ifelse(sum(BB)>0,1,0),
            C2=ifelse(sum(C2)>0,1,0),C5=ifelse(sum(C5)>0,1,0),C8=ifelse(sum(C8)>0,1,0),CC=ifelse(sum(CC)>0,1,0),
            avg.freq=(A2+A5+A8+AA+B2+B5+B8+BB+C2+C5+C8+CC)/first(numQuadrats)) %>% 
  mutate(guild= case_when(Tree==1 ~'Tree',
                          Shrub==1~'Shrub',
                          Herbaceous==1~'Herbaceous',
                          Graminoid==1~'Graminoid')) %>% ungroup() %>% select(Event_ID,guild,avg.cover,avg.freq)

quad4$guild<-as.factor(quad4$guild)

park.plots2<-park.plots %>% mutate(Graminoid=1,Herbaceous=1,Shrub=1,Tree=1) %>%
  gather(key=guild,value=pres,Graminoid:Tree) %>% select(-pres)
# makes a matrix with every plot visit and every combination of guild

quads.comb1<-merge(park.plots2,quad4,by=c("Event_ID","guild"),all.x=T)
quads.comb1[,14:15][is.na(quads.comb1[,14:15])]<-0

guild<-quads.comb1 %>% select(Location_ID,Event_ID,Unit_Code:cycle,guild,avg.cover:avg.freq)

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

comb5<-comb5 %>% mutate(cycle=as.numeric(str_sub(cycle,6)))

comb5<-comb5 %>% mutate(quadFreq=avg.freq*numQuadrats) %>% select(Event_ID,Location_ID,Unit_Code:cycle,guild,
  plotFreq, avg.quad.r, avg.cover, quadFreq, avg.freq, numPlotSpp)

colnames(comb5)<-c("Event_ID","Location_ID","Unit_Code","Plot_Name","Plot_Number","X_Coord","Y_Coord",    
  "Panel","Year","Event_QAQC","cycle","guild","plot.freq","avg.quad.r","avg.cover","quad.freq","qpct.freq","numPlotSpp")

comb5<-comb5 %>% arrange(Plot_Name,cycle)
write.csv(comb5,'./data/MIDN/MIDN_invasive_guild_data.csv',row.names=F)

#-------------------------------------
# Summarize total invasive data
#-------------------------------------
# Plot Frequency and Plot Richness of all invasives
prespplist1<-makeSppList('all',from=2007, to=2018)
prespplist2<-prespplist1 %>% filter(Latin_Name %in% invlist_trends$Latin_Name) %>% droplevels() %>% 
  select(Event_ID, Latin_Name, tree.stems:addspp.present)

prespplist3<-merge(park.plots,prespplist2, by='Event_ID',all.x=T)
prespplist3$Latin_Name[is.na(prespplist3$Latin_Name)]<-'noinvspp'
prespplist4 <- rmRejected(prespplist3)
names(prespplist4)

prespplist5 <- prespplist4 %>% mutate(cycle2=case_when(Unit_Code=='COLO' & cycle=="Cycle2" ~ 3,
                                                       Unit_Code=='COLO' & cycle=="Cycle1" ~ 2,
                                                       Unit_Code!='COLO' & cycle=="Cycle1" ~ 1,
                                                       Unit_Code!='COLO' & cycle=="Cycle2" ~ 2,
                                                       Unit_Code!='COLO' & cycle=="Cycle3" ~ 3),
                                      cycle=cycle2, present=1) %>% 
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

quad2<-quad1 %>% filter(Latin_Name %in% invlist_trends$Latin_Name) %>% droplevels() %>% 
  select(Event_ID, numQuadrats, A2:CC,Latin_Name,avg.cover,avg.freq )

quad3<-merge(park.plots, quad2, by='Event_ID',all.x=T)

quad.r<-quad2 %>% group_by(Event_ID) %>%
  summarise(numQuadrats=first(numQuadrats),A2=sum(A2),A5=sum(A5),A8=sum(A8),AA=sum(AA),
    B2=sum(B2),B5=sum(B5),B8=sum(B8),BB=sum(BB),C2=sum(C2),C5=sum(C5),C8=sum(C8),CC=sum(CC),
    avg.cover=sum(avg.cover))

quad.r2<-quad.r %>% mutate(avg.quad.r=(A2+A5+A8+AA+B2+B5+B8+BB+C2+C5+C8+CC)/numQuadrats, 
  quadFreq=ifelse(A2>0,1,0)+ifelse(A5>0,1,0)+ifelse(A8>0,1,0)+ifelse(AA>0,1,0)+
    ifelse(B2>0,1,0)+ifelse(B5>0,1,0)+ifelse(B8>0,1,0)+ifelse(BB>0,1,0)+
    ifelse(C2>0,1,0)+ifelse(C5>0,1,0)+ifelse(C8>0,1,0)+ifelse(CC>0,1,0),
  qPctFreq=quadFreq/numQuadrats) %>% 
  select(-(numQuadrats:CC))

head(quad.r2)

quad.r2[,c(2:5)][(is.na(quad.r2[,c(2:5)]))] <-0
head(quad.r2) #quad.r2 has average quadrat exotic richness

#--------------------------
# Combine metrics
#--------------------------
comb1<-merge(park.plots,plotFreq[,c('Event_ID','numPlotSpp','plotFreq')],by='Event_ID',all.x=T) 
comb2<-merge(comb1,quad.r2,by=c("Event_ID"),all.x=T)
head(comb2)

comb2[,13:18][is.na(comb2[,13:18])]<-0

comb2<-comb2 %>% mutate(cycle=as.numeric(str_sub(cycle,6)))

comb2<-comb2 %>% select(Event_ID:cycle,  plotFreq, avg.quad.r, avg.cover, quadFreq, qPctFreq, numPlotSpp)
colnames(comb2)<-c("Event_ID","Location_ID","Unit_Code","Plot_Name","Plot_Number","X_Coord","Y_Coord",    
  "Panel","Year","Event_QAQC","cycle","plot.freq","avg.quad.r","avg.cover","quad.freq","qpct.freq","numPlotSpp")
comb2<-comb2 %>% arrange(Plot_Name,cycle)

head(comb2)
write.csv(comb2,'./data/MIDN/MIDN_invasive_total_data.csv',row.names=F)

