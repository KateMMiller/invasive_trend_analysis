#-----------------------------------
# Code for invasive trend analysis
#-----------------------------------
library(forestMIDN)

#++++++++++++++++++++++++++++++++++
# NOTE: These summaries are only include exotic species that were on the indicator list
# since 2007 to ensure that comparisons across cycles are consistent. 
#++++++++++++++++++++++++++++++++++
spp<-c("Ligustrum", "Ligustrum vulgare","Oplismenus hirtellus ssp. undulatifolius")# Species that
# are exotic and were added to indicator list after 2007.

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
# First summarize data by guild
#---------------------------
# Need to set guilds to only count a species once
table(plants$Tree, plants$Shrub)
table(plants$Shrub, plants$Vine)
table(plants$Graminoid,plants$Herbaceous)

plants<-plants %>% mutate(Tree=ifelse(Tree+Shrub>1,0,Tree),
  Shrub=ifelse(Tree+Shrub>1,1,Shrub),Shrub=ifelse(Vine==1,1,Shrub))
table(plants$Exotic)

plants<-plants %>% mutate(Indicator_MIDN2=ifelse((Indicator_MIDN==1 & !Latin_Name %in% spp)|(Exotic==1 & Tree==1),1,0))
table(plants$Indicator_MIDN2,plants$Indicator_MIDN)

# Plot Frequency and Plot Richness
spplist1<-makeSppList(speciesType='exotic')
spplist<-rmRejected(spplist1)

spplist<-spplist %>% mutate(Tree=ifelse(Tree+Shrub>1,0,Tree),
  Shrub=ifelse(Tree+Shrub>1,1,Shrub))

spplist[,c(13:22,25:29)][is.na(spplist[,c(13:22,25:29)])]<-0
#spplist$pres.temp<-ifelse(rowSums(spplist[,c(13:22)])>0,1,0)# Changed this for MIDN to only include quad data
spplist$pres.temp<-ifelse(spplist$avg.quad.cover>0,1,0)
spplist2<-spplist %>% left_join(.,plants[,c("TSN","Indicator_MIDN2")], by='TSN') %>% 
  filter(Indicator_MIDN2==1) %>% droplevels()

head(spplist2)

spplist3<-spplist2 %>% group_by(Event_ID,Tree,Shrub,Herbaceous,Graminoid) %>% 
  summarise(numPlotSpp=sum(pres.temp),plotFreq=ifelse(sum(pres.temp)>0,1,0))

spplist4<-merge(park.plots,spplist3,by="Event_ID",all.x=T)

plotFreq<-spplist4 %>% select(Event_ID,Tree:Graminoid,plotFreq) %>% gather(guild,present,Tree:Graminoid) %>%  
  filter(present==1) %>% select(-present) %>% group_by(Event_ID,guild) %>% summarise(plotFreq=ifelse(sum(plotFreq)>0,1,0)) 

numPlotSpp<-spplist4 %>% select(Event_ID,Tree:Graminoid,numPlotSpp) %>% 
  gather(guild,present,Tree:Graminoid) %>% filter(present==1) %>% select(-present) %>% 
  group_by(Event_ID,guild) %>% summarise(numPlotSpp=sum(numPlotSpp))

#---------------------------
# Quadrat richness by guild
quad<-joinQuadData(from=2007, to=2018, QAQC=F,locType='VS',output='short',speciesType='exotic') 
quad1<-rmRejected(quad)

quad1<-quad1 %>% mutate(Tree=ifelse(Tree+Shrub>1,0,Tree),
  Shrub=ifelse(Tree+Shrub>1,1,Shrub),Shrub=ifelse(Vine==1,1,Shrub)) %>% select(-Vine)

quad2<-quad1 %>% left_join(.,plants[,c("TSN","Indicator_MIDN2")], by='TSN') %>% 
  filter(Indicator_MIDN2==1) %>% droplevels()

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

quad3<-quad2 %>% group_by(Event_ID,Tree,Shrub,Herbaceous,Graminoid) %>% summarise(avg.cover=sum(avg.cover),
  A2=ifelse(sum(A2)>0,1,0),A5=ifelse(sum(A5)>0,1,0),A8=ifelse(sum(A8)>0,1,0),AA=ifelse(sum(AA)>0,1,0),
  B2=ifelse(sum(B2)>0,1,0),B5=ifelse(sum(B5)>0,1,0),B8=ifelse(sum(B8)>0,1,0),BB=ifelse(sum(BB)>0,1,0),
  C2=ifelse(sum(C2)>0,1,0),C5=ifelse(sum(C5)>0,1,0),C8=ifelse(sum(C8)>0,1,0),CC=ifelse(sum(CC)>0,1,0),
  avg.freq=(A2+A5+A8+AA+B2+B5+B8+BB+C2+C5+C8+CC)/first(numQuadrats)) %>%
  mutate(guild= case_when(Tree == 1 ~ 'Tree',
    Shrub == 1 ~ 'Shrub',
    Herbaceous == 1 ~ 'Herbaceous',
    Graminoid == 1 ~ 'Graminoid')) %>% ungroup() %>% select(Event_ID,guild,avg.cover,avg.freq)

quad3$guild<-as.factor(quad3$guild)

park.plots2<-park.plots %>% mutate(Graminoid=1,Herbaceous=1,Shrub=1,Tree=1) %>%
  gather(key=guild,value=pres,Graminoid:Tree) %>% select(-pres)
# makes a matrix with every plot visit and every combination of guild

quads.comb1<-merge(park.plots2,quad3,by=c("Event_ID","guild"),all.x=T)
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
spplist1<-makeSppList(speciesType='exotic')
spplist<-rmRejected(spplist1)

names(spplist)
spplist[,c(13:22,25:29)][is.na(spplist[,c(13:22,25:29)])]<-0
#spplist$pres.temp<-ifelse(rowSums(spplist[,c(13:22)])>0,1,0)# Changed this for MIDN to only include quad data
spplist$pres.temp<-ifelse(spplist$avg.quad.cover>0,1,0)

spplist2<-spplist %>% left_join(.,plants[,c("TSN","Indicator_MIDN2")], by='TSN') %>% 
  filter(Indicator_MIDN2==1) %>% droplevels()

plotFreq<-spplist2 %>% group_by(Event_ID) %>% 
  summarise(numPlotSpp=sum(pres.temp),plotFreq=ifelse(sum(pres.temp)>0,1,0))

head(plotFreq)
#---------------------------
# Quadrat richness and average cover of all invasives
quad<-joinQuadData(from=2007, to=2018, QAQC=F,locType='VS',output='short',speciesType='exotic') 
quad1<-rmRejected(quad) %>% arrange(Plot_Name)
quad1<-quad1 %>% mutate(Shrub=ifelse(Shrub+Vine>0,1,0)) %>% select(-Vine)

quad2<-quad1 %>% left_join(.,plants[,c("TSN","Indicator_MIDN2")], by='TSN') %>% 
  filter(Indicator_MIDN2==1) %>% droplevels()

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
comb1<-merge(park.plots,plotFreq,by='Event_ID',all.x=T) 
comb2<-merge(comb1,quad.r2,by=c("Event_ID"),all.x=T)
head(comb2)

comb2[,13:18][is.na(comb2[,13:18])]<-0

comb2<-comb2 %>% mutate(cycle=as.numeric(str_sub(cycle,6)))

comb2<-comb2 %>% select(Event_ID:cycle,  plotFreq, avg.quad.r, avg.cover, quadFreq, qPctFreq, numPlotSpp)
colnames(comb2)<-c("Event_ID","Location_ID","Unit_Code","Plot_Name","Plot_Number","X_Coord","Y_Coord",    
  "Panel","Year","Event_QAQC","cycle","plot.freq","avg.quad.r","avg.cover","quad.freq","qpct.freq","numPlotSpp")
comb2<-comb2 %>% arrange(Plot_Name,cycle)

summary(comb2)
write.csv(comb2,'./data/MIDN/MIDN_invasive_total_data.csv',row.names=F)
