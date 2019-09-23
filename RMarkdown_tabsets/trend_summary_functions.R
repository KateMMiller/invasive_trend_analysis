#---------------------------------------
# Functions and datasets called throughout markdown tabset briefs
#---------------------------------------
library(tidyverse)
library(gridExtra)

# Total invasives trends
plot_freq_tot<-read.csv('./results/results_PFreq-total-coefs.csv')
quad_freq_tot<-read.csv('./results/results_qfreq-total-coefs_NP.csv')
avg_cov_tot<-read.csv('./results/results_avecov-total-coefs_NP.csv')

# Guild level trends
plot_freq_guild<-read.csv('./results/results_PFreq-by_guild-coefs.csv')
quad_freq_guild<-read.csv('./results/results_qfreq-by_guild-coefs_NP.csv')
avg_cov_guild<-read.csv('./results/results_avecov-by_guild-coefs_NP.csv')

# Species level trends
invlist<-read.csv('./results/Invasive_List.csv')[,c('Latin_Name','Common')]
plot_freq_spp<-read.csv('./results/results_PFreq-by_species-coefs.csv')
quad_freq_spp<-read.csv('./results/results_qfreq-by_species-coefs_NP.csv')
avg_cov_spp<-read.csv('./results/results_avecov-by_species-coefs_NP.csv')

# Status tables
parkstatus<-read.csv('./results/Table_1_park-level_summaries.csv')
parkstatus<-parkstatus %>% arrange(rank)

sppstatus<-read.csv('./results/Table_2_species-level_summaries.csv')[1:10,c(1,2,4,5)]
allparkspp<-read.csv('./results/park-level_species_summaries.csv')

totinv<-read.csv('./results/park-level_summaries.csv')
guildinv<-read.csv('./results/park-level_summaries_guild.csv')
allparkspp<-read.csv('./results/park-level_species_summaries.csv')

# Modeled responses
AC_guild_resp<-read.csv('./results/results_avecov-by_guild-response_NP.csv')
AC_guild_resp<-AC_guild_resp %>% mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))
AC_guild_resp$guild<-fct_relevel(AC_guild_resp$guild,
                                 c('Shrub','Herbaceous','Graminoid', 'Tree'))

QF_guild_resp<-read.csv('./results/results_qfreq-by_guild-response_NP.csv')
QF_guild_resp<-QF_guild_resp %>% mutate(cycle2=as.factor(cycle2),sign=as.factor(sign))
QF_guild_resp$guild<-fct_relevel(QF_guild_resp$guild,
                                 c('Shrub','Herbaceous','Graminoid', 'Tree'))

   #Plot frequency takes more work, b/c have to add non-modeled parks back in
pfreq_guild_coefs<-read.csv('./results/results_pfreq-by_guild-coefs.csv')
pfreq_guild_slopes<-pfreq_guild_coefs %>% filter(coef=='Slope') %>% droplevels()

df<-read.csv("./data/NETN-MIDN-ERMN-NCRN_guild_invasives.csv")#[,-c(1,2)]
df_pf<- df %>% arrange(park,plot_name,cycle,guild) %>% select(park,plot_name,cycle,guild,lat.rank,plot.freq)
df_pf2<-df_pf %>% filter(park!='SAHI' & park!='WOTR') %>% group_by(park,guild,cycle) %>% 
  summarise(plot.freq=sum(plot.freq), num.plots=n(),pfreq=round(((plot.freq/num.plots)*100),2), lat.rank=first(lat.rank))

pfreq_guild_comb<-merge(df_pf2,pfreq_guild_slopes, by=c('park','guild'),all.x=T)
pfreq_guild_comb$sign[is.na(pfreq_guild_comb$sign)]<-0

pfreq_guild_comb<- pfreq_guild_comb %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle),sign=as.factor(sign), park=reorder(park, -lat.rank))

pfreq_guild_comb$guild<-fct_relevel(pfreq_guild_comb$guild,c('Shrub','Herbaceous','Graminoid', 'Tree'))

# Functions for park-level results
check_too_small<-function(metric){
  num<-ifelse(metric < 0.1, paste0('<0.1'), paste0(format(round(metric,1),nsmall=1)))
  return(num)
}

# Total invasives
filter_tot_trends<-function(df, metric, park){
  if(metric=='plot frequency'){
    df<-df %>% filter(park==park_code, coef=='Slope', sign==1) %>% droplevels() %>% 
      select(estimate) %>% mutate(metric=paste('plot frequency'), estimate=exp(estimate))
  } else {
    
    df<-df %>% filter(park==park_code, coef=='Slope', sign==1) %>% droplevels() %>% 
      select(estimate) %>%  mutate(metric=paste(metric))
  }
  return(df)    
}

# Guild level
filter_guild_trends<-function(df, metric, park){
  if(metric=='plot frequency'){
    df<-df %>% filter(park==park_code, coef=='Slope', sign==1) %>% droplevels() %>% 
      select(guild, estimate) %>% mutate(metric=paste('plot frequency'),   
                                         estimate=exp(estimate))
  } else {
    
    df<-df %>% filter(park==park_code, coef=='Slope', sign==1) %>% droplevels() %>% 
      select(guild, estimate) %>% mutate(metric=paste(metric))
  }
  return(df)    
}

# Species level
filter_spp_trends<-function(df, metric, park){
  if(metric=='plot frequency'){
    df<-df %>% filter(park==park_code, coef=='Slope', sign==1) %>% droplevels() %>% 
      select(species, estimate) %>% left_join(., invlist, by=c('species'='Latin_Name')) %>% 
      select(Common, species, estimate) %>% mutate(metric=paste('plot frequency'),   
                                                   estimate=exp(estimate))
  } else {
    
    df<-df %>% filter(park==park_code, coef=='Slope', sign==1) %>% droplevels() %>% 
      select(species, estimate) %>% left_join(., invlist, by=c('species'='Latin_Name')) %>% 
      select(Common, species, estimate) %>% mutate(metric=paste(metric))
  }
  return(df)    
}

totinv_to_sentences<-function(park_code){
  parkinv<-totinv %>% filter(park==park_code) %>% droplevels()

  park_PF_tot <- filter_tot_trends(plot_freq_tot, 'plot frequency')
  park_QF_tot <- filter_tot_trends(quad_freq_tot, 'quadrat frequency')
  park_AC_tot <- filter_tot_trends(avg_cov_tot, 'average cover')

  results_tot<-rbind(park_PF_tot,park_QF_tot,park_AC_tot)
  results_tot<-results_tot %>% 
    mutate(prose=ifelse(metric!='plot frequency',
                      paste0(str_to_sentence(metric), ' of total invasives',
                             ifelse(estimate>0,' increased significantly by ',
                                    ' decreased significantly by '),                                                                
                             check_too_small(round(estimate,1)),'% per cycle.'),
                      paste0('An invasive species ','was ', 
                             check_too_small(round(estimate,1)), ' times ',  
                             ifelse(estimate>1,'more ',
                                    'less '), 
                             'likely to occur in a plot in subsequent cycles.')))

  results_tot_paste<-function(x){paste(" ", x[3], sep="")}
  tot_results<-apply(results_tot,1,results_tot_paste)
}

guild_to_sentences<-function(park_code){
  parkguild<-guildinv %>% filter(park==park_code) %>% droplevels() %>% 
    arrange(-avg.cover,-quad.pfreq,-plot.pfreq) %>% slice(1) 
  
  park_PF_guild <- filter_guild_trends(plot_freq_guild, 'plot frequency')
  park_QF_guild <- filter_guild_trends(quad_freq_guild, 'quadrat frequency')
  park_AC_guild <- filter_guild_trends(avg_cov_guild, 'average cover')

  results_guild<-rbind(park_PF_guild,park_QF_guild,park_AC_guild)
  results_guild<-results_guild %>% 
    mutate(guild=ifelse(metric!='plot frequency', 
                      paste0(str_to_sentence(guild)),
                      paste0('The invasive ', tolower(guild), ' guild')),
         prose=ifelse(metric!='plot frequency',
                      paste0(metric,
                             ifelse(estimate>0,' increased significantly by ',
                                    ' decreased significantly by '),                    
                             check_too_small(round(estimate,1)),'% per cycle.'),
                      paste0('was ', check_too_small(round(estimate,1)), ' times ',  
                             ifelse(estimate>1,'more ',
                                    'less '), 
                             'likely to occur in a plot in subsequent cycles.')))
  results_guild_paste<-function(x){paste(" ", x[1], " ", x[4], sep="")}
  guild_results<-apply(results_guild,1,results_guild_paste)
}

spp_to_sentences<-function(park_code){
  parkspp<-allparkspp %>% filter(park==park_code) %>% 
    arrange(-avg.cover, -qpct.freq, -plot.pfreq) %>% 
    select(sppgroup, Common, plot.pfreq, qpct.freq, avg.cover) %>% slice(1:5)
  
  park_PF_spp <- filter_spp_trends(plot_freq_spp, 'plot frequency')
  park_QF_spp <- filter_spp_trends(quad_freq_spp, 'quadrat frequency')
  park_AC_spp <- filter_spp_trends(avg_cov_spp, 'average cover')

  results_spp<-rbind(park_PF_spp,park_QF_spp,park_AC_spp)
  results_spp<-results_spp %>% 
    mutate(Common=str_to_sentence(Common),
         prose=ifelse(metric!='plot frequency',
                      paste0(metric,
                             ifelse(estimate>0,' increased significantly by ',
                                    ' decreased significantly by '),                                                                
                             check_too_small(round(estimate,1)),'% per cycle.'),
                      paste0('was ', check_too_small(round(estimate,1)), ' times ',  
                             ifelse(estimate>1,'more ',
                                    'less '), 
                             'likely to occur in a plot in subsequent cycles.')))

results_spp_paste<-function(x){paste(" ", x[1], " (", "<i>", x[2], "</i>", ") ", x[5], sep="")}
spp_results<-apply(results_spp,1,results_spp_paste)
}

quaddf_park_by_guild<-function(df, title, park_code, ylabels){
  parkdf<-df %>% filter(park==park_code) %>% droplevels()
  ggplot(parkdf, aes(x=cycle2, y=mean, group=guild))+ 
          geom_line(aes(y=mean, x=cycle2, colour=factor(guild), linetype=sign), na.rm=TRUE)+
          scale_shape_manual(values=c(21,19))+
          geom_errorbar(aes(ymin=lower, ymax=upper, x=cycle2,
                      colour=factor(guild)), width=0.1,size=1, na.rm=TRUE)+
          geom_point(aes(y=mean, x=cycle2, colour=factor(guild), shape=sign), 
                     size=1.8, stroke=1.5, fill='white', na.rm=TRUE)+
          #      scale_fill_manual(values=c('white'))+
          scale_linetype_manual(values=c('dashed','solid'))+
          scale_color_manual(values=c('IndianRed','ForestGreen','Gold','RoyalBlue'))+
          theme_bw()+
          theme(axis.text=element_text(size=11),axis.title=element_text(size=12),
                plot.margin=unit(c(1,1,1,1),'lines'),
                plot.title=element_text(hjust=0.5, size=12, margin=margin(t=10,b=-15)),
                panel.background=element_blank(),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                axis.line = element_blank(), 
                legend.position='none') + 
          labs(x='Cycle', y=title)+
          scale_x_discrete(breaks=c(1,2,3))+
          scale_y_continuous(breaks=ylabels, labels=ylabels, limits=c(-10,110))
} # plots results for each park


plotfreq_park_by_guild<-function(df, title, park_code, ylabels){
  parkdf<-df %>% filter(park==park_code) %>% droplevels()
  ggplot(parkdf, aes(x=cycle2, y=pfreq, group=guild))+ 
          geom_line(aes(y=pfreq, x=cycle2, colour=factor(guild), linetype=sign), na.rm=TRUE)+
          scale_shape_manual(values=c(21,19))+
          geom_point(aes(y=pfreq, x=cycle2, colour=factor(guild), shape=sign), 
                     size=1.8, stroke=1.5, fill='white', na.rm=TRUE)+
          #      scale_fill_manual(values=c('white'))+
          scale_linetype_manual(values=c('dashed','solid'))+
          scale_color_manual(values=c('IndianRed','ForestGreen','Gold','RoyalBlue'))+
          theme_bw()+
          theme(axis.text=element_text(size=11),axis.title=element_text(size=12),
                plot.margin=unit(c(1,1,1,1),'lines'),
                plot.title=element_text(hjust=0.5, size=12, margin=margin(t=10,b=-15)),
                panel.background=element_blank(),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                axis.line = element_blank(), 
                legend.position='none') + 
          labs(x='Cycle', y=title)+
          scale_x_discrete(breaks=c(1,2,3))+
          scale_y_continuous(breaks=ylabels, labels=ylabels, limits=c(-10,110))
} # plots results for each park
