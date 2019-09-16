library(tidyverse)
parkstatus<-read.csv('./results/Table_1_park-level_summaries.csv')

sppstatus<-read.csv('./results/Table_2_species-level_summaries.csv')[1:10,c(1,2,4,5)]

allparkspp<-read.csv('./results/park-level_species_summaries.csv')

# trends functions for park-level results
# Compile trend data for param park
# Total invasives
plot_freq_tot<-read.csv('./results/results_PFreq-total-coefs.csv')
quad_freq_tot<-read.csv('./results/results_qfreq-total-coefs_NP.csv')
avg_cov_tot<-read.csv('./results/results_avecov-total-coefs_NP.csv')

# Guild level
plot_freq_guild<-read.csv('./results/results_PFreq-by_guild-coefs.csv')
quad_freq_guild<-read.csv('./results/results_qfreq-by_guild-coefs_NP.csv')
avg_cov_guild<-read.csv('./results/results_avecov-by_guild-coefs_NP.csv')

# Species level
invlist<-read.csv('./results/Invasive_List.csv')[,c('Latin_Name','Common')]
plot_freq_spp<-read.csv('./results/results_PFreq-by_species-coefs.csv')
quad_freq_spp<-read.csv('./results/results_qfreq-by_species-coefs_NP.csv')
avg_cov_spp<-read.csv('./results/results_avecov-by_species-coefs_NP.csv')

# Status tables
parkstatus<-read.csv('./results/Table_1_park-level_summaries.csv')
sppstatus<-read.csv('./results/Table_2_species-level_summaries.csv')[1:10,c(1,2,4,5)]
allparkspp<-read.csv('./results/park-level_species_summaries.csv')

totinv<-read.csv('./results/park-level_summaries.csv')
guildinv<-read.csv('./results/park-level_summaries_guild.csv')
allparkspp<-read.csv('./results/park-level_species_summaries.csv')

# trends functions for park-level results
# Compile trend data for param park
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
                                    ' decreased significantly by '),                                                                round(estimate,1),'% per cycle.'),
                      paste0('An invasive species ','was ', round(estimate,1), ' times ',  
                             ifelse(estimate>1,'more ',
                                    'less '), 
                             'likely to occur in subsequent cycles.')))

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
                                    ' decreased significantly by '),                                                                round(estimate,1),'% per cycle.'),
                      paste0('was ', round(estimate,1), ' times ',  
                             ifelse(estimate>1,'more ',
                                    'less '), 
                             'likely to occur in subsequent cycles.')))
results_guild_paste<-function(x){paste(" ", x[1], " ", x[4], sep="")}
guild_results<-apply(results_guild,1,results_guild_paste)}

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
                                    ' decreased significantly by '),                                                                round(estimate,1),'% per cycle.'),
                      paste0('was ', round(estimate,1), ' times ',  
                             ifelse(estimate>1,'more ',
                                    'less '), 
                             'likely to occur in subsequent cycles.')))

results_spp_paste<-function(x){paste(" ", x[1], " (", "<i>", x[2], "</i>", ") ", x[5], sep="")}
spp_results<-apply(results_spp,1,results_spp_paste)
}