#-----------------------------------
# Code for Park-level Invasive Trend Analysis
#-----------------------------------
library(tidyverse) # attaches most of the important packages in the tidyverse
library(lme4) # for lmer and glmer
library(modelr) #for handling multiple models in tidyverse
library(broom.mixed)# for better model summary tables than default in nlme
library(directlabels) # for labeling lines in plots
library(colorRamps) # for park-level color ramp
options("scipen"=100, "digits"=4) # keeps TSN numbers as numbers 

# Results plotting functions
# Park-level plot for total invasive cover
plotCoverParkTotal<-function(df){ 
  print(ggplot(df, aes(x=cycle2, y=mean, group=park))+ 
          facet_wrap(~park,ncol=8,scales='free')+
          geom_errorbar(aes(ymin=lower, ymax=upper, x=cycle2), width=0.1,size=1, na.rm=TRUE)+
          #geom_smooth(method='lm',se=F, linetype=sign)+
          geom_line(aes(y=mean, x=cycle2, linetype=sign), na.rm=TRUE)+
          geom_point(aes(y=mean, x=cycle2, shape=sign), 
                     size=2, stroke=1.5, fill='white', na.rm=TRUE)+
          scale_shape_manual(values=c(21,19))+
          scale_fill_manual(values=c('white'))+
          scale_linetype_manual(values=c('dashed','solid'))+
          scale_color_manual(values=c('black'))+
          theme_bw()+
          theme(axis.text=element_text(size=11),axis.title=element_text(size=12),
                plot.margin=unit(c(0.4,0.4,0.5,0.3),'lines'),
                plot.title=element_text(hjust=0.5, size=12, margin=margin(t=10,b=-15)),
                panel.background=element_blank(),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                axis.line = element_blank(), 
                legend.position='none') + 
          labs(x="Cycle", y='Avg. Quadrat % Cover')+
          scale_x_discrete(breaks=c(1,2,3))+
          scale_y_continuous(limits=c(-10,80),breaks=c(0,20,40,60,80),labels=c(0,20,40,60,80)))
}

# Park-level plot for invasive cover by guild
plotCoverParkGuild<-function(df){ 
  print(ggplot(df, aes(x=cycle2, y=mean, group=guild))+ 
      facet_wrap(~park,ncol=8,scales='free')+
      geom_errorbar(aes(ymin=lower, ymax=upper, x=cycle2,
        colour=factor(guild)), width=0.1,size=1, na.rm=TRUE)+
      geom_line(aes(y=mean, x=cycle2,colour=factor(guild), linetype=sign), na.rm=TRUE)+
      geom_point(aes(y=mean, x=cycle2,colour=factor(guild), shape=sign), 
        size=1.5, stroke=1.5, fill='white', na.rm=TRUE)+
      scale_shape_manual(values=c(21,19))+
      scale_fill_manual(values=c('white'))+
      scale_linetype_manual(values=c('dashed','solid'))+
      scale_color_manual(values=c('Gold', 'ForestGreen','IndianRed','RoyalBlue', '#595959'))+
      theme_bw()+
      theme(axis.text=element_text(size=11),axis.title=element_text(size=12),
        plot.margin=unit(c(0.4,0.4,0.5,0.3),'lines'),
        plot.title=element_text(hjust=0.5, size=12, margin=margin(t=10,b=-15)),
        panel.background=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_blank(), 
        legend.position='none') + 
      labs(x='Cycle', y='Avg. Quadrat % Cover')+
      scale_x_discrete(breaks=c(1,2,3))+
      scale_y_continuous(limits=c(-10,60),breaks=c(0,20,40,60),labels=c(0,20,40,60)))
} # plots results for each park

# Overall plot for invasive cover- one guild plotted at a time
plotCoverGuild<-function(df){
  print(ggplot(df,aes(x=cycle,y=mean,group=park))+
      geom_point(aes(x=cycle,y=mean,colour=sign,shape=sign,size=sign),stroke=1,fill='white', na.rm=TRUE)+
      scale_size_manual(values=c(3,2))+
      scale_shape_manual(values=c(21,19))+theme_bw()+
      theme(panel.background=element_blank(),legend.position='none',
        plot.title=element_text(hjust=0.5, size=12, margin=margin(t=10,b=-15),face='bold'),
        panel.grid.major=element_blank(),panel.grid.minor = element_blank())+
      labs(x='Cycle', y='Avg. Quadrat % Cover')+scale_x_continuous(breaks=c(1,2,3)))
} #

# Species by park coefficient plots for invasive cover trends
coefPlot<- function(df, yrange=c(-10,30), ylabel){
  spplabs<-data.frame(table(df$species))
  spplabs<-spplabs %>% filter(Freq>0) %>% droplevels()
  spplabs$axistick<-lag(spplabs$Freq,k=1)
  spplabs[1,3]<-0
  spplabs$axisticks<-cumsum(spplabs$axistick)+1
  spplabs<-spplabs[,c('Var1','axisticks')]
  colnames(spplabs)<-c('species', 'axisticks')
  df2<-unique(merge(df, spplabs, by='species',all.y=T))
  df2<-df2 %>% arrange(species,park) %>% droplevels()
  df2$axis<-as.numeric(row.names(df2))
  df2$sign<-as.factor(df2$sign)
  print(ggplot(df2, aes(x=axis, y=estimate, fill=species, labels=df2$species)) +
          geom_hline(yintercept=0, lwd=1, color='DimGrey') +
          geom_errorbar(aes(ymin=lower, ymax=upper, colour=species), width=1.5, size=1, show.legend = F)+
          geom_point(stroke=1, size=2, colour='black', shape=21)+ 
          scale_fill_manual(values=c(colrkeep), guide=F)+
          scale_color_manual(values=colrkeep)+ theme_bw()+
          theme(axis.text.y=element_text(angle=45, size=11),
                axis.title.x=element_text(margin=margin(5,0,1,0)))+
          coord_flip() +  ylab(paste0('Slope: ',ylabel))+ ylim(yrange)+
          scale_x_reverse(name=NULL, breaks=unique(df2$axisticks), labels=levels(df2$species))+ 
          geom_text(data=df2,aes(label=park, y=ifelse(upper>0,upper+2,lower-0.3)), # avecov as upper+2 lower-0.3
                    nudge_x=-0.1, vjust=0.5, hjust=1, colour='black', cex=3.5 )
  )
}

# Species by park odds plots for invasive cover plot frequency
PFreqPlot<- function(df,  ylabel){
  spplabs<-data.frame(table(df$species))
  spplabs$axistick<-lag(spplabs$Freq,k=1)
  spplabs[1,3]<-0
  spplabs$axisticks<-cumsum(spplabs$axistick)+1
  spplabs<-spplabs[,c('Var1','axisticks')]
  colnames(spplabs)<-c('species', 'axisticks')
  df2<-merge(df, spplabs, by='species',all.y=T)
  df2<-df2 %>% arrange(species,park) %>% droplevels() %>% 
    mutate(axis=as.numeric(row.names(.)), species=as.factor(species))
  
  print(ggplot(df2, aes(x=axis, y=exp_est, fill=as.factor(species), group=species, labels=df2$species)) +
          geom_hline(yintercept=1, lwd=1, color='DimGrey') +
          geom_hline(yintercept=0, lwd=1, color='grey')+
          geom_errorbar(aes(ymin=exp_low, ymax=exp_high, colour=species), width=1.5, size=1, show.legend = F)+
          geom_point(stroke=1, size=2, colour='black', shape=21)+ 
          scale_fill_manual(values=c(colrkeep), guide=F)+
          scale_color_manual(values=colrkeep)+ theme_bw()+
          theme(axis.text.y=element_text(angle=45, size=11),
                axis.title.x=element_text(margin=margin(5,0,1,0)))+
          coord_flip() +  ylab(paste0('Odds of Plot Occurence ', ylabel))+ 
          scale_y_continuous(breaks=c(0,1,10,20,30,40), labels=c(0,1,10, 20, 30, 40))+
          scale_x_reverse(name=NULL, breaks=unique(df2$axisticks), labels=levels(df2$species))+ 
          geom_text(data=df2,aes(label=park, y=ifelse(exp_high>0,exp_high+2,exp_low-0.2)),
                    nudge_x=-0.1, vjust=0.5, hjust=1, colour='black', cex=3.5 )+
          geom_hline(yintercept=1, lty=2, color='black')
  )
}


# Park-level plot for invasive cover by species
plotCoverParkSpecies<-function(df, parkname, yrange=c(-10,40), yaxis=NULL){ 
  df2<-df %>% filter(park==parkname) %>% droplevels()
  print(ggplot(df2, aes(x=cycle2, y=mean, group=species, shape=species, fill=species))+ 
          #facet_wrap(~park,ncol=5,scales='free')+
          geom_errorbar(aes(ymin=lower, ymax=upper, x=cycle2,
                            colour=species), width=0.1, lwd=1)+
          geom_line(aes(y=mean, x=cycle2, colour=species), lwd=1)+
          geom_point(aes(y=mean, x=cycle2, fill=species, shape=species, size=species), 
                     stroke=1, size=2,colour='black')+
          scale_shape_manual(values = symb)+
          #scale_size_manual(values=c(3,3,3,3,2.5,3,3,3,2.5,3,3))+
          scale_fill_manual(values = colramp)+ 
          scale_color_manual(values = colramp)+
          theme_bw()+
          theme(axis.text=element_text(size=11),axis.title=element_text(size=12),
                plot.margin=unit(c(0.4,1,0.5,1),'lines'),
                plot.title=element_text(hjust=0.5, size=12, margin=margin(t=10,b=-15)),
                panel.background=element_blank(),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                axis.line = element_blank(), 
                legend.position='none') + 
          labs(x='Cycle', y=yaxis,title=parkname)+
          scale_x_discrete(breaks=c(1,2,3))+
          ylim(yrange))
} # plots results for each park

# Park-level plot for total invasive quad frequency
plotQFreqParkTotal<-function(df){ 
  print(ggplot(df, aes(x=cycle2, y=mean, group=park))+ 
          facet_wrap(~park,ncol=8,scales='free')+
          geom_errorbar(aes(ymin=lower, ymax=upper, x=cycle2), width=0.1,size=1, na.rm=TRUE)+
          #geom_smooth(method='lm',se=F, linetype=sign)+
          geom_line(aes(y=mean, x=cycle2, linetype=sign), na.rm=TRUE)+
          geom_point(aes(y=mean, x=cycle2, shape=sign), 
                     size=2, stroke=1.5, fill='white', na.rm=TRUE)+
          scale_shape_manual(values=c(21,19))+
          scale_fill_manual(values=c('white'))+
          scale_linetype_manual(values=c('dashed','solid'))+
          scale_color_manual(values=c('black'))+
          theme_bw()+
          theme(axis.text=element_text(size=11),axis.title=element_text(size=12),
                plot.margin=unit(c(0.4,0.4,0.5,0.3),'lines'),
                plot.title=element_text(hjust=0.5, size=12, margin=margin(t=10,b=-15)),
                panel.background=element_blank(),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                axis.line = element_blank(), 
                legend.position='none') + 
          labs(x='Cycle', y='Quadrat % Frequency')+
          scale_x_discrete(breaks=c(1,2,3))+
          scale_y_continuous(limits=c(-10,110),breaks=c(0,20,40,60,80,100),
                             labels=c(0,20,40,60,80,100)))
}

# Park-level plot for quad frequency by guild
plotQFreqParkGuild<-function(df){ 
  print(ggplot(df, aes(x=cycle2, y=mean, group=guild))+ 
          facet_wrap(~park,ncol=8,scales='free')+
          geom_errorbar(aes(ymin=lower, ymax=upper, x=cycle2,
                            colour=factor(guild)), width=0.1,size=1, na.rm=TRUE)+
          geom_line(aes(y=mean, x=cycle2,colour=factor(guild), linetype=sign), na.rm=TRUE)+
          geom_point(aes(y=mean, x=cycle2,colour=factor(guild), shape=sign), 
                     size=2, stroke=1.5, fill='white', na.rm=TRUE)+
          scale_shape_manual(values=c(21,19))+
          scale_fill_manual(values=c('white'))+
          scale_linetype_manual(values=c('dashed','solid'))+
          scale_color_manual(values=c('Gold', 'ForestGreen','IndianRed','RoyalBlue'))+
          theme_bw()+
          theme(axis.text=element_text(size=11),axis.title=element_text(size=12),
                plot.margin=unit(c(0.4,0.4,0.5,0.3),'lines'),
                plot.title=element_text(hjust=0.5, size=12, margin=margin(t=10,b=-15)),
                panel.background=element_blank(),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                axis.line = element_blank(), 
                legend.position='none') + 
          labs(x='Cycle', y='Quadrat % Frequency')+
          scale_x_discrete(breaks=c(1,2,3))+
          scale_y_continuous(limits=c(-10,110),breaks=c(0,20,40,60,80,100),
                             labels=c(0,20,40,60,80,100)))
} # plots results for each park

# Park-level plot for quad frequency by species
plotQFreqParkSpecies<-function(df){ 
  print(ggplot(df, aes(x=cycle2, y=mean, group=species))+ 
          facet_wrap(~park,ncol=5,scales='free')+
          geom_errorbar(aes(ymin=lower, ymax=upper, x=cycle2,
                            colour=factor(species)), width=0.1,size=1, na.rm=TRUE)+
          geom_line(aes(y=mean, x=cycle2,colour=factor(species), linetype=sign), na.rm=TRUE)+
          geom_point(aes(y=mean, x=cycle2,colour=factor(species), shape=sign), 
                     size=1.8, stroke=1.5, fill='white', na.rm=TRUE)+
          scale_shape_manual(values=c(21,19))+
          scale_fill_manual(values=c('white'))+
          scale_linetype_manual(values=c('dashed','solid'))+ theme_bw()+
          scale_color_manual(values = colramp)+ 
          theme(axis.text=element_text(size=11),axis.title=element_text(size=12),
                plot.margin=unit(c(0.4,0.4,0.5,0.3),'lines'),
                plot.title=element_text(hjust=0.5, size=12, margin=margin(t=10,b=-15)),
                panel.background=element_blank(),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                axis.line = element_blank())+#, 
          #  legend.position='bottomright') + 
          labs(x='Cycle', y='Quadrat % Frequency')+
          scale_x_discrete(breaks=c(1,2,3))+
          scale_y_continuous(limits=c(-10,100),breaks=c(0,20,40,60,80,100),labels=c(0,20,40,60,80,100)))
} 

# Park-level plot for total invasive quad richness
plotQRichParkTotal<-function(df){ 
  print(ggplot(df, aes(x=cycle2, y=mean, group=park))+ 
          facet_wrap(~park,ncol=8,scales='free')+
          geom_errorbar(aes(ymin=lower, ymax=upper, x=cycle2), width=0.1,size=1, na.rm=TRUE)+
          #geom_smooth(method='lm',se=F, linetype=sign)+
          geom_line(aes(y=mean, x=cycle2, linetype=sign), na.rm=TRUE)+
          geom_point(aes(y=mean, x=cycle2, shape=sign), 
                     size=2, stroke=1.5, fill='white', na.rm=TRUE)+
          scale_shape_manual(values=c(21,19))+
          scale_fill_manual(values=c('white'))+
          scale_linetype_manual(values=c('dashed','solid'))+
          scale_color_manual(values=c('black'))+
          theme_bw()+
          theme(axis.text=element_text(size=11),axis.title=element_text(size=12),
                plot.margin=unit(c(0.4,0.4,0.5,0.3),'lines'),
                plot.title=element_text(hjust=0.5, size=12, margin=margin(t=10,b=-15)),
                panel.background=element_blank(),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                axis.line = element_blank(), 
                legend.position='none') + 
          labs(x='Cycle', y='Quadrat Richness')+
          scale_x_discrete(breaks=c(1,2,3))+
          scale_y_continuous(limits=c(-1,5),breaks=c(0,1,2,3,4,5),
                             labels=c(0,1,2,3,4,5)))
}

# Park-level plot for quad richness by guild
plotQRichParkGuild<-function(df){ 
  print(ggplot(df, aes(x=cycle2, y=mean, group=guild))+ 
          facet_wrap(~park,ncol=8,scales='free')+
          geom_errorbar(aes(ymin=lower, ymax=upper, x=cycle2,
                            colour=factor(guild)), width=0.1,size=1, na.rm=TRUE)+
          geom_line(aes(y=mean, x=cycle2,colour=factor(guild), linetype=sign), na.rm=TRUE)+
          geom_point(aes(y=mean, x=cycle2,colour=factor(guild), shape=sign), 
                     size=2, stroke=1.5, fill='white', na.rm=TRUE)+
          scale_shape_manual(values=c(21,19))+
          scale_fill_manual(values=c('white'))+
          scale_linetype_manual(values=c('dashed','solid'))+
          scale_color_manual(values=c('Gold', 'ForestGreen','IndianRed','RoyalBlue'))+
          theme_bw()+
          theme(axis.text=element_text(size=11),axis.title=element_text(size=12),
                plot.margin=unit(c(0.4,0.4,0.5,0.3),'lines'),
                plot.title=element_text(hjust=0.5, size=12, margin=margin(t=10,b=-15)),
                panel.background=element_blank(),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                axis.line = element_blank(), 
                legend.position='none') + 
          labs(x='Cycle', y='Quadrat Richness')+
          scale_x_discrete(breaks=c(1,2,3))+
          scale_y_continuous(limits=c(-1,2),breaks=c(0,1,2),
                             labels=c(0,1,2)))
} # plots results for each park

# Park level plot for plot frequency total
plotFreqParkTotal<-function(df){ 
  print(ggplot(df, aes(x=cycle2, y=pfreq, group=park))+ 
      facet_wrap(~park,ncol=8,scales='free')+
      geom_line(aes(y=pfreq, x=cycle2, linetype=sign), na.rm=TRUE)+
      geom_point(aes(y=pfreq, x=cycle2, shape=sign), 
        size=2, stroke=1.5, fill='white', na.rm=TRUE)+
      scale_shape_manual(values=c(21,19))+
      scale_fill_manual(values=c('white'))+
      scale_linetype_manual(values=c('dashed','solid'))+
      scale_color_manual(values=c('black'))+
      theme_bw()+
      theme(axis.text=element_text(size=11),axis.title=element_text(size=12),
        plot.margin=unit(c(0.4,0.4,0.5,0.3),'lines'),
        plot.title=element_text(hjust=0.5, size=12, margin=margin(t=10,b=-15)),
        panel.background=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_blank(), 
        legend.position='none') + 
      labs(x="Cycle", y='Plot % Frequency')+
      scale_x_discrete(breaks=c(1,2,3))+
      scale_y_continuous(limits=c(-10,110),breaks=c(0,25,50,75,100),labels=c(0,25,50,75,100)))
}

# Park level plot for plot frequency by guild
plotFreqParkGuild<-function(df){ 
  print(ggplot(df, aes(x=cycle2, y=pfreq, group=guild))+ 
      facet_wrap(~park,ncol=8,scales='free')+
      geom_line(aes(y=pfreq, x=cycle2,colour=factor(guild), linetype=sign), na.rm=TRUE)+
      scale_shape_manual(values=c(21,19))+
      geom_point(aes(y=pfreq, x=cycle2,colour=factor(guild), shape=sign), 
        size=1.8, stroke=1.5, fill='white', na.rm=TRUE)+
      #      scale_fill_manual(values=c('white'))+
      scale_linetype_manual(values=c('dashed','solid'))+
      scale_color_manual(values=c('IndianRed','ForestGreen','Gold','RoyalBlue'))+
      theme_bw()+
      theme(axis.text=element_text(size=11),axis.title=element_text(size=12),
        plot.margin=unit(c(0.4,0.4,0.5,0.3),'lines'),
        plot.title=element_text(hjust=0.5, size=12, margin=margin(t=10,b=-15)),
        panel.background=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_blank(), 
        legend.position='none') + 
      labs(x='Cycle', y='Plot % Frequency')+
      scale_x_discrete(breaks=c(1,2,3))+
      scale_y_continuous(limits=c(-5,105),breaks=c(0,20,40,60,80,100),labels=c(0,20,40,60,80,100)))
} # plots results for each park

# Generalized plotting function for total invasive by metric
# Plots one metric at a time and requires metric and y-axis name
plotTotalByMetrics<-function(df, metric=NA, axis_name=NA){
  print(ggplot(df,aes(x=cycle,y=mean, group=park))+
          geom_line(aes(x=cycle,y=mean,linetype=sign,color=sign),lwd=1, alpha=0.8, na.rm=TRUE)+
          scale_linetype_manual(values=c('dashed','solid'))+ labs(title=metric)+
          scale_color_manual(values=c('#909090','#686868'))+ 
          geom_point(aes(x=cycle,y=mean,colour=sign,shape=sign,size=sign),stroke=1.5,fill='white', na.rm=TRUE)+
          scale_size_manual(values=c(3,3))+
          scale_shape_manual(values=c(21,19))+theme_bw()+
          scale_fill_manual(values=c('white',NA))+
          theme(panel.background=element_blank(),legend.position='none',
                plot.margin=unit(c(0.1,0,0.1,0),'cm'),
                plot.title=element_text(hjust=0.5, size=11, margin=margin(t=7),face='bold'),
                panel.grid.major=element_blank(),panel.grid.minor = element_blank())+
          labs(x=NULL, y=axis_name)+scale_x_continuous(breaks=c(1,2,3)))
} #

# Generalized plotting function for guilds by metric
# Plots one guild at a time and requires metric and y-axis name
plotGuildsByMetrics<-function(df, guild_name=NULL, axis_name=NULL, yrange=c(0,40)){
  df<-df %>% filter(guild==guild_name) %>% droplevels()
  print(ggplot(df,aes(x=cycle,y=mean,group=park))+
          geom_line(aes(x=cycle,y=mean,linetype=sign,color=sign),lwd=1, alpha=0.8, na.rm=TRUE)+
          scale_linetype_manual(values=c('dashed','solid'))+ 
          scale_color_manual(values=c('#909090','#686868'))+
          scale_size_manual(values=c(3,2))+
          geom_point(aes(x=cycle,y=mean,colour=sign,shape=sign,size=sign),stroke=1,fill='white',
                     na.rm=TRUE)+
          scale_shape_manual(values=c(21,19))+theme_bw()+
          scale_fill_manual(values=c('white',NA))+
          theme(panel.background=element_blank(),legend.position='none',
                plot.title=element_text(hjust=0.5, size=11, margin=margin(t=10,b=-15),face='bold'),
                panel.grid.major=element_blank(),panel.grid.minor = element_blank())+
          labs(x=NULL, y=axis_name, title=guild_name)+scale_x_continuous(breaks=c(1,2,3))+
          ylim(yrange))
} #

plotFreqByGuilds<-function(df, guild_name=NULL, axis_name=NULL){
  df<-df %>% filter(guild==guild_name) %>% droplevels()
  print(ggplot(df,aes(x=cycle,y=pfreq,group=park))+
      geom_line(aes(x=cycle,y=pfreq,linetype=sign,color=sign),lwd=1, alpha=0.8, na.rm=TRUE)+
      scale_linetype_manual(values=c('dashed','solid'))+ 
      scale_color_manual(values=c('#909090','#686868'))+
      scale_size_manual(values=c(3,2))+
      geom_point(aes(x=cycle,y=pfreq,colour=sign,shape=sign,size=sign),stroke=1,fill='white',
        na.rm=TRUE)+
      scale_shape_manual(values=c(21,19))+theme_bw()+
      scale_fill_manual(values=c('white',NA))+
      theme(panel.background=element_blank(),legend.position='none',
        plot.title=element_text(hjust=0.5, size=11, margin=margin(t=10,b=-15),face='bold'),
        panel.grid.major=element_blank(),panel.grid.minor = element_blank())+
      labs(x=NULL, y=axis_name, title=guild_name)+scale_x_continuous(breaks=c(1,2,3)))
} #
#------------------------------------
# Species-level plots for summary
#------------------------------------
plotSpeciesTrends<-function(df, species_name=NULL, y_axis=NULL, yrange=c(0,50), col=NULL){
  df<-df %>% filter(species==species_name) %>% droplevels()
    print(ggplot(df,aes(x=cycle,y=mean, group=park))+
            geom_line(aes(x=cycle,y=pfreq,color=col),lwd=1,alpha=0.8,na.rm=T)+
            geom_errorbar(aes(ymin=lower, ymax=upper, x=cycle,
                              colour=col), width=0.1,size=1, na.rm=TRUE)+
            geom_point(aes(x=cycle,y=mean,colour=col),stroke=1,na.rm=T)+
            theme_bw()
          )
}



