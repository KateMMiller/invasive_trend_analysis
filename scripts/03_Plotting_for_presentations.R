library(tidyverse)
library(gganimate)
altrenderer<-gifski_renderer(loop=F) # Set gif in gganimate to only run once

# Plot function for gif
plot_metric_by_park<-function(df, plot_title=NA, axis_name=NA){
  col<-as.character(df$col)
  names(col)<-df$col
  ggplot(df,aes(x=cycle,y=mean, group=park, color=col))+
    geom_line(aes(x=cycle,y=mean,linetype=sign),lwd=1, alpha=0.8, na.rm=TRUE)+
    scale_linetype_manual(values=c('dashed','solid'))+ labs(title=plot_title)+
    scale_color_manual(values=col)+
    geom_point(aes(x=cycle,y=mean,colour=col,shape=sign,size=sign),stroke=1.5,fill='white', na.rm=TRUE)+
    scale_size_manual(values=c(3,3))+
    scale_shape_manual(values=c(21,19))+theme_bw()+
    scale_fill_manual(values=c('white',NA))+
    theme(panel.background=element_blank(),legend.position='none',
          plot.margin=margin(t=5,b=5,r=10,l=5),
          axis.title.y = element_text(size=15, margin=margin(t=5, b=5, r=10, l=5)), 
          axis.title.x = element_text(size=15, margin=margin(t=5, b=5, r=10, l=5)), 
          axis.text = element_text(size=15),
          plot.title=element_text(hjust=0.5, size=16, margin=margin(t=7,b=10),face='bold'),
          panel.grid.major=element_blank(),panel.grid.minor = element_blank())+
    labs(x="Cycle", y=axis_name)+scale_x_continuous(breaks=c(1,2,3))
} #

freqplot_metric_by_park<-function(df, plot_title=NA, axis_name=NA){
  col<-as.character(df$col)
  names(col)<-df$col
  ggplot(df,aes(x=cycle,y=pfreq, group=park, color=col))+
    geom_line(aes(x=cycle,y=pfreq,linetype=sign),lwd=1, alpha=0.8, na.rm=TRUE)+
    scale_linetype_manual(values=c('dashed','solid'))+ labs(title=plot_title)+
    scale_color_manual(values=col)+
    geom_point(aes(x=cycle,y=pfreq,colour=col,shape=sign,size=sign),stroke=1.5,fill='white', na.rm=TRUE)+
    scale_size_manual(values=c(3,3))+
    scale_shape_manual(values=c(21,19))+theme_bw()+
    scale_fill_manual(values=c('white',NA))+
    theme(panel.background=element_blank(),legend.position='none',
          plot.margin=margin(t=5,b=5,r=10,l=5),
          axis.title.y = element_text(size=15, margin=margin(t=5, b=5, r=10, l=5)), 
          axis.title.x = element_text(size=15, margin=margin(t=5, b=5, r=10, l=5)), 
          axis.text = element_text(size=15),
          plot.title=element_text(hjust=0.5, size=16, margin=margin(t=7,b=10),face='bold'),
          panel.grid.major=element_blank(),panel.grid.minor = element_blank())+
    labs(x="Cycle", y=axis_name)+scale_x_continuous(breaks=c(1,2,3))
}

# Read in data
#--- Average Cover
avgcov_total<-read.csv("./results/results_avecov-total-response_NP.csv")[,-1]
avgcov_coef<-read.csv("./results/results_avecov-total-coefs_NP.csv")
avgcov_slope<-avgcov_coef %>% filter(coef=="Slope") %>% droplevels() %>% select(park,estimate)
avgcov_comb<-merge(avgcov_total, avgcov_slope, by="park", all.x=T)

avgcov_comb<- avgcov_comb %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign), col=ifelse(estimate<0 & sign==1,'blue', 
                                                                   ifelse(estimate>0 & sign==1,'red','DimGrey')))
avgcov_comb$park<-reorder(avgcov_comb$park,-avgcov_total$lat.rank)

#--- Quad frequency
qfreq_total<-read.csv("./results/results_qfreq-total-response_NP.csv")[,-1]
qfreq_coef<-read.csv('./results/results_qfreq-total-coefs_NP.csv')
qfreq_slope<-qfreq_coef %>% filter(coef=="Slope") %>% droplevels() %>% select(park,estimate)
qfreq_comb<-merge(qfreq_total,qfreq_slope, by="park",all.x=T)

qfreq_comb<- qfreq_comb %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle2),sign=as.factor(sign), col=ifelse(estimate<0 & sign==1,'blue', 
                                                                   ifelse(estimate>0 & sign==1,'red','DimGrey')))
qfreq_comb$park<-reorder(qfreq_comb$park,-qfreq_total$lat.rank)

#--- Plot frequency
pfreq_total_coefs<-read.csv('./results/results_PFreq-total-coefs.csv')
pfreq_total_slopes<-pfreq_total_coefs %>% filter(coef=='Slope') %>% droplevels()

df<-read.csv("./data/NETN-MIDN-ERMN-NCRN_total_invasives.csv")#[,-c(1,2)]
df_pf<- df %>% arrange(park,plot_name,cycle) %>% select(park,plot_name,cycle,lat.rank,plot.freq)
df_pf2<-df_pf %>% filter(!(park %in% c('SAHI','WOTR'))) %>% droplevels()
df_pf3<-df_pf2 %>% group_by(park,cycle) %>% 
  summarise(plot.freq=sum(plot.freq), num.plots=n(),pfreq=round(((plot.freq/num.plots)*100),2), lat.rank=first(lat.rank))

pfreq_total_comb<-merge(df_pf3,pfreq_total_slopes, by=c('park'),all.x=T)
pfreq_total_comb$sign[is.na(pfreq_total_comb$sign)]<-0

pfreq_total_comb<- pfreq_total_comb %>% arrange(lat.rank) %>% 
  mutate(cycle2=as.factor(cycle),sign=as.factor(sign), park=reorder(park, -lat.rank), 
         odds=ifelse(is.na(estimate),1, exp(estimate)),
         col=ifelse(odds<1 & sign==1,'blue', 
                    ifelse(odds>1 & sign==1,'red','DimGrey'))) %>% arrange(park,cycle)

head(pfreq_total_comb)

# Create the gifs
avgcov<-plot_metric_by_park(avgcov_comb, "Average % Cover", '% Cover')+   
  geom_point(aes(group = seq_along(cycle),colour=col,shape=sign,size=sign),
             stroke=1.5,fill='white', na.rm=TRUE, size=3)+
  transition_reveal(cycle)

animate(avgcov,fps=15)

anim_save('./results/figures/total_invasive_cover.gif', avgcov, renderer=altrenderer, fps=20)

qfreq<-plot_metric_by_park(qfreq_comb,"Quadrat % Frequency", "% Frequency")+ 
  geom_point(aes(group = seq_along(cycle),colour=col,shape=sign,size=sign),
             stroke=1.5,fill='white', na.rm=TRUE, size=3)+
  transition_reveal(cycle)

anim_save('./results/figures/total_invasives_qfreq.gif', qfreq, renderer=altrenderer, fps=20)

pfreq<-freqplot_metric_by_park(pfreq_total_comb, 'Plot % Frequency', '% Frequency')+ 
  geom_point(aes(group = seq_along(cycle),colour=col,shape=sign,size=sign),
             stroke=1.5,fill='white', na.rm=TRUE, size=3)+
  transition_reveal(cycle)

anim_save('./results/figures/total_invasives_pfreq.gif', pfreq, renderer=altrenderer, fps=20)


ggsave("./results/figures/total_invasives_cover_static.jpg",
       plot_metric_by_park(avgcov_comb, "Average % Cover", '% Cover') 
       )

ggsave("./results/figures/total_invasives_quadfreq_static.jpg",
       plot_metric_by_park(qfreq_comb,  "Quadrat % Frequency", '% Frequency') 
)

ggsave("./results/figures/total_invasives_plotfreq_static.jpg",
       freqplot_metric_by_park(pfreq_total_comb,"Plot % Frequency", '% Frequency') 
)


