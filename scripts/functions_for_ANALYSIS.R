#-----------------------------------
# Code for Park-level Invasive Trend Analysis
#-----------------------------------
## @knitr codesetup

library(knitr)
library(tidyverse) # attaches most of the important packages in the tidyverse
library(lme4) # for glmer with Poisson
library(modelr) #for handling multiple models in tidyverse
library(broom.mixed)# for better model summary tables than default in nlme
library(sjstats) # for overdisp
#library(lmerTest) # for p-values based on Satterthwaite-approximated degrees of freedom 
# I'm not using p-values anymore- using empiracle CIs instead.
# attaching lmerTest automatically incorporates this test in the lmer output.
library(prediction)

options("scipen"=100, "digits"=4) # keeps TSN numbers as numbers 

glmerCtlList <- glmerControl(optimizer=c("bobyqa","Nelder_Mead"),
  optCtrl=list(maxfun=2e10000)) # set controls for glmer so longer before times out

# Modeling and plotting functions 

findSmallNZ<-function(df,varName){
  df_col<-select(df,!!sym(varName))
  df_col2<-filter(df_col,!!sym(varName)>0) %>% 
    summarise(sm.nz=min(!!sym(varName),na.rm=T))
  return(df_col2$sm.nz)
} #varName must be in quotes. 
#Find smallest NZ value to add to log transformations.

# Diagnostic plotting functions
residPlot<-function(df,resid,pred,park){
  print(ggplot(df,aes(pred,resid))+geom_point(aes(group=park))+geom_smooth(method='lm',se=F)+
      facet_wrap(~park,scales='free',ncol=4)+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title=element_text(hjust=0.5, size=12),panel.background = element_rect(fill=NA,color='DimGrey'), 
        axis.line = element_line(colour = "black"))+labs(title=paste('Residual Plot',analysis.title,sep=":")))
}

histPlot<-function(df,resid,park){
  print(ggplot(df,aes(resid))+geom_histogram()+facet_wrap(~park,scales='free',ncol=4)+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title=element_text(hjust=0.5, size=12),panel.background = element_rect(fill=NA,color='DimGrey'), 
        axis.line = element_line(colour = "black"))+labs(title=paste('Residual Plot',analysis.title,sep=":")))
}

# Store fixed effects and print park that simulation is on
fixed_fun<-function(model){
  cat(paste0(substr(model@frame$plot_name[[1]],1,4)," "))
  fixed<-fixef(model)
}

# Modeling functions for bootstraps
confFun<-function(model){
  if (getME(model,'p')>2){
    orig.df=data.frame(find_data(model))
    cat(paste0(substr(orig.df$plot_name[[1]],1,4)," "))
    newdf=orig.df %>% select(cycle,guild) %>% arrange(cycle,guild) %>% unique()
    predict(model,
      newdata=newdf,
      type='response',re.form=NA)
  } else {
    orig.df=data.frame(find_data(model))
    cat(paste0(substr(orig.df$plot_name[[1]],1,4), " "))
    newdf=orig.df %>% select(cycle) %>% arrange(cycle) %>% unique()
    predict(model,newdata=newdf, type='response', re.form=NA)} 
}# re.form=NA means that the random effects remain fixed.

confFunSpp<-function(model){
  if (getME(model,'p')>2){
    orig.df=data.frame(find_data(model))
    cat(paste0(substr(orig.df$plot_name[[1]],1,4)," "))
    newdf=orig.df %>% select(cycle,species) %>% arrange(cycle,species) %>% unique()
    predict(model,
            newdata=newdf,
            type='response',re.form=NA)
  } else {
    orig.df=data.frame(find_data(model))
    cat(paste0(substr(orig.df$plot_name[[1]],1,4), " "))
    newdf=orig.df %>% select(cycle) %>% arrange(cycle) %>% unique()
    predict(model,newdata=newdf, type='response', re.form=NA)} 
}# re.form=NA means that the random effects remain fixed.


getColNames<-function(model){
  if (getME(model,'p')>2){
    orig.df=data.frame(find_data(model))
    newdf=orig.df %>% select(cycle,guild) %>% arrange(cycle,guild) %>% unique()
    cols=paste0('c',newdf$cycle,"_",newdf$guild)
  } else {
    orig.df=data.frame(find_data(model))
    newdf=orig.df %>% select(cycle) %>% arrange(cycle) %>% unique()
    cols=paste0('c',newdf$cycle)
  } 
  return(cols)
} # sets levels of guild and cycles in the correct order per park

getColNamesSpp<-function(model){
  if (getME(model,'p')>2){
    orig.df=data.frame(find_data(model))
    newdf=orig.df %>% select(cycle,species) %>% arrange(cycle,species) %>% unique()
    cols=paste0('c',newdf$cycle,"_",newdf$species)
  } else {
    orig.df=data.frame(find_data(model))
    newdf=orig.df %>% select(cycle) %>% arrange(cycle) %>% unique()
    cols=paste0('c',newdf$cycle)
  } 
  return(cols)
} # sets levels of guild and cycles in the correct order per park


setColNames<-function(conf.est,cols){ 
  boot.t<-conf.est$t
  colnames(boot.t)<-cols
  return(boot.t)
} # sets the column names for t


bootCI<-function(boot.t){
  boot.ci<-data.frame(apply(boot.t[,1:ncol(boot.t)],2,quantile,probs=c(0.025,0.975), na.rm=T))
} # calculates the empirical CIs (ie no normality assumption) 
# based on all cycle*guild or just cycle combinations

bootMean<-function(boot.t){
  boot.mean<-data.frame(apply(boot.t[,1:ncol(boot.t)],2,mean,na.rm=T))
} # calculates the empirical CIs (ie no normality assumption) 
# based on all cycle*guild or just cycle combinations
