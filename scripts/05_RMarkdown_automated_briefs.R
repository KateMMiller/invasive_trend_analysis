#-------------------------
# R Markdown Automated Reporting Loop
#-------------------------
library(tidyverse)
library(purrr)
library(knitr)
library(markdown)
library(rmarkdown)
#setwd('C:/NETN/Monitoring_Projects/Forest_Health/manuscripts/Invasive_trend_analysis')

sites<-read.csv('./RMarkdown/site_info_NETN-MIDN-ERMN-NCRN.csv')
sites<-sites %>% filter(park_code!="SHEN") %>% droplevels() #SHEN not included

getParkList<-function(netcode){
  df<-sites %>% filter(network_code==netcode) %>% select(park_code) %>% droplevels()
  plist<-as.character(df[,1])
  } # Makes a quoted vector of parks for each network

ermn<-getParkList('ERMN')
netn<-getParkList('NETN')
midn<-getParkList('MIDN') 
ncrn<-getParkList('NCRN')
ncbn<-getParkList('NCBN')

render_reports<-function(park_list){
  filepath=c('C:/NETN/Monitoring_Projects/Forest_Health/manuscripts/Invasive_trend_analysis/RMarkdown/')
  network<-sites %>% filter(park_code == park_list) %>% select(network_code) %>% 
    droplevels() %>% mutate(network_code=as.character(network_code))
  rmarkdown::render(input=paste0(filepath,'Template_Invasive_Trend_Brief.Rmd'),
                    params=list(park_code=park_list),
                    output_file=paste0("Invasive_Trend_Brief_",network,"_", park_list,".pdf"), 
                    output_dir=paste0(filepath,'reports')
    )
}

# furrr::future_map was actually slower than purrr::map

walk(ermn,render_reports)
walk(netn,render_reports)
walk(midn,render_reports)
walk(ncrn,render_reports)
walk(ncbn,render_reports)
