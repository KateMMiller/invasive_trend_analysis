#----------------------------
# Source Scripts to rerun all analyses and update results and plots
#----------------------------
library(beepr)

# List files in project folder: scripts
list.files(path="./scripts", pattern="\\02") #lmer and glmer scripts
list.files(path='./scripts', pattern="\\03") # plotting scripts
#==================================
# Run model scripts for each metric
#==================================
# Total Invasives
#-------------------
source('./scripts/02_Average_quadrat_cover-total.R')
head(results_final_AC_T)
beep(sound=2)

source('./scripts/02_Average_quadrat_frequency-total.R')
head(results_final_QF_T)
beep(sound=2)

source('./scripts/02_Average_quadrat_richness-total.R')
head(results_final_QR_T)
beep(sound=2)

source('./scripts/02_Plot_frequency-total.R')
head(results_final_PF_T)
beep(sound=2)

#-------------------
# Invasive by Guild
#-------------------
source('./scripts/02_Average_quadrat_cover-by_guild.R')
head(results_final_AC_G)
beep(sound=2)

source('./scripts/02_Average_quadrat_frequency-by_guild.R')
head(results_final_QF_G)
beep(sound=2)

source('./scripts/02_Average_richness_cover-by_guild.R')
head(results_final_QR_G)
beep(sound=2)

source('./scripts/02_Plot_frequency-by_guild.R')
head(results_final_PF_G)
beep(sound=2)

#==================================
# Update plotting scripts 
#==================================
source('./scripts/03_Plotting_Results.R')
head(df)
beep(sound=2)
