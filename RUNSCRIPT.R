install.packages("here")

library(here) # for easy and clear relative paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set work directory to this file
here::i_am("TaskReadme.txt")

library(readr) #For storing large data into .csv files

source("utils.R") # common utility functions for this repository
source("calculator_utils.R") # specific model utility functions

########################Solution##########################

IAM.OUTPUT.SHORT.FILENAME = "SHAPE-final"  #Appropriate name for the calculation
source("calculator_output_lognormal-calculation.R")
#Now the data is stored in two environmental variables: scenario.assessment.data, scenario.assessment.calculated

#Print out the "untouched" data (this is not neccessary, because the calculated list contains every information the raw data contains)
write_csv(scenario.assessment.data, "ScenarioDataOutputCSV.csv")
save(scenario.assessment.data, file = "ScenarioDataOutputR.RData")

#Print out the calculated data
write_csv(scenario.assessment.calculated, "CalculatedOutputCSV.csv")
save(scenario.assessment.calculated, file = "CalculatedOutputR.RData")


#################Original "solution" (working using the wrong files)###################

#library(data.table)
#library(reshape2)
#library(tidyverse)
#
##1) Read the data, store it as lists
#scenario <- readRDS("Data/scenario_FE_SHAPE-final.Rdata")
#dle <- readRDS("Data/projected_dle-total-and-sectoral-scaled_SHAPE-final.Rdata")
#energy <- readRDS("Data/projected_energy_inequality_SHAPE-final.Rdata")
#
##2) Combine them into one superlist
#comb12 = full_join(dle, scenario)
#combined = full_join(comb12,energy)
#combined %>% group_by(model, scenario, iso, variable, year) %>% dplyr::summarize(model = paste(na.omit(model)), scenario = paste(na.omit(scenario)), iso = paste(na.omit(iso)), variable = paste(na.omit(variable)), year = paste(na.omit(year)))
#
##3) Create the output
#capture.output(combined, file = "output.csv")
#capture.output(combined, file = "output_txt.txt")

