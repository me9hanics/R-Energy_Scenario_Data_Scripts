install.packages("here")
install.packages("testthat")
#("C:/InterviewTask/Data/scenario_FE_SHAPE-final.RData")
library(here) # for easy and clear relative paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set work directory to this file
here::i_am("TaskReadme.txt")
library(data.table)
#library(reshape2)
library(tidyverse)
library(RUnit)

source("utils.R") # common utility functions for this repository
source("calculator_utils.R") # specific model utility functions

scenario <- readRDS("Data/scenario_FE_SHAPE-final.Rdata")
dle <- readRDS("Data/projected_dle-total-and-sectoral-scaled_SHAPE-final.Rdata")
energy <- readRDS("Data/projected_energy_inequality_SHAPE-final.Rdata")

comb12 = full_join(dle, scenario)
combined = full_join(comb12,energy)
combined %>% group_by(model, scenario, iso, variable, year) %>% dplyr::summarize(model = paste(na.omit(model)), scenario = paste(na.omit(scenario)), iso = paste(na.omit(iso)), variable = paste(na.omit(variable)), year = paste(na.omit(year)))

capture.output(combined, file = "output.csv")
capture.output(combined, file = "output_txt.txt")

