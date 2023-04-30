install.packages("RUnit")
install.packages("testthat") #For unit testing



library(here) # for easy and clear relative paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set work directory to this file
here::i_am("TaskReadme.txt")

library(RUnit) #For unit testing
library(testthat)

source("RUNSCRIPT.R")

readingFile <- function(filename){
  read.csv(filename)
}


test_that("Our Data", {
  expect_false(expect_null(readingFile("output.csv")))
})