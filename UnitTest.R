#######################Initialization#########################
#install.packages("testthat") #For unit testing, run just once

library(testthat) #For unit testing
library(R.utils)

#source("RUNSCRIPT.R")  #Run this first, to generate an output

##########################Functions##########################
#File Reader function (to be tested)
readingFile <- function(filename){
  read.csv(filename)
}

#Expect 
#I used this as a reference: 
#https://testthat.r-lib.org/articles/custom-expectation.html
expect_min_row_amount <- function(list, min) {
  # Capture list, label
  act <- quasi_label(rlang::enquo(list), arg = "list")
  
  act$n <- nrow(act$val)
  if(act$n >= min){
    succeed()
    return(invisible(act$val))#Unneccessary in our case, but a good practice
  }
  message <- sprintf("%s has %i amount of rows, less than %i.", act$lab, act$n, min)
  fail(message)
}

##############################Testing#################################

test_that("Our Data", {
  expect_false(is.null(readingFile("CalculatedOutputCSV.csv")))
  expect_false(isZero(nrow(readingFile("CalculatedOutputCSV.csv"))))
  expect_false(isZero(ncol(readingFile("CalculatedOutputCSV.csv"))))
  expect_min_row_amount(readingFile("CalculatedOutputCSV.csv"), 10000)
  #expect_min_row_amount(readingFile("CalculatedOutputCSV.csv"), 50000)
})