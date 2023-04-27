library(here) # for easy and clear relative paths
library(testthat)

readingFile <- function(filename){
  read.csv(filename)
}


test_that("Our Data", {
  expect_false(expect_null(readingFile("output.csv"), 0))
})