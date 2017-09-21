
packages <- c("xgboost", "ROCR", "caret", "png", "dplyr", "RTextTools", "textcat", "tidytext", "RTextTools")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}


### Libraries to load
library(data.table)
library(dplyr)
library(caret)
library(RTextTools)
library(xgboost)
library(ROCR)


setwd("~/Box Sync/FoodReviews")
data <- read.csv("FoodReviewsCleanData.csv", stringsAsFactors = FALSE)
