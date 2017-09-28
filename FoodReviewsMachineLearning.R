
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


setwd("~/MachineLearning/FoodReviewSentimentAnalysis")
reviews <- read.csv("FoodReviewsCleanData.csv", stringsAsFactors = FALSE)

# Creating the binary outcome feature
reviews$good.read <- 0
reviews$good.read[reviews$Score == 4 | reviews$Score == 5] <- 1

trainIdx <- createDataPartition(reviews$good.read, 
                                p = .75,
                                list = FALSE,
                                times = 1)
train <- reviews[trainIdx, ]
test <- data.table(reviews[-trainIdx, ])


#### Creating the Document-Term Matrix ####

# Creating a DTM for the negative reviews
sparsity <- .99
bad.dtm <- create_matrix(train$Text[train$good.read == 0], 
                         language = "english",
                         removeStopwords = FALSE, 
                         removeNumbers = TRUE,
                         stemWords = FALSE, 
                         removeSparseTerms = sparsity) 
#Converting the DTM in a data frame
bad.dtm.df  <-  data.table(as.matrix(bad.dtm))

# Creating a DTM for the positive reviews
good.dtm <- create_matrix(train$Text[train$good.read == 1], 
                          language="english",
                          removeStopwords = FALSE, 
                          removeNumbers = TRUE,
                          stemWords = FALSE, 
                          removeSparseTerms = sparsity) 
good.dtm.df <- data.table(as.matrix(good.dtm))

# Joining the two DTM together
train.dtm.df <- bind_rows(bad.dtm.df, good.dtm.df)
train.dtm.df$Id <- c(train$Id[train$good.read == 0],
                            train$Id[train$good.read == 1])
train.dtm.df <- arrange(train.dtm.df, Id)
train.dtm.df$good.read  <- train$good.read

train.dtm.df <- train %>%
  select(-c(HelpfulnessNumerator, HelpfulnessDenominator, Score, Time, Summary, Text, good.read)) %>%
  inner_join(train.dtm.df, by = "Id") %>%
  select(-Id)

train.dtm.df[is.na(train.dtm.df)] <- 0


# Creating the test DTM
test.dtm <- create_matrix(test$Text, 
                          language = "english", 
                          removeStopwords = FALSE, 
                          removeNumbers = TRUE, 
                          stemWords = FALSE, 
                          removeSparseTerms = sparsity) 
test.dtm.df <- data.table(as.matrix(test.dtm))
test.dtm.df$Id <- test$Id
test.dtm.df$good.read <- test$good.read

test.dtm.df <- test %>%
  select(-c(HelpfulnessNumerator, HelpfulnessDenominator, Score, Time, Summary, Text, good.read)) %>%
  inner_join(test.dtm.df, by = "Id") %>%
  select(-Id)


# Ensuring that the test DTM has the same columns as the train dataset
test.dtm.df <- head(bind_rows(test.dtm.df, train.dtm.df[1, ]), -1)
test.dtm.df <- test.dtm.df %>% 
  select(one_of(colnames(train.dtm.df)))
test.dtm.df[is.na(test.dtm.df)] <- 0
test.dtm.df <- data.table(test.dtm.df)


##### Machine learning algorithm: XGBoost #####

XGB.train <-  as.matrix(select(train.dtm.df, -good.read),
                        dimnames = dimnames(train.dtm.df))
XGB.test <- as.matrix(select(test.dtm.df, -good.read),
                      dimnames = dimnames(test.dtm.df))
XGB.model <- xgboost(data = XGB.train, 
                     label = train.dtm.df$good.read, 
                     nrounds = 400, 
                     objective = "binary:logistic")

XGB.predict <- predict(XGB.model, XGB.test)

XGB.results <- data.frame(good.read = test$good.read, pred = XGB.predict)

ROCR.pred <- prediction(XGB.results$pred, XGB.results$good.read)
ROCR.perf <- performance(ROCR.pred, 'tnr','fnr') 


plot(ROCR.perf, colorize = TRUE)



ROCR.pos.perf <- performance(ROCR.pred, 'tpr','fpr') 
plot(ROCR.pos.perf, colorize = TRUE)


XGB.table  <- table(true = XGB.results$good.read, 
                    pred = as.integer(XGB.results$pred >= 0.80))
XGB.table
XGB.acc <- sum(diag(XGB.table)) / nrow(test)

### Feature analysis with XGBoost
names <- colnames(test.dtm.df)
importance.matrix <- xgb.importance(names, model = XGB.model)
xgb.plot.importance(importance.matrix[1:20, ])