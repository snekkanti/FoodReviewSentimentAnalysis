packages <- c("readr", "tm", "ztable", "wordcloud", "png", "parallel", "plot3D", "textcat", "tidytext", "RTextTools")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}


library(data.table)
library(readr)
library(tm)
library(wordcloud)
library(png)
library(DBI)
library(RSQLite)
library(ggplot2)
library(scales)
library(plot3D)
library(dplyr)
library(stringr)
library(magrittr)
library(textcat)
library(tidytext)
library(RTextTools)
library(ztable)
setwd("~/MachineLearning/FoodReviewSentimentAnalysis")
con = dbConnect(SQLite(), dbname="database.sqlite")
# get a list of all tables
alltables = dbListTables(con)
# get the Reviews as a data.frame
allreviews = dbGetQuery( con,'select * from Reviews LIMIT 10000' )
# allreviews <- read.csv("/Users/shnekkanti/Box Sync/AmazonFoodReviewsSubset.csv")
allreviews$ProductId <- NULL
allreviews$UserId <- NULL
allreviews$ProfileName <- NULL
allreviews$review.length <- nchar(allreviews$Text)

# Loading the first sentiment score lexicon
AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)
## Print in table format to help understand the words and scores associated.  
ztab <- ztable(as.data.frame(head(AFINN, 10)), digits = 1
               , caption = 'AFINN Score')
# Loading the second sentiment score lexicon
Bing <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(word, bing_sentiment = sentiment)

## Print in table format to help understand the words and scores associated.  
ztab <- ztable(as.data.frame(head(Bing, 10)), digits = 1
               , caption = 'Bing Score')

# "tidying" up the data (1 word per row) and adding the sentiment scores for each word
food_review_words <- allreviews %>%
  unnest_tokens(word, Text) %>%
  select(-c(Summary, HelpfulnessNumerator,HelpfulnessDenominator)) %>%
  left_join(AFINN, by = "word") %>%
  left_join(Bing, by = "word")

#Filter the data to display rows with proper score. Just for viewing purpose
filteredData <- subset(food_review_words, bing_sentiment == "positive" | bing_sentiment == "negative")
ztab <- ztable(as.data.frame(head(filteredData , 10)), digits = 1
               , caption = 'Sentiment score for each word')
ztab

# Grouping by mean for observation 
  review_mean_sentiment <- food_review_words %>%
    group_by(Id, Score) %>%
    summarize(mean_sentiment = mean(afinn_score, na.rm = TRUE))
  # Plotting the result
  theme_set(theme_bw())
  ggplot(review_mean_sentiment, aes(Score, mean_sentiment, group = Score)) + 
    geom_boxplot() +
    ylab("Average sentiment score")



# Transferring the results to the new dataset
review_mean_sentiment <- review_mean_sentiment %>%
  select(-Score) %>%
  data.table()
cleaned_food_reviews <- allreviews %>%
  left_join(review_mean_sentiment, by = "Id")


# Same as previous, but with the median
review_median_sentiment <- food_review_words %>%
  group_by(Id, Score) %>%
  summarize(median_sentiment = median(afinn_score, na.rm = TRUE))
theme_set(theme_bw())
ggplot(review_median_sentiment, aes(Score, median_sentiment, group = Score)) +
  geom_boxplot() +
  ylab("Median sentiment score")

# Transferring the results to our new dataset
review_median_sentiment <- review_median_sentiment %>%
  select(-Score) %>%
  data.table()
cleaned_food_reviews <- cleaned_food_reviews %>%
  left_join(review_median_sentiment, by = "Id")


# Counting the number of negative words per review according to AFINN lexicon
review_count_afinn_negative <- food_review_words %>%
  filter(afinn_score < 0) %>%
  group_by(Id, Score) %>%
  summarize(count_afinn_negative = n())
# Transferring the results to our dataset
review_count_afinn_negative <- review_count_afinn_negative %>%
  select(-Score) %>%
  data.table()
cleaned_food_reviews <- cleaned_food_reviews %>%
  left_join(review_count_afinn_negative, by = "Id")

# Counting the number of positive words per review according to AFINN lexicon
review_count_afinn_positive <- food_review_words %>%
  filter(afinn_score > 0) %>%
  group_by(Id, Score) %>%
  summarize(count_afinn_positive = n())
# Transferring the results to our dataset
review_count_afinn_positive <- review_count_afinn_positive %>%
  select(-Score) %>%
  data.table()
cleaned_food_reviews <- cleaned_food_reviews %>%
  left_join(review_count_afinn_positive, by = "Id")




# Counting the number of negative words per review according to BING lexicon
review_count_bing_negative <- food_review_words %>%
  filter(bing_sentiment == "negative" ) %>%
  group_by(Id, Score) %>%
  summarize(count_bing_negative = n())
# Transferring the results to our dataset
review_count_bing_negative <- review_count_bing_negative %>%
  select(-Score) %>%
  data.table()
cleaned_food_reviews <- cleaned_food_reviews %>%
  left_join(review_count_bing_negative, by = "Id")

# Counting the number of positive words per review according to BING lexicon
review_count_bing_positive <- food_review_words %>%
  filter(bing_sentiment == "positive") %>%
  group_by(Id, Score) %>%
  summarize(count_bing_positive = n())
# Transferring the results to our dataset
review_count_bing_positive <- review_count_bing_positive %>%
  select(-Score) %>%
  data.table()
cleaned_food_reviews <- cleaned_food_reviews %>%
  left_join(review_count_bing_positive, by = "Id")

# Writing the data to file for future analyses
write.csv(cleaned_food_reviews, "FoodReviewsCleanData2.csv", row.names = FALSE)

