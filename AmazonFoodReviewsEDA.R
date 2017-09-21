# install.packages("readr")
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("png")
# install.packages("parallel")



library(readr)
library(tm)
library(wordcloud)
library(png)
library(DBI)
library(RSQLite)
library(ggplot2)
library(scales)

setwd("~/Box Sync")
con = dbConnect(SQLite(), dbname="database.sqlite")
# get a list of all tables
alltables = dbListTables(con)
# get the Reviews as a data.frame
p1 = dbGetQuery( con,'select * from Reviews LIMIT 10000' )


##I would like to start by looking at the distribution of ratings. 
##Took a sample of 10000 ratings and below is how the distribution looks like. 
barplot(table(as.factor(p1$Score)),
        ylim = c(0,50000),
        main = "Distribution of ratings")

##I also would like to start by looking at the distribution of review length. 

ReviewTextLength = nchar(p1$Text)
hist(ReviewTextLength,
     ylim = c(0,50000),
     main = "Distribution of review length" )

##Since there are very few reviews that are more than 2000 characters, I got rid of them to avoid skewing our analysis. 
p1$Text.length = nchar(p1$Text)
data <- p1$Text[nchar(p1$Text) <= 2000]
ReviewTextLength<-nchar(data)
hist(ReviewTextLength,
     ylim = c(0,20000),
     main = "Distribution of review length" )

##Here is how the distribution of review text length by rating. 
##(TODO: I need to make some change in ggplot2 code to modify the y axis limit. I need to explore more about this)

ggplot(p1, aes(x=as.factor(p1$Score), y=nchar(p1$Text))) +
  geom_boxplot(fill="slateblue", alpha=0.2) +
  scale_y_continuous(limits = c(0,2000), breaks = seq(0,100,0), expand=c(0,0))
  # xlim(2000, 10) +
  xlab("Ratings") +
  ylab( "Review Summary Length" )

##I would also like to see the Summary length distribution and below is the chart
summarydata <- p1$Summary[nchar(p1$Summary) <= 1000]
ReviewSummaryLength<-nchar(summarydata)
hist(ReviewSummaryLength,
     ylim = c(0,20000),
     main = "Distribution of review Summary length" )

##Distribution of review summary length by rating.
ggplot(p1, aes(x=as.factor(p1$Score), y=nchar(p1$Summary))) +
  geom_boxplot(fill="slateblue", alpha=0.2) +
  # xlim(2000, 10) +
  xlab("Ratings") +
  ylab( "Review Summary Length" )

##I have also created word cloud for review Text by cleaning up the text using tm package. 
##I still see some stop words in the world cloud which I need to work further

reviews_word_cloud <- function(reviewSummary) {
  # .libPaths(c(.libPaths(), "/Users/shnekkanti/Library/R/3.4/library"))
  # .libPaths(c(.libPaths(), "/Library/Frameworks/R.framework/Versions/3.4/Resources/library"))
  summari <- as.character(reviewSummary)
  class(summari)
  corpus = Corpus(VectorSource(summari))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  frequencies = DocumentTermMatrix(corpus)
  wordFrequencies = as.data.frame(as.matrix(frequencies))
  
  words <- colnames(wordFrequencies)
  frequency <- colSums(wordFrequencies)
  wordcloud(words, frequency,
            colors=brewer.pal(8, "Dark2"),
            random.color=TRUE)  
}
reviews_word_cloud(p1$Text)






