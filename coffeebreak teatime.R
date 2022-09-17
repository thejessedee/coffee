library(twitteR)
library(rtweet)
library(readr)
library(ggplot2)
library(wordcloud)
library(tidytext)
library(tidyverse)
library(sentimentr)

library(quanteda)
library(devtools)
library(quanteda.corpora)
library(quanteda.textplots)
library(quanteda.textstats)
library(quanteda.sentiment)
library(spacyr)
# library(arrow)

#information on getting tokens
vignette("auth", package = "rtweet") 

#set up your bearer 
bearer_token <- read_file("bearer_token1.txt")

# Get tweets from search: returns tweets from the last ~7 days
# Rate limit: ~18,000 tweets every 15 minutes.

#search tweets on coffeebreak
coffee <- search_tweets("#coffee",n = 500)

coffeebreak_seattle1 <- search_tweets("#coffeebreak", n = 500,
                                      geocode = lookup_coords("seattle"),
                                      include_rts = FALSE)

coffeebreak_test <- search_tweets("Biden", n = 500,
                                  geocode = lookup_coords("seattle", "country:US"),
                                  include_rts = FALSE)

coffeebreak_usa <- search_tweets("#coffeebreak", n = 500,
                                 geocode = lookup_coords("usa"),
                                 include_rts = FALSE)

teatime_usa <- search_tweets("#teatime", n = 500,
                             geocode = lookup_coords("usa"),
                             include_rts = FALSE)

teatime_usa$nohash <- lengths(teatime_usa$hashtags)
coffeebreak_usa$nohash <- lengths(coffeebreak_usa$hashtags)

save(coffeebreak_usa, file="/users/magic/Documents/coffeebreak_usa.Rdata")

save(teatime_usa, file="/users/magic/Documents/teatime_usa.Rdata")

text_coffee <- coffeebreak_usa$text
text_tea <- teatime_usa$text

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

d<-get_nrc_sentiment(text_tea)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)
#transpose
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[1:168]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

emo_words_coffee <- extract_emotion_terms(text_coffee)
emo_words_coffee
emo_words_coffee$sentence
emo_words_coffee[, c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')]
attributes(emo_words_coffee)$counts
attributes(emo_words_coffee)$elements

(emo_coffee <- emotion(text_coffee))
emotion(text_coffee, drop.unused.emotions = TRUE)

plot(emo_coffee)
plot(emo_coffee, drop.unused.emotions = FALSE)
plot(emo_coffee, facet = FALSE)
plot(emo_coffee, facet = 'negated')

#tea emotion
emo_words_tea <- extract_emotion_terms(text_tea)
emo_words_tea
emo_words_tea$sentence
emo_words_tea[, c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')]
attributes(emo_words_tea)$counts
attributes(emo_words_tea)$elements

(emo_tea <- emotion(text_tea))
emotion(text_tea, drop.unused.emotions = TRUE)

plot(emo_tea)
plot(emo_tea, drop.unused.emotions = FALSE)
plot(emo_tea, facet = FALSE)
plot(emo_tea, facet = 'negated')

indcoffee <- subset(coffeebreak_usa, nohash<10)
buscoffee <- subset(coffeebreak_usa, nohash>9)

text_indcoffee <- indcoffee$text
text_buscoffee <- buscoffee$text

indtea <- subset(teatime_usa, nohash<10)
bustea <- subset(teatime_usa, nohash>9)

text_indtea <- indtea$text
text_bustea <- bustea$text

wordcloud::wordcloud(indcoffee$text, min.freq=3) #take a glance of the text
wordcloud::wordcloud(buscoffee$text, min.freq=3) #take a glance of the text

wordcloud::wordcloud(indtea$text, min.freq=3) #take a glance of the text
wordcloud::wordcloud(bustea$text, min.freq=3) #take a glance of the text

emo_indcoffee <- extract_emotion_terms(text_indcoffee)
emo_indcoffee
emo_indcoffee$sentence
emo_indcoffee[, c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')]
attributes(emo_indcoffee)$counts
attributes(emo_indcoffee)$elements

(emo_icoffee <- emotion(text_indcoffee))
emotion(text_indcoffee, drop.unused.emotions = TRUE)

plot(emo_icoffee)
plot(emo_icoffee, drop.unused.emotions = FALSE)
plot(emo_icoffee, facet = FALSE)
plot(emo_icoffee, facet = 'negated')



emo_buscoffee <- extract_emotion_terms(text_buscoffee)
emo_buscoffee
emo_buscoffee$sentence
emo_buscoffee[, c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')]
attributes(emo_buscoffee)$counts
attributes(emo_buscoffee)$elements

(emo_bcoffee <- emotion(text_buscoffee))
emotion(text_buscoffee, drop.unused.emotions = TRUE)

plot(emo_bcoffee)
plot(emo_bcoffee, drop.unused.emotions = FALSE)
plot(emo_bcoffee, facet = FALSE)
plot(emo_bcoffee, facet = 'negated')


emo_indtea <- extract_emotion_terms(text_indtea)
emo_indtea
emo_indtea$sentence
emo_indtea[, c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')]
attributes(emo_indtea)$counts
attributes(emo_indtea)$elements

(emo_itea <- emotion(text_indtea))
emotion(text_indtea, drop.unused.emotions = TRUE)

plot(emo_itea)
plot(emo_itea, drop.unused.emotions = FALSE)
plot(emo_itea, facet = FALSE)
plot(emo_itea, facet = 'negated')


emo_bustea <- extract_emotion_terms(text_bustea)
emo_bustea
emo_bustea$sentence
emo_bustea[, c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')]
attributes(emo_bustea)$counts
attributes(emo_bustea)$elements

(emo_btea <- emotion(text_bustea))
emotion(text_bustea, drop.unused.emotions = TRUE)

plot(emo_btea)
plot(emo_btea, drop.unused.emotions = FALSE)
plot(emo_btea, facet = FALSE)
plot(emo_btea, facet = 'negated')