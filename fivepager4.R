library(RColorBrewer)
library(wordcloud)
library(tm)
library(ROAuth)
library(RCurl)
library(tidyverse)
library(tidytext)
library(purrr)
library(dplyr)
library(rtweet)
library(twitteR)
library(SentimentAnalysis)
library(textdata)
library(ggplot2)
library(NLP)
library("tm")
library("SnowballC")
library("RColorBrewer")
library("wordcloud")




appname <- "ProjectFivePages"
consumerKey <- 'K3IyMpUH0EKpkb5SY4GwXZE81'
consumerSecret <- 'x03DQn56gLd3uhIIARs1eZXMxR7bGY0lFuSv2HbeiHQxLYnpmi'
accessToken <- '1161104689431273473-l5obFXSZCR2WV59PhX1j34SnrEosn5'
accessSecret <- 'DhtopBi0SjCruo17hYpa3cJjm0gGAwNceA0TKruRNMAJm'

twitter_token <- create_token(
  app = appname,
  consumer_key = consumerKey,
  consumer_secret = consumerSecret,
  access_token = accessToken,
  access_secret = accessSecret)


setup_twitter_oauth(consumerKey ,consumerSecret, accessToken,  accessSecret)


tweets <- searchTwitter("#feminism", n = 12, lang = "en", since = '2020-01-01')
strip_retweets(tweets)


femi_tweets <- search_tweets(q = "#feminism", n = 1800,
                                lang = "en",
                                include_rts = FALSE)
head(femi_tweets$text)

femi_tweets$stripped_text <- gsub("http.*","",  femi_tweets$text)
femi_tweets$stripped_text <- gsub("https.*","", femi_tweets$stripped_text)

femi_tweets_clean <- femi_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

femi_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(150) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")

data("stop_words")
head(stop_words)
nrow(femi_tweets_clean)
cleaned_tweet_words <- femi_tweets_clean %>%
  anti_join(stop_words)

cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

library(widyr)
femi_tweets_paired_words <- femi_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

femi_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

femi_tweets_separated_words <- femi_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

femi_tweets_filtered <- femi_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

femi_words_counts <- femi_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

library(igraph)
library(ggraph)
library()

femi_words_counts %>%
  filter(n >= 2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "darkslategray4", size = 1) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - Feminism",
       subtitle = "Text mining twitter data ",
       x = "", y = "")


femi_json <- as.data.frame(femi_tweets_clean)

library(jsonlite)

write_json(femi_tweets_clean, "femitweetsclean2.json")


mach_corpus = Corpus(VectorSource(femi_words_counts))

# create document term matrix applying some transformations
tdm = TermDocumentMatrix(mach_corpus,
                         control = list(removePunctuation = TRUE,
                                        stopwords = c("feminism", stopwords("english")),
                                        removeNumbers = TRUE, tolower = TRUE))


m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# save the image in png format
png("FeminismCloud.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()



ggplot(pivot[-1,], aes(x = hour, y = sentiment)) + geom_line(group = 1) + geom_point() + 
  theme_minimal() + labs(title = paste0('Average sentiment of tweetings mentioning "',femi_tweets_clean,'"'),
                         subtitle = paste0(pivot$hour[2],' - ',pivot$hour[nrow(pivot)],' on ', format(sentiment$created_at[1], '%d %B %Y')),
                         x = 'Date', y = 'Sentiment', caption = 'Source: Twitter API')



################################################################################
asso_count <- table(femi_words_counts)
top_20_freqs <- all_of(femi_words_counts, decreasing = TRUE)[1:2]
top_20_freqs


femi_cloud <- as.character(as.data.frame(femi_words_counts)[,1])
wordcloud(femi_cloud, femi_words_counts, 
          scale = c(3.5, 1.5), random.order = FALSE, rot.per=.25)



as.data.frame(femi_words_counts) %>%
  count(femi_words_counts, sort = TRUE) %>%
  mutate(nrow = reorder(femi_words_counts, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = femi_words_counts, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Hashtag",
       title = "Top 20 Popular Hashtags along with Covid19")


library(wordcloud)
  
femi_cloud <- as.character(as.data.frame(femi_words_counts)[,1])
wordcloud(femi_cloud, femi_words_counts, 
          scale = c(3.5, 1.5), random.order = FALSE, rot.per=.25)





femi_words_counts <- as.character(as.data.frame(top_20_freqs)[,1])
wordcloud(femi_words_counts, top_20_freqs, 
          scale=c(3.5,1.5), random.order=FALSE, rot.per=.25)




# gsub("https\\S*", "", tweets$text) 
# gsub("@\\S*", "", tweets$text) 
# gsub("amp", "", tweets$text) 
# gsub("[\r\n]", "", tweets$text)
# 
# text <- data$text
# 
# tweets_words <-  tweets %>%
#   select(text) %>%
#   unnest_tokens(word, text)words <- tweets_words %>% count(word, sort=TRUE)
# 
# 
# set.seed(1234)
# wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=200, 
#           random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))








