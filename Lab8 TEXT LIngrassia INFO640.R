install.packages("gutenbergr")
install.packages("tm")
install.packages("topicmodels")
install.packages("data.table")
install.packages("tidytext")
install.packages("textdata")


library(tidytext)
library(tm)
library(stringr)
library(topicmodels)
library(gutenbergr)
library(tidyverse)
library(dplyr)
library(data.table)
library(textdata)

#Emily D

text <- ("Because I could not stop for Death - 
  He kindly stopped for me -
  The Carriage held but just Ourselves - 
  and Immortality")

text

tolower(text)
removePunctuation(text)
stripWhitespace(text)
text

stopwords("en")
removeWords(text, stopwords("en"))

#Exercise1
text1 <- tolower(text)
text2 <- removePunctuation(text1)
text_new <- stripWhitespace(text2)
print(text_new)

removeWords(text_new, stopwords("en"))

my_stops <- c(stopwords("en"), "death")

nvec <- unlist(strsplit(new_text, split=" "))
stem_text <- stemDocument(nvec)

nvec <- unlist(strsplit(new_text, split=" "))
stem_text <- stemDocument(nvec)

print(stem_text)
completed_text <- stemCompletion(stem_text,nvec)
completed_text

#Structured Text
peace_res <- read.csv("School/Data Analysis/Labs/pax_20_02_2018_1_CSV.csv", encoding = "utf-8", 
                      header = TRUE, stringsAsFactors = FALSE)
str(peace_res)
glimpse(peace_res)

#renames collumns
names(peace_res)[names(peace_res)=="AgtId"]<-"doc_id"
peace_res$doc_id <-as.character(peace_res$doc_id)
names(peace_res)[names(peace_res)=="OthAgr"]<-"text"
colnames(peace_res)

peace_source <- DataframeSource(peace_res)
peace_corpus <- VCorpus(peace_source)

peace_corpus

peace_corpus[[10]]
peace_corpus[[10]][1]
peace_corpus[[10]][2]

peace_cleaned <- tm_map(peace_corpus, removeNumbers)
peace_cleaned[[10]][1]

#Practice Cleaning

#1 Make lowercase
peace_cleaned <- tm_map(peace_cleaned, content_transformer(tolower))

#2 Add "peace" and "agreement" to the stopwords list
swl = c(stopwords("en"), "peace", "agreement", "shall", "government", "page", "parties")

#3 Remove your stopwords from your corpus
peace_cleaned <- tm_map(peace_cleaned, removeWords, swl)

#4 Remove Punctuation
peace_cleaned <- tm_map(peace_cleaned, removePunctuation)

#5 Strip Whitespace
peace_cleaned <- tm_map(peace_cleaned, stripWhitespace)

#6 Inspect an entry
peace_cleaned[[10]][1]

#TOPIC MODELING use for project
peace_dtm <- DocumentTermMatrix(peace_cleaned)
peace_dtm

unique_indexes <- unique(peace_dtm$i)
peace_dtm <- peace_dtm[unique_indexes,]
peace_dtm

peace_dtm_tidy <- tidy(peace_dtm)
peace_dtm_tidy

#LDA
k <- 6
peace_lda <- LDA(peace_dtm, k = k, control = list(seed=1234))
peace_lda
peace_lda_words <- terms(peace_lda,5)

peace_lda_topics <-as.matrix(peace_lda_words)

#saves csv matrix to computer
write.csv(peace_lda_topics,file=paste("School/Data Analysis/Labs/peace_LDA",k,".csv"))

head(peace_lda_topics)

peace_lda_tidy <- tidy(peace_lda)
peace_lda_tidy

top_terms <- peace_lda_tidy %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
head(top_terms)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~ topic, scales = "free")+
  coord_flip()

get_LDA_topics_terms_by_topic <- function(input_corpus, plot = TRUE, number_of_topics = 6, number_of_words=5, path="School/Data Analysis/Labs/peace_LDA_doc")
{
  my_dtm <- DocumentTermMatrix(input_corpus)
  unique_indexes <- unique(my_dtm$i)
  my_dtm <- my_dtm[unique_indexes,]
  my_lda <- LDA(my_dtm, k = number_of_topics, control = list(seed=1234))
  my_topics <- tidy(my_lda, matrix="beta")
  my_lda_words <- terms(my_lda, number_of_words)
  my_lda_topics <- as.matrix(my_lda_words)
  write.csv(my_lda_topics,file=paste(path,k,".csv"))
  my_top_terms <- my_topics %>%
    group_by(topic) %>%
    top_n(number_of_words, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  if(plot==TRUE){
    my_top_terms %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta, fill=factor(topic))) +
      geom_col(show.legend=FALSE) +
      facet_wrap(~ topic, scales = "free")+
      coord_flip()
  }else{
    return(my_top_terms)
  }
}

get_LDA_topics_terms_by_topic(peace_cleaned, number_of_topics = 4, number_of_words = 4)

peace_lda_document_topics <- tidy(peace_lda, matrix="gamma")
peace_lda_document_topics

write.csv(peace_lda_document_topics,file=paste("School/Data Analysis/Labs/peace_LDA_document_topics",k,".csv"))

head(peace_lda_document_topics)
dim(peace_lda_document_topics)

peace_lda_document <- spread(peace_lda_document_topics, topic, gamma)
dim(peace_lda_document)
head(peace_lda_document)

peace_lda_document$max_topic <- colnames(peace_lda_document[2:7])[apply(peace_lda_document,1,which.max)]
head(peace_lda_document)

dt1 <- data.table(peace_lda_document, key = "document")
dt2 <- data.table(peace_res, key = "doc_id")
peace_merged <- dt1[dt2]
dim(peace_merged)
colnames(peace_merged)

peace_analyze <- select(peace_merged, c(Con, Contp, Reg, Dat, Status, Lgt, Agtp, Stage, StageSub, Part, ThrdPart, text, 1, 2, 3, 4, 5, 6, max_topic))
head(peace_analyze)

#UNSTRUCTURED TEXT

dq <- gutenberg_download(996)
head(dq)


#SENTIMENT ANALYSIS

sentiments
get_sentiments("afinn")
get_sentiments("nrc")
get_sentiments("bing")

lyrics_raw <- read.csv("School/Data Analysis/Labs/songdata.csv",encoding = "utf-8", header = TRUE, stringsAsFactors = FALSE)
summary(lyrics_raw)

tidy_lyrics <- lyrics_raw %>%
  ungroup() %>%
  unnest_tokens(word, text)
summary(tidy_lyrics)
head(tidy_lyrics)

nrc_sent <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

tidy_lyrics_bowie <- tidy_lyrics %>%
  filter(artist == "David Bowie")

tidy_lyrics_bowie %>%
  inner_join(nrc_sent) %>%
  dplyr::count(word, sort = TRUE)

bowie_sentiment <- tidy_lyrics_bowie %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(song, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

head(bowie_sentiment)

ggplot(bowie_sentiment, aes(negative, positive, color = song)) +
  geom_jitter(show.legend = FALSE)

bowie_career_sentiment <- mean(bowie_sentiment$sentiment)
bowie_career_sentiment

#Exercise
unique(lyrics_raw$artist)

tidy_lyrics_zevon <- tidy_lyrics %>%
  filter(artist == "Warren Zevon")

tidy_lyrics_zevon %>%
  inner_join(nrc_sent) %>%
  dplyr::count(word, sort = TRUE)

zevon_sentiment <- tidy_lyrics_zevon %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(song, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

head(zevon_sentiment)

ggplot(zevon_sentiment, aes(negative, positive, color = song)) +
  geom_jitter(show.legend = FALSE)

zevon_career_sentiment <- mean(zevon_sentiment$sentiment)
zevon_career_sentiment

tidy_lyrics_tool <- tidy_lyrics %>%
  filter(artist == "Tool")

tidy_lyrics_tool %>%
  inner_join(nrc_sent) %>%
  dplyr::count(word, sort = TRUE)

tool_sentiment <- tidy_lyrics_tool %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(song, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

head(tool_sentiment)

ggplot(tool_sentiment, aes(negative, positive, color = song)) +
  geom_jitter(show.legend = FALSE)

tool_career_sentiment <- mean(tool_sentiment$sentiment)
tool_career_sentiment