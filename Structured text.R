
library(tidyverse)
library(tm)
library(tidytext)
library(stringr)

library(topicmodels)
library(data.table)

library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(sotu)

head(sotu_meta)
head(sotu_text)

#add unique id


names(sotu_df)[names(sotu_df)=="ID"] <-"doc_id"
names(sotu_df)[names(sotu_df)=="sotu_text"] <-"text"
colnames(sotu_df)
colnames(sotu_source)
#Corpus
sotu_source <- DataframeSource(sotu_df)
sotu_corpus <- VCorpus(sotu_source)

sotu_corpus[[10]]
sotu_corpus[[10]][1]

sotu_cleaned <- tm_map(sotu_corpus,FUN=tm_reduce,
                       tmFuns=list(content_transformer(tolower),
                                   removePunctuation,removeNumbers))
mystops = c(stopwords("en"),"mr","ms","madam","will","shall", "should", "laughter", "speaker", "vice", "president")
sotu_cleaned1 <- tm_map(sotu_cleaned, removeWords, mystops)
head(sotu_cleaned)
sotu_clean <- tm_map(sotu_cleaned1, stripWhitespace)
sotu_clean[[10]][1]
head(sotu_clean)

#Something with Sparcity Need this for stuff
sotu_dtm <- DocumentTermMatrix(sotu_clean)
sotu_dtm

unique_indexes <- unique(sotu_dtm$i)
sotu_dtm <- sotu_dtm[unique_indexes,]
sotu_dtm

sotu_dtm_tidy <- tidy(sotu_dtm)
sotu_dtm_tidy
tail(sotu_dtm_tidy)

#LDA analysis
k <- 6
sotu_lda <- LDA(sotu_dtm, k=k, control = list(seed=1234))
sotu_lda
sotu_lda_words <- terms(sotu_lda,5)
sotu_lda_words

sotu_lda_topics <-as.matrix(sotu_lda_words)
head(sotu_lda_topics)
write.csv(sotu_lda_topics,file=paste
          ("School/Data Analysis/Final Project/sotu_lda_topics",k,".csv"))

sotu_lda_tidy <- tidy(sotu_lda)
head(sotu_lda_tidy)

top_terms <- sotu_lda_tidy %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms


#TOPIC GRAPH
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend=FALSE) + 
  facet_wrap(~ topic, scales = "free")+
  coord_flip() 

#DO I NEED THIS FUNCTION? get_LDA_topics_terms_by_topic(sotu_clean, number_of_topics = 5, number_of_words = 4)

sotu_lda_document_topics <- tidy(sotu_lda, matrix="gamma")
sotu_lda_document_topics

write.csv(sotu_lda_document_topics,file=paste("School/Data Analysis/Final Project/sotu_LDA_document_topics_",k,".csv"))
head(sotu_lda_document_topics)

head(sotu_lda_document_topics)
dim(sotu_lda_document_topics)

sotu_lda_document <- spread(sotu_lda_document_topics, topic, gamma)
dim(sotu_lda_document)
head(sotu_lda_document)

sotu_lda_document$max_topic <- colnames(sotu_lda_document[2:6])[apply(sotu_lda_document,1,which.max)]
head(sotu_lda_document)

#Lauren - remember to use sotu_df to join dt2 to sentiment 
dt1 <- data.table(sotu_lda_document, key="document")
dt2 <- data.table(sotu_df, key="doc_id")

sotu_merged <- dt1[dt2]
head(sotu_merged)
glimpse(sotu_merged)

#trying to figure out why does this xxxx does not work
sotu_meta <- mutate(sotu_meta, ID = row_number())

sotu_meta$ID <- as.character(sotu_meta$ID)
head(sotu_meta)

sotu_meta_df <- (sotu_meta)
head(sotu_meta_df)
sotu_meta_df$year <- as.character(sotu_meta$year)
head(sotu_meta_df)

summary(sotu_text)  
head(sotu_text)


#merge meta data with text
sotu_df <- data.frame(
  cbind(sotu_meta_df, sotu_text), stringsAsFactors=FALSE)

sotu_df$text <- as.character(sotu_df$text)
summary(sotu_df)
head(sotu_df)

glimpse(sotu_df)

names(sotu_df)[names(sotu_df)=="ID"] <-"doc_id"
names(sotu_df)[names(sotu_df)=="sotu_text"] <-"text"

#now do sentiment analysis
sentiments
get_sentiments("afinn")
get_sentiments("nrc")
get_sentiments("bing")

glimpse(sotu_df)
summary(sotu_df)
colnames(sotu_df)

tidy_sotu <- sotu_df %>%
  ungroup() %>%
  unnest_tokens(word,text)

summary(tidy_sotu)
head(tidy_sotu)

tidy_sotu_app_pres <- tidy_sotu %>%
  filter(year>"1932")

head(tidy_sotu_app_pres)

#omfg it finally worked

tidy_sotu_app_pres %>% inner_join(nrc_sent) %>%
  dplyr::count(word, sort = TRUE)
head(tidy_sotu_app_pres)

#This is the data I want to join on
#Sentiment by President pos neg BING
sotu_sentiment_bing <- tidy_sotu_app_pres %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(president, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
sotu_sentiment_bing

#This is the graph I want1
ggplot(sotu_sentiment_bing, 
       aes(president, sentiment, fill = president)) +
  geom_bar(stat = "identity", show.legend = FALSE)

barplot(height=sotu_sentiment_bing$sentiment, 
        names.arg = sotu_sentiment_bing$president, 
        col=coul,
        las = 2)

par(mar=c(11,4,4,4))

#This is the graph I want2
#graphing senitment
ggplot(sotu_sentiment_bing, 
       aes(negative, positive, color = president)) + 
  geom_jitter(show.legend = FALSE)

total_pres_sent <- mean(sotu_sentiment_bing$sentiment)
total_pres_sent

sotu_sentiment_afinn <- tidy_sotu_app_pres %>%
  inner_join(get_sentiments("afinn")) %>%
  dplyr::count(president, value) %>% spread(value, n, fill = 0) 
head(sotu_sentiment_afinn)

#row with pos vs neg
sotu_sentiments <- sotu_dtm_tidy %>% 
  inner_join(get_sentiments("bing"), by=c(term="word"))  
sotu_sentiments

#top terms
terms <- Terms(sotu_dtm)
head(terms)
#tidy terms
sotu_td <- tidy(sotu_dtm)
sotu_td 


sotu_sentiments1 <- sotu_td %>% inner_join(get_sentiments("bing"), by=c(term="word"))  
sotu_sentiments1

sotu_sentiments1 %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()

#specific words
sotu_tf_idf <- sotu_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))
sotu_tf_idf

dt3 <- data.table(sotu_tf_idf, key="document")
dt2 <- data.table(sotu_df, key="doc_id")

sotu_merged_sent <- dt3[dt2]
head(sotu_merged_sent)
glimpse(sotu_merged_sent)

pres_sent <- sotu_tf_idf %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

pres_sent 

write.csv(sotu_sentiment_bing,file=paste
          ("School/Data Analysis/Final Project/sotu_senitment_bing",k,".csv"))