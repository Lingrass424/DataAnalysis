library(Matrix)
library(stringr)
library(SnowballC)
library(reshape2)
library(phrasemachine)
library(igraph)
library(ggraph)
library(networkD3)

library(sotu)
library(syuzhet)
library(tidytext)
library(tm)
library(stringr)
library(topicmodels)
library(tidyverse)
library(dplyr)
library(data.table)
library(textdata)
library(ggplot2)
library(tokenizers)
library(lubridate)
library(textnets)
library(Matrix)
library(stringr)
library(SnowballC)
library(reshape2)
library(phrasemachine)
library(igraph)
library(ggraph)
library(networkD3)
library(wordcloud)
library(RColorBrewer)
glimpse(sotu_meta)
class(sotu_meta)
dim(sotu_meta)
str(sotu_meta)
summary(sotu_meta)
head(sotu_meta)
tail(sotu_meta)

unique(sotu_meta$president)

glimpse(sotu_text)
class(sotu_text)
dim(sotu_text)
str(sotu_text)
summary(sotu_text)
head(sotu_text)
tail(sotu_text)

#merge meta data with text
sotu <- data.frame(
  cbind(sotu_meta, sotu_text), stringsAsFactors=FALSE)
sotu$sotu_text <- as.character(sotu$sotu_text)

glimpse(sotu)
#This is my data table that includes text and is filtered to include FDR and future
#it is not clean yet

str(sotu)
glimpse(sotu)

#doc IDs can bite me
names(sotu)[names(sotu)=="year"] <- "doc_id"
sotu$doc_id <- as.character(sotu$doc_id)
names(sotu)[names(sotu)=="sotu_text"] <- "text"
colnames(sotu)

#This breaks up my unclean data sotu_corpus will need to be rejoined with sotu_meta_filtered
sotu_source <- VectorSource(sotu)
sotu_corpus <- VCorpus(sotu_source)
head(sotu_source)
print(sotu_corpus)
print(sotu_corpus[[4]])
print(sotu_corpus[[2]][1])

#
sotu_cleaned <- tm_map(sotu_corpus,FUN=tm_reduce,
                       tmFuns=list(content_transformer(tolower),removePunctuation,removeNumbers))
mystops = c(stopwords("en"),"mr","ms","madam","will", "laughter", "speaker", "vice", "president")
sotu_cleaned1 <- tm_map(sotu_cleaned, removeWords, mystops)

sotu_clean <- tm_map(sotu_cleaned1, stripWhitespace)
glimpse(sotu_clean)

#Something with Sparcity Need this for stuff
sotu_dtm <- DocumentTermMatrix(sotu_clean)
sotu_dtm

glimpse(sotu_dtm)

unique_indexes <- unique(sotu_dtm$i)
sotu_dtm <- sotu_dtm[unique_indexes,]
sotu_dtm

sotu_dtm_tidy <- tidy(sotu_dtm)
sotu_dtm_tidy

#LDA analysis
k <- 10
sotu_lda <- LDA(sotu_dtm, k = k, control = list(seed=1234))
sotu_lda
sotu_lda_words <- terms(sotu_lda,5)
sotu_lda_words

sotu_lda_topics <-as.matrix(sotu_lda_words)
head(sotu_lda_topics)
write.csv(sotu_lda_topics,file=paste("School/Data Analysis/Final Project/sotu_lda_topics",k,".csv"))

sotu_lda_tidy <- tidy(sotu_lda)
sotu_lda_tidy

top_terms <- sotu_lda_tidy %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

wordcloud(sotu_dtm_tidy, scale=c(5,0.5), max.words=1, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8,"Set3"))

#TOPIC GRAPH
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend=FALSE) + 
  facet_wrap(~ topic, scales = "free")+
  coord_flip() 

#How much each topic applies to each doc

sotu_lda_document_topics <- tidy(sotu_lda, matrix="gamma")
sotu_lda_document_topics

write.csv(sotu_lda_document_topics,file=paste("School/Data Analysis/Final Project/sotu_LDA_document_topics_",k,".csv"))
head(sotu_lda_document_topics)

head(sotu_lda_document_topics)
dim(sotu_lda_document_topics)

sotu_lda_document <- spread(sotu_lda_document_topics, topic, gamma)
head(sotu_lda_document)
sotu_lda_document$max_topic <- colnames(sotu_lda_document[2:7])[apply(sotu_lda_document,1,which.max)]

colnames(sotu_lda_document)


dt1 <- data.table(sotu_clean, key = "document")
dt2 <- data.table(sotu_clean, key = "doc_id")
glimpse(sotu_clean)
glimpse()
sotu_merged <- dt1[dt2]
dim(sotu_merged)
colnames(sotu_merged)

head(sotu_merged)

#Clean sotu text

  #punctuation
  #lowercase
  #take out stop words
  #annotations
  #spaces

#group by pres
#word cloud test
#topic modeling
#positive negative
#avg sentiment
