install.packages("XML")
library(XML)
library(XML)
speechtext <- function(ymd){
  sotu <- data.frame(matrix(nrow=1,ncol=3))
  colnames(sotu) = c("speechtext","year","date")
  for(i in 1:length(ymd)){
    year <- substr(ymd[i],1,4)
    url <- paste0('http://stateoftheunion.onetwothree.net/texts/',ymd[i],'.html')
    doc.html = htmlTreeParse(url, useInternal = TRUE)
    
    doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
    
    # Replace all newline notation with spaces and all quotes with nothing
    doc.text = gsub('\\n', ' ', doc.text)
    doc.text = gsub('\\"', '', doc.text)
    
    doc.text = paste(doc.text, collapse = ' ')
    
    x <- data.frame(doc.text, year, ymd[i], stringsAsFactors = FALSE)
    names(x) <- c("speechtext","year","date")
    sotu <- rbind(sotu, x)
    sotu <- sotu[!is.na(sotu$speechtext), ]
  }
  return(sotu)
}

sotu <- speechtext(c("20080128","20160112"))

library(tm)
library(dplyr)
library(xtable)

docs <- Corpus(VectorSource(sotu$speechtext)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(tolower)  %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(PlainTextDocument)

tdm <- TermDocumentMatrix(docs) %>%
  as.matrix()
colnames(tdm) <- c("Bush","Obama")

print(xtable(head(tdm)), type="html")

library(wordcloud)
#Select Bush speech frequencies and sort in descending order
bushsotu <- as.matrix(tdm[,1])
bushsotu <- as.matrix(bushsotu[order(bushsotu, decreasing=TRUE),])

print(xtable(head(bushsotu)), type="html")

#Select Obama speech frequencies and sort in descending order
obamasotu <- as.matrix(tdm[,2])
obamasotu <- as.matrix(obamasotu[order(obamasotu, decreasing=TRUE),])

print(xtable(head(obamasotu)), type="html")
             
#Create Bush and Obama word clouds and plot them side-by-side
#Create two panels to add the word clouds to
par(mfrow=c(1,2))
#Create word cloud of Bush speech
wordcloud(rownames(bushsotu), bushsotu, min.freq =3, scale=c(5, .2), random.order = FALSE, random.color = FALSE, colors= c("indianred1","indianred2","indianred3","indianred"))
#Create word cloud of Obama speech
wordcloud(rownames(obamasotu), obamasotu, min.freq =3, scale=c(5, .2), random.order = FALSE, random.color = FALSE, colors= c("lightsteelblue1","lightsteelblue2","lightsteelblue3","lightsteelblue"))

par(mfrow=c(1,1))
comparison.cloud(tdm, random.order=FALSE, colors = c("indianred3","lightsteelblue3"),
                 title.size=2.5, max.words=200)

library(RColorBrewer)
commonality.cloud(tdm, random.order=FALSE, scale=c(5, .5),colors = brewer.pal(4, "Dark2"), max.words=400)        




library(tokenizers)
library(tidyverse)
library(tm)
library(dplyr)
library(xtable)

docs <- Corpus(VectorSource(sotu$)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(tolower)  %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(PlainTextDocument)

tdm <- TermDocumentMatrix(docs) %>%
  as.matrix()
colnames(tdm) <- c("Bush","Obama")

print(xtable(head(tdm)), type="html")






input_loc <- "/Users/stevejobs/Desktop/sotu_text"
files <- dir(input_loc, full.names = TRUE)
text <- c()
for (f in files) {
  text <- c(text, paste(readLines(f), collapse = "\n"))
}

words <- tokenize_words(text)
length(words[[1]])

tab <- table(words[[1]])
tab <- data_frame(word = names(tab), count = as.numeric(tab))
tab <- arrange(tab, desc(count))
tab

words <- tokenize_words(text)
sapply(words, length)
  qplot(sotu_meta$president, sapply(words, length))
