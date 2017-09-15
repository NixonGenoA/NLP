library("wordcloud")
library("tm")
library("ggplot2")
#data
filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- readLines(filePath)
#Corpus
docs<-Corpus(VectorSource(text))
inspect(docs)

#remove punc.
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

#TDM
dtm <- TermDocumentMatrix(docs)
dmat <- as.matrix(dtm)
dfreq <- sort(rowSums(dmat),decreasing=TRUE)
fm <- data.frame(word = names(dfreq),freq=dfreq)
head(fm, 10)


wordcloud(words = fm$word, freq = fm$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

ggplot(fm)+
  geom_bar(aes(word,freq),stat = "identity",color="black",fill="red",position="dodge")+
  theme(axis.text.x =element_text(angle = 90,hjust = 1,size = 10, vjust=0.5,margin=margin(10,10,10,10)) )+
  ggtitle("FREQUENCY PLOT")+labs(x="Word",y="Frequency")


barplot(fm[1:10,]$freq, las = 2, names.arg = fm[1:10,]$word,
        col ="Red", main ="Most frequent words",
        ylab = "Word frequencies")


#Save WordCloud
png("Cloud.png", width=12, height=8, units="in", res=300)

wordcloud(words = fm$word, freq = fm$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()