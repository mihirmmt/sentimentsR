library(ggplot2)
library(tm)
library(worldcloud)
library(syuzhet)
#install.packages("wordcloud")
#install.packages("SnowballC")
getwd()
setwd("Path of your directory")
texts=readLines("Your file name.txt")
print(texts)
docs=Corpus(VectorSource(texts))
docs
trans=content_transformer(function(x,pattern)gsub(pattern,"",x))

docs=tm_map(docs,trans,"/")
docs=tm_map(docs,trans,"@")
docs=tm_map(docs,trans,"\\|")
docs=tm_map(docs,content_transformer(tolower))
docs=tm_map(docs,removeNumbers)
docs=tm_map(docs,removeWords,stopwords("english"))
docs=tm_map(docs,removePunctuation)
docs=tm_map(docs,stripWhitespace)
docs=tm_map(docs,stemDocument)
library(SnowballC)
dtm=TermDocumentMatrix(docs)
mat=as.matrix(dtm)
mat

#sort data alphabeticaly and count
v=sort(rowSums(mat),decreasing = TRUE)
print(v)

#take words and their count
d=data.frame(word=names(v),freq=v)
head(d)

library(wordcloud)
set.seed(1056)
wordcloud(words=d$word,freq=d$freq,min.freq=1,
          max.words=200,random.order=FALSE,rot.per=0.35,
          colors=brewer.pal(8,"Dark2"))

#sentiment analysis library=>syzhet
?get_nrc_sentiment #to get sentiment values
texts
sentiment=get_nrc_sentiment(texts)
text=cbind(texts,sentiment)
print(text)
TotalSentiment=data.frame(colSums(text[,c(2:11)]))

names(TotalSentiment)="count"
TotalSentiment=cbind("sentiment"=rownames(TotalSentiment),
                     TotalSentiment)

print(TotalSentiment)

names(TotalSentiment)

library(ggplot2)
ggplot(data=TotalSentiment,aes(x=sentiment,y=count))+
  geom_bar(aes(fill=sentiment),stat="identity")+
  theme(legend.position = "none")+xlab("sentiment")+
  ylab("total count")+
  ggtitle("Total sentiment score")
