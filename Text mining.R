library('tm')
library('pdftools')
library('wordcloud')

#######pdf location########
file_location="C:/Users/Adhvaidh/Desktop/DSP 19/wikipedia-articles.pdf"
#######
txt=pdf_text(file_location)
########   TO view page 2  #########
cat(txt[4])
##########  creating a corpus  ######
txt_corpus<-Corpus(VectorSource(txt))
    #######  cleaning the corpus file  ########
txt_corpus<-tm_map(txt_corpus,tolower)
txt_corpus<-tm_map(txt_corpus,removePunctuation)
txt_corpus<-tm_map(txt_corpus,stripWhitespace)
########  Remove words like : I, me myself, etc.. #########
head(stopwords("en"))
txt_corpus=tm_map(txt_corpus, removeWords, stopwords("en"))

######  View content of corpus  ##########
txt_corpus$content
######  Create document term matrix  ###########
dtm=DocumentTermMatrix(txt_corpus)
dtm=as.matrix(dtm)
dtm=t(dtm)
View(dtm)
#######  No. of occurence  ######
number_occurances=rowSums(dtm)
number_occurances=sort(number_occurances, decreasing = T)
head(number_occurances)
#############  Creating a word cloud  ######
wordcloud(head(names(number_occurances),100),head(number_occurances,100),scale=c(1.5,1))
