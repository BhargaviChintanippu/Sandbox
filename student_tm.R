fileName <- '//cifsp02/UD101P01/Home/HKCPU/DATA/My Documents/Data Science/Projects/Student Text Mining/student1_txt.txt'
txt <- readChar(fileName, file.info(fileName)$size)

# number of characters
nchar(txt)



clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "mug", "coffee"))
  return(corpus)
}

# Cleaning functions
UmlautCleaner <- function(text_vector){
  text_vector <- gsub("[ä]", "ae", text_vector)
  text_vector <- gsub("[ö]", "oe", text_vector)
  text_vector <- gsub("[ü]", "ue", text_vector)
  text_vector <- gsub("[è]", "e", text_vector)
  text_vector <- gsub("[é]", "e", text_vector)
  text_vector <- gsub("[à]", "a", text_vector)
  return(text_vector)
}

TextPreprocessing <- function(text_vector){
  # Remove Punctuation and Remove Numbers
  text_vector <- gsub("[^A-Za-z]", " ", text_vector)
  text_vector <- gsub(" *\\b[[:alpha:]]{1,3}\\b *", " ", text_vector)
  return(text_vector)
}

CorpusCleaner <- function(corpus){
  corpus_tmp <- tm_map(corpus, content_transformer(tolower))
  corpus_tmp <- tm_map(corpus_tmp, removeWords, stopwords(kind = "de"))
  corpus_tmp <- tm_map(corpus_tmp, removeWords, c(stopwords("de"), "https", "intranet", "sitepages", "aspx", "portal","fuer", "dass", "wurde", "dachbegruenung", "abbildung", "abbildungsverzeichnis"))
  corpus_tmp <- tm_map(corpus_tmp, removeWords, c("schritte", "erwartetes", "ergebnis", "tatsaechliches"))
  corpus_tmp <- tm_map(corpus_tmp, stripWhitespace)
  return(corpus_tmp)
}

URLCleaner <- function(URL){
  URL <- gsub("https://intranet.hel.kko.ch/", "", URL)
  URL <- gsub("\\=.*", "", URL, perl = TRUE)
  URL <- gsub("\\#.*", "", URL, perl = TRUE)
  URL <- gsub(" ", "", URL)
  return(URL)
}


# Cleaning the text

txt_clean <- UmlautCleaner(txt) 
txt_clean <- TextPreprocessing(txt_clean)





############################

# Let's start with Text Mining

library("tm")
library("ggplot2")

txt_source <- VectorSource(txt_clean)
txt_corpus <- VCorpus(txt_source)
txt_corpus_clean <- CorpusCleaner(txt_corpus)



# Let's start with a tdm
txt_tdm <- TermDocumentMatrix(txt_corpus_clean)
txt_m <- as.matrix(txt_tdm)


term_frequency <- rowSums(txt_m)
term_frequency <- sort(term_frequency, decreasing = TRUE)
term_frequency[1:10]

#### Some plots

library(dplyr)

tf <- as.data.frame(term_frequency)
tf$words <- row.names(tf)
tf10 <- as.data.frame(tf[1:10,])
tf10 <- mutate(tf10, words = factor(words, words))

                 
 ggplot(tf10, aes(x = tf10$words , y = tf10$term_frequency   )) + geom_bar(     stat = "identity", fill = "tan", col = "black")+ theme_grey()+theme(text = element_text(size=16),  axis.title.x=element_blank(),axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ylab("Words Frequency")

 
 ## Wordclouds
 
 library(wordcloud)
 
 term_frequency[1:10]
 word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
 wordcloud(word_freqs$term, word_freqs$num, max.words = 100, colors = c("grey80", "darkgoldenrod1", "tomato"))
 
 library(qdap)
 word_associate(txt_clean, match.string = c("niederschlag"), 
                stopwords = c(stopwords("de"), "https", "intranet", "sitepages", "aspx", "portal","fuer", "dass", "wurde", "dachbegruenung", "abbildung", "abbildungsverzeichnis"), 
                network.plot = TRUE)
 
 
# Word associations
txt_tdm1 <- removeSparseTerms(txt_tdm, sparse = 0.9999)

txt_tdm1 <- txt_tdm[1:20,]
dim(txt_tdm1)
 
txt_tdm1_m <- as.matrix(txt_tdm1)
txt_tdm1_df <- as.data.frame(txt_tdm1_m)  
txt_dist <- dist(txt_tdm1_df)
 

tst <- dist(term_frequency[1:20])
txt_hc <- hclust(tst)
plot(txt_hc)
