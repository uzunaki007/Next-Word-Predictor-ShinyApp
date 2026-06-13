install.packages(c(
"tm",
"RWeka",
"stringi",
"dplyr",
"ggplot2",
"wordcloud",
"shiny",
"RColorBrewer"
))
Sys.which("make")
install.packages(c(
"tm",
"RWeka",
"stringi",
"dplyr",
"ggplot2",
"wordcloud",
"shiny",
"RColorBrewer"
))
Sys.which("make")
install.packages(c(
"tm",
"RWeka",
"stringi",
"dplyr",
"ggplot2",
"wordcloud",
"shiny",
"RColorBrewer"
))
library(stringi)
blogs <- readLines(
"en_US.blogs.txt",
encoding="UTF-8",
skipNul=TRUE
)
setwd("C:/Users/Sitanshu Chakraborty/Downloads")
install.packages(c(
"tm",
"RWeka",
"stringi",
"dplyr",
"ggplot2",
"wordcloud",
"shiny",
"RColorBrewer"
))
#.........................
# creating a report
library(stringi)
blogs <- readLines(
"en_US.blogs.txt",
encoding="UTF-8",
skipNul=TRUE
)
library(stringi)
blogs <- readLines(
"en_US.blogs.txt",
encoding="UTF-8",
skipNul=TRUE
)
news <- readLines(
"en_US.news.txt",
encoding="UTF-8",
skipNul=TRUE
)
twitter <- readLines(
"en_US.twitter.txt",
encoding="UTF-8",
skipNul=TRUE
)
data.frame(
File=c("Blogs","News","Twitter"),
Lines=c(length(blogs),
length(news),
length(twitter)),
Words=c(
sum(stri_count_words(blogs)),
sum(stri_count_words(news)),
sum(stri_count_words(twitter))
)
)
#..............................
# creating sample data
set.seed(123)
sampleBlogs <- sample(
blogs,
length(blogs)*0.01
)
sampleNews <- sample(
news,
length(news)*0.01
)
sampleTwitter <- sample(
twitter,
length(twitter)*0.01
)
sampleData <- c(
sampleBlogs,
sampleNews,
sampleTwitter
)
writeLines(
sampleData,
"sample.txt"
)
#...................
#data cleaning
library(tm)
corpus <- Corpus(
VectorSource(sampleData)
)
corpus <- tm_map(
corpus,
content_transformer(tolower)
)
corpus <- tm_map(
corpus,
removePunctuation
)
corpus <- tm_map(
corpus,
removeNumbers
)
corpus <- tm_map(
corpus,
stripWhitespace
)
#..................
# the frequent words
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
install.packages("slam")
tdm <- TermDocumentMatrix(corpus)
library(slam)
tdm <- TermDocumentMatrix(corpus)
# Installing the package
install.packages("tm")
#.............................
#finding the most frequent words
library(tm)
library(tm)
library(slam) # For the memory fix from earlier!
# 1. Create your TDM
tdm <- TermDocumentMatrix(corpus)
# 2. Sum the rows efficiently
word_counts <- slam::row_sums(tdm)
# 3. Sort and view the top 20
freq <- sort(word_counts, decreasing = TRUE)
head(freq, 20)
#..............................
#creating word frequency graph
library(ggplot2)
df <- data.frame(
word=names(freq)[1:20],
freq=freq[1:20]
)
ggplot(df,
aes(reorder(word,freq),
freq)) +
geom_bar(stat="identity") +
coord_flip()
#--------------------------------------
# building bigrams
library(tm)
library(RWeka)
library(slam) # Loads the memory-saving package

# 1. Create the bigram tokenizer
BigramTokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

# 2. Build the bigram matrix
bigram <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))

# 3. Sum the rows efficiently without converting to a dense matrix
bigram_counts <- slam::row_sums(bigram)

# 4. Sort and view
bigramFreq <- sort(bigram_counts, decreasing = TRUE)
head(bigramFreq, 20)
#............................................
#building trigrams
library(tm)
library(RWeka)
library(slam) # The memory-saving package we need!

# 1. Create the Trigram Tokenizer
TrigramTokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 3, max = 3))
}

# 2. Build the trigram matrix (this stays efficient)
trigram <- TermDocumentMatrix(
  corpus,
  control = list(tokenize = TrigramTokenizer)
)

# 3. FIX: Sum rows directly using slam without using as.matrix()
trigram_counts <- slam::row_sums(trigram)

# 4. Sort and view the top 20
trigramFreq <- sort(trigram_counts, decreasing = TRUE)
head(trigramFreq, 20)
#......................................
#creating prediction function
predictWord <- function(text) {
  # 1. Clean up input text and grab the last word
  words <- strsplit(text, " ")[[1]]
  lastWord <- tail(words, 1)
  
  # 2. Get all names from your bigram frequencies
  bigram_names <- names(bigramFreq)
  
  # 3. Match the bigrams that start with your last word
  match_pattern <- paste0("^", lastWord, " ")
  candidates <- bigram_names[grepl(match_pattern, bigram_names)]
  
  # 4. If no match is found, fallback to the most common word
  if(length(candidates) == 0) {
    return("the")
  }
  
  # 5. Extract the predicted second word
  prediction <- strsplit(candidates[1], " ")[[1]][2]
  
  return(prediction)
}
#............................................