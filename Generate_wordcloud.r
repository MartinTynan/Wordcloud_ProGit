# This code creates a wordcloud from a text file. 
# This is for use in a presentation - the text file is the book 'Progit'
# See https://git-scm.com/book/en/v2
# Adapted from http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

#####Install and load packages#####
# install.packages("tm")  # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
# Load packages
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#####Data Loading and Wrangling#####
# Loading the text from the file
text <- readLines("Progit_txt.txt")
# Load the data as a corpus
docs <- Corpus(VectorSource(text))

inspect(docs)


# Mapping special characters to spaces
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/") # function for transformations on a corpus
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector# removing non-relevant words that were seen in the top 200
words_to_remove = c("can", "will", "use", "want", "like", "see", "one", "now", "first"
                    , "using", "just")
docs <- tm_map(docs, removeWords, words_to_remove) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)


# Builds the term-document matrix: a table containing words & frequencies
dtm <- TermDocumentMatrix(docs) # from text mining package
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)



#####Generating wordcloud#####
set.seed(1234)

# Deciding on colour pallete
pal = brewer.pal(9,"Blues")
display.brewer.pal(9,"Blues")
# Light blue is too light. Removing the first two levels
pal = pal[3:9] # just a list of hex-colour codes

# Generating wordcloud
# Saved output as 'Progit_wordcloud.png'
wordcloud(words = d$word
          , freq = d$freq
          , min.freq = 1
          , max.words=200
          , random.order=FALSE
          , rot.per=0.35
          , colors=pal)

# Saved output as 'Progit_wordcloud_fewerwords.png'
wordcloud(words = d$word
          , freq = d$freq
          , min.freq = 1
          , max.words=100
          , random.order=FALSE
          , rot.per=0.35
          , colors=pal)


