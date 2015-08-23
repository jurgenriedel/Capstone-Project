Sys.setenv(JAVA_HOME="C:\\Program Files (x86)\\Java\\jdk1.8.0_45\\jre")

require("tm")
require("RWeka")
require("qdapRegex")
require("stringr")
require("dplyr")
require("qdap")
require("data.table")
require("NLP")
require("openNLP")

# Loading Texts

## Read source files
fileName <- "final/en_US/en_US.twitter.txt"
con <- file(fileName,open="r")
line <- readLines(con)
close(con)

fileName <- "final/en_US/en_US.news.txt"
con <- file(fileName,open="r")
line2 <- readLines(con)
close(con)

fileName <- "final/en_US/en_US.blogs.txt"
con <- file(fileName,open="r")
line3 <- readLines(con)
close(con)

require("caret")

sample.size <- 0.05
set.seed(10302002)

# Get training and test set for twitter documents 
line.sample <- line[line.list]
line.sample <- data.table(line.sample)
line.sample <- data.frame(line.sample)
colnames(line.sample) <- c("line")
line.sample <- mutate(line.sample, linelen=nchar(as.character(line)))
line.sample.sum <- mutate(line.sample, cumsum=cumsum(linelen))
total.line.chars <- sum(unlist(lapply(line,nchar)))
samplesize <- total.line.chars*sample.size
line.sample <- filter(line.sample.sum, cumsum<=samplesize)
writeLines(line[1:nrow(line.sample)], con="sample/sample.en_US.twitter.txt", "\n")
trainIndex <- createDataPartition(y=line.sample$linelen, p=0.5, list=FALSE)
line.train <- line.sample[trainIndex,1]
line.test <- line.sample[trainIndex,]
trainIndex2 <- createDataPartition(y=line.test$linelen, p=0.5, list=FALSE)
line.dev <- line.test[trainIndex2,1]
line.test <- line.test[-trainIndex2,1]

writeLines(line.train, con="sample/train/sample.en_US.twitter.training.txt", "\n")
writeLines(line.dev, con="sample/test/sample.en_US.twitter.test.txt", "\n")
writeLines(line.test, con="sample/dev/sample.en_US.twitter.dev.txt", "\n")

# Get training and test set for news documents 
line.sample <- line2[line2.list]
line.sample <- data.table(line.sample)
line.sample <- data.frame(line.sample)
colnames(line.sample) <- c("line")
line.sample <- mutate(line.sample, linelen=nchar(as.character(line)))
line.sample.sum <- mutate(line.sample, cumsum=cumsum(linelen))
total.line.chars <- sum(unlist(lapply(line2,nchar)))
samplesize <- total.line.chars*sample.size
line.sample <- filter(line.sample.sum, cumsum<=samplesize)
writeLines(line2[1:nrow(line.sample)], con="sample/sample.en_US.news.txt", "\n")
trainIndex <- createDataPartition(y=line.sample$linelen, p=0.5, list=FALSE)
line.train <- line.sample[trainIndex,1]
line.test <- line.sample[trainIndex,]
trainIndex2 <- createDataPartition(y=line.test$linelen, p=0.5, list=FALSE)
line.dev <- line.test[trainIndex2,1]
line.test <- line.test[-trainIndex2,1]

writeLines(line.train, con="sample/train/sample.en_US.news.training.txt", "\n")
writeLines(line.dev, con="sample/test/sample.en_US.news.test.txt", "\n")
writeLines(line.test, con="sample/dev/sample.en_US.news.dev.txt", "\n")

# Get training and test set for blogs documents 
line.sample <- line3[line3.list]
line.sample <- data.table(line.sample)
line.sample <- data.frame(line.sample)
colnames(line.sample) <- c("line")
line.sample <- mutate(line.sample, linelen=nchar(as.character(line)))
line.sample.sum <- mutate(line.sample, cumsum=cumsum(linelen))
total.line.chars <- sum(unlist(lapply(line3,nchar)))
samplesize <- total.line.chars*sample.size
line.sample <- filter(line.sample.sum, cumsum<=samplesize)
writeLines(line3[1:nrow(line.sample)], con="sample/sample.en_US.blogs.txt", "\n")
trainIndex <- createDataPartition(y=line.sample$linelen, p=0.5, list=FALSE)
line.train <- line.sample[trainIndex,1]
line.test <- line.sample[trainIndex,]
trainIndex2 <- createDataPartition(y=line.test$linelen, p=0.5, list=FALSE)
line.dev <- line.test[trainIndex2,1]
line.test <- line.test[-trainIndex2,1]

writeLines(line.train, con="sample/train/sample.en_US.blogs.training.txt", "\n")
writeLines(line.dev, con="sample/test/sample.en_US.blogs.test.txt", "\n")
writeLines(line.test, con="sample/dev/sample.en_US.blogs.dev.txt", "\n")



# # Get training and test set for twitter documents 
# index <- sample(1:length(line),samplesize)
# document.sample <- line[index]
# #document.sample1 <- RemoveControlSequence(document.sample)
# writeLines(document.sample, con="sample/sample.en_US.twitter.txt", "\n")
# 
# # Get training and test set fpr news documents 
# samplesize <- length(line2)*0.1
# index <- sample(1:length(line2),samplesize)
# document.sample <- line[index]
# #document.sample2 <- RemoveControlSequence(document.sample)
# writeLines(document.sample, con="sample/sample.en_US.news.txt", "\n")
# 
# # Get training and test set fpr news documents 
# samplesize <- length(line3)*0.1
# index <- sample(1:length(line3),samplesize)
# document.sample <- line[index]
# #document.sample3 <- RemoveControlSequence(document.sample)
# writeLines(document.sample, con="sample/sample.en_US.blogs.txt", "\n")

rm(line, line.sample, line.train, line.test,line.dev)

InsertSentenceTerminatorToken<-function(input.string)
{
    if (nchar(input.string)>1)
    {
        input.string <- as.String(input.string)
        sent_token_annotator <- Maxent_Sent_Token_Annotator()
        string.an <- annotate(input.string, sent_token_annotator)
        
        output.string <- lapply(input.string[string.an], function(x) paste0("yyy ",x," zzz", collapse = " "))
        output.string <- paste0(output.string, collapse = " ")
        
        return(output.string)
    }
    else
    {
        return(input.string)
    }
}

RemoveControlSequence<-function(x)
{
    x <- paste0(x,".",collapse=NULL)
    x <- gsub(pattern="!", replacement= ".", x)  # remove exclamtion marks
    x <- gsub(pattern="(\\w)\\1{2}", replacement="", x) # remove all repeated characters more than a double (language exaggerations)
    x <- InsertSentenceTerminatorToken(x)
    
    x <- gsub(pattern="â€¦", replacement= "", x) # elipsis
    x <- gsub(pattern="â€“", replacement= "", x) # long hyphen
    x <- gsub(pattern="â€™", replacement= "", x) # curly apostrophe
    x <- gsub(pattern="â€œ", replacement= "", x) # curly open quote
    x <- gsub(pattern="/â€[[:cntrl:]]/", replacement= "", x) # curly close quote
    x <- gsub(pattern="http\\w+", replacement= "", x) # http address
    x <- gsub(pattern="âu*", replacement= "", x) # other escapes
    x <- gsub(pattern="[[:digit:]]", replacement= "", x) # other escapes
    x <- gsub(pattern="'", replacement= "", x)  # remove apostrophes
    #     x <- gsub(pattern="[[:cntrl:]]", replacement= "", x)  # replace control characters with space
    x <- gsub(pattern="[]$*+.?[^{|(\\#%&~_=✬!,:;❵\")}@-]", replacement= "", x) # remove punctation
    x <- gsub(pattern="http\\w+", replacement= "", x)  # remove http refs
    x <- gsub(pattern="(ftp|http)(s?)://.*\\b", replacement= "", x)  # remove URL
    x <- gsub(pattern="RT |via ", replacement= "", x)  # remove witter tags
    x <- gsub(pattern="[@][a-zA-Z0-9_]{1,15}", replacement= "", x)  # remove Twitter usernames
    x <- gsub(pattern="\\b[A-Za-z0-9._-]*[@](.*?)[.].{1,3}\\b", replacement= "", x)  # remove emails
    x <- gsub(pattern="[^\x0A\x0D\x20-\x7E]", replacement="", x) # replace all non-ASCII characters
    #    x <- rm_emoticon(x) # remove emoticons
    
    return(x)
}

# Clean files

## Twitter
fileName <- "sample/train/sample.en_US.twitter.training.txt"
con <- file(fileName,open="r")
line <- readLines(con)
line <- paste(line, sep = " ", collapse = " ")
line <- RemoveControlSequence(line)
close(con)

fileConn<-file("sample/clean/corpus.twitter.txt")
writeLines(line, fileConn)
close(fileConn)

## News
fileName <- "sample/train/sample.en_US.news.training.txt"
con <- file(fileName,open="r")
line <- readLines(con)
line <- paste(line, sep = " ", collapse = " ")
line <- RemoveControlSequence(line)
close(con)

fileConn<-file("sample/clean/corpus.news.txt")
writeLines(line, fileConn)
close(fileConn)

## Blogs
fileName <- "sample/train/sample.en_US.blogs.training.txt"
con <- file(fileName,open="r")
line <- readLines(con)
line <- paste(line, sep = " ", collapse = " ")
line <- RemoveControlSequence(line)
close(con)

fileConn<-file("sample/clean/corpus.blogs.txt")
writeLines(line, fileConn)
close(fileConn)

# Creating the corpus clean
files <- DirSource(directory = "sample/clean",encoding ="UTF-8" )
US.Corpus <- VCorpus(x=files)
US.Corpus <- tm_map(US.Corpus, PlainTextDocument)
US.Corpus <- tm_map(US.Corpus, content_transformer(tolower)) # Case folding
US.Corpus <- tm_map(US.Corpus, content_transformer(removeNumbers))
US.Corpus <- tm_map(US.Corpus, content_transformer(removePunctuation))
profanity.words <- scan("bad-words.txt", "", quiet=TRUE)
exception.words <- scan("exception.txt", "", quiet=TRUE)
US.Corpus <- tm_map(US.Corpus, removeWords, profanity.words) 
US.Corpus <- tm_map(US.Corpus, removeWords, exception.words) 
US.Corpus <- Corpus(VectorSource(US.Corpus))

writeCorpus(US.Corpus, path = "sample/premodel/", filenames = c("corpus.blogs.txt","corpus.news.txt","corpus.twitter.txt"))

## Twitter
# Set proper seetence delimiters
fileName <- "sample/premodel/corpus.twitter.txt"
con <- file(fileName,open="r")
line <- readLines(con)
line <- gsub(pattern="yyy", replacement="<s>", line)  
line <- gsub(pattern="zzz", replacement="</s>", line)
close(con)

fileConn<-file("sample/premodel/corpus.twitter.txt")
writeLines(line, fileConn)
close(fileConn)

## News
fileName <- "sample/premodel/corpus.news.txt"
con <- file(fileName,open="r")
line <- readLines(con)
line <- gsub(pattern="yyy", replacement="<s>", line)  
line <- gsub(pattern="zzz", replacement="</s>", line)
close(con)

fileConn<-file("sample/premodel/corpus.news.txt")
writeLines(line, fileConn)
close(fileConn)

## Blogs
fileName <- "sample/premodel/corpus.blogs.txt"
con <- file(fileName,open="r")
line <- readLines(con)
line <- gsub(pattern="yyy", replacement="<s>", line)  
line <- gsub(pattern="zzz", replacement="</s>", line)
close(con)

fileConn<-file("sample/premodel/corpus.blogs.txt")
writeLines(line, fileConn)
close(fileConn)


