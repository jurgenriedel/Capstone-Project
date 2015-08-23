#Sys.setenv(JAVA_HOME="C:\\Program Files (x86)\\Java\\jdk1.8.0_45\\jre")

require("tm")
require("RWeka")
require("qdapRegex")
require("stringr")
require("dplyr")
require("qdap")
require("data.table")
require("slam")
require("distr")

# Building the model

## 1. Relative frequency approach

#We take some training corpus, and from this corpus take the count of a particular bigram, and divide
#this count by the sum of all the bigrams that share the same first word: 
#    <bigram count>/<unigram count of first token in bigram>

#This ratio is called a relative frequency; the use of relative frequencies as a way to estimate probabilities is one example of the technique
#known asMaximum Likelihood Estimation orMLE, because the resulting MLE parameter set is one in which the likelihood of the training set T given the
#model M (i.e. P(TjM)) is maximized.

# Replace words with <UNA>-token which are not in the dicionary
files <- DirSource(directory = "sample/premodel",encoding ="UTF-8" )
US.Corpus <- VCorpus(x=files)
US.Corpus <- Corpus(VectorSource(US.Corpus))

#token.delim <- " \\t\\r\\n.!?,;\"()"
token.delim <- " \\r\\n\\t.,;:\"()?!"
Unigram.1 <- NGramTokenizer(US.Corpus[[1]], Weka_control(min = 1, max = 1,delimiters = token.delim))
Unigram.2 <- NGramTokenizer(US.Corpus[[2]], Weka_control(min = 1, max = 1,delimiters = token.delim))
Unigram.3 <- NGramTokenizer(US.Corpus[[3]], Weka_control(min = 1, max = 1,delimiters = token.delim))
#Unigram <- Unigram.1
Unigram <- c(Unigram.1,Unigram.2,Unigram.3)

# converting tokens of n-grams into tables
Tab_unigram <- data.frame(table(Unigram))
# Filter out unigrams with cout 1
#Tab_unigram <- filter(Tab_unigram, Freq>1)
# Filter start/end of sentence tokens
Tab_unigram <- filter(Tab_unigram, !grepl('s>$',Unigram))
# sorting the word distribution frequency 
UnigramGrp <- Tab_unigram[order(Tab_unigram$Freq,decreasing = TRUE),]
word.count <- sum(UnigramGrp$Freq)
wf.all <- UnigramGrp
wf.all$type="Term"
# Add ranking
wf.all <- transform(wf.all, 
                    rank = ave(Freq, type, 
                               FUN = function(x) rank(-x, ties.method = "first")))
wf.all.sum <- mutate(group_by(wf.all,type), cumsum=cumsum(Freq))

# Create corpus dictionary based of 90% word type coverage
corpus.dictionary <- filter(wf.all.sum, cumsum<word.count*0.9)
corpus.exclusions <- filter(wf.all.sum, cumsum>=word.count*0.9)

corpus.exclusions <- mutate(corpus.exclusions, pat=paste0(" ",Unigram," "))

pat.list <- as.character(corpus.exclusions$pat)

saveRDS(corpus.dictionary, file = "corpus.dictionary.rda")
saveRDS(corpus.exclusions, file = "corpus.exclusions.rda")

# Create model files
RemoveInfrequentWords<-function(x)
{
    x <- mgsub(pat.list," <unk> ",x)
    
    return(x)
}

## Twitter
fileName <- "sample/premodel/corpus.twitter.txt"
con <- file(fileName,open="r")
line <- readLines(con)
line <- RemoveInfrequentWords(line)  
close(con)

fileConn<-file("sample/model/corpus.twitter.una2.txt")
writeLines(line, fileConn)
close(fileConn)

## News
fileName <- "sample/premodel/corpus.news.txt"
con <- file(fileName,open="r")
line <- readLines(con)
line <- RemoveInfrequentWords(line)  
close(con)

fileConn<-file("sample/model/corpus.news.una.txt")
writeLines(line, fileConn)
close(fileConn)

## Blogs
fileName <- "sample/premodel/corpus.blogs.txt"
con <- file(fileName,open="r")
line <- readLines(con)
line <- RemoveInfrequentWords(line)  
close(con)

fileConn<-file("sample/model/corpus.blogs.una.txt")
writeLines(line, fileConn)
close(fileConn)

# Building the model
files <- DirSource(directory = "sample/model",encoding ="UTF-8" )
US.Corpus <- VCorpus(x=files)
US.Corpus <- VCorpus(VectorSource(US.Corpus)) # Building the main corpus

#US.Corpus <- Corpus(VectorSource(US.Corpus))

#token.delim <- " \\t\\r\\n.!?,;\"()"
token.delim <- " \\r\\n\\t.,;:\"()?!"
Unigram.1 <- NGramTokenizer(US.Corpus[[1]], Weka_control(min = 1, max = 1,delimiters = token.delim))
Unigram.2 <- NGramTokenizer(US.Corpus[[2]], Weka_control(min = 1, max = 1,delimiters = token.delim))
Unigram.3 <- NGramTokenizer(US.Corpus[[3]], Weka_control(min = 1, max = 1,delimiters = token.delim))
Unigram <- c(Unigram.1,Unigram.2,Unigram.3)

# converting tokens of n-grams into tables
Tab_unigram <- data.frame(table(Unigram))
Tab_unigram <- mutate(Tab_unigram, Freq=ifelse(Freq==1,0,Freq))

# sorting the word distribution frequency  
UnigramGrp <- Tab_unigram[order(Tab_unigram$Freq,decreasing = TRUE),]

## General measures
# V = vocabulary size, N = Token size
V <- nrow(corpus.dictionary)
N <- sum(UnigramGrp$Freq)

# Filter out unigrams with cout 1
UnigramGrp <- mutate(UnigramGrp, Freq=ifelse(Freq==1,0,Freq))

## Add-on smoothing
unigram.prob.norm <- mutate(UnigramGrp,rel.freq=Freq/N,AD_count=(as.numeric(Freq)+1), AD_prob=(as.numeric(Freq)+1)/(N+V))
## Add Good-Turing smoothing
unigram.fof <- aggregate(Unigram ~ Freq, unigram.prob.norm, length)
colnames(unigram.fof) <- c("Freq","n")

DT <- data.table(unigram.fof)
setkey(DT, Freq)
DT[ DT[,list(Freq-1L,n)], n_next:=i.n, roll=+Inf]

unigram.fof.df <- data.frame(DT)
## Set N for max frequency = 0
max.freq <- max(unigram.fof$Freq)
unigram.fof.df <- mutate(unigram.fof.df, n_next=ifelse(Freq==max.freq,0,n_next))
unigram.fof.df <- mutate(unigram.fof.df, GT=(Freq+1)*n_next/n)
unigram.prob.norm.sm <- merge(x = unigram.prob.norm, y = unigram.fof.df, by = "Freq", all.x = TRUE)

# Define trashhold for GT smoothing
k=5
unigram.prob.norm.sm <- mutate(unigram.prob.norm.sm, PGT=ifelse(Freq>k,rel.freq,GT/N))

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
Bigram.tdm.1 <- TermDocumentMatrix(US.Corpus[1], control = list(tokenize = BigramTokenizer))
Bigram.tdm.2 <- TermDocumentMatrix(US.Corpus[2], control = list(tokenize = BigramTokenizer))
Bigram.tdm.3 <- TermDocumentMatrix(US.Corpus[3], control = list(tokenize = BigramTokenizer))
Bigram.tdm <- c(Bigram.tdm.1,Bigram.tdm.2,Bigram.tdm.3)

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
Trigram.tdm.1 <- TermDocumentMatrix(US.Corpus[1], control = list(tokenize = TrigramTokenizer))
Trigram.tdm.2 <- TermDocumentMatrix(US.Corpus[2], control = list(tokenize = TrigramTokenizer))
Trigram.tdm.3 <- TermDocumentMatrix(US.Corpus[3], control = list(tokenize = TrigramTokenizer))
Trigram.tdm <- c(Trigram.tdm.1,Trigram.tdm.2,Trigram.tdm.3)

QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
Quadgram.tdm.1 <- TermDocumentMatrix(US.Corpus[1], control = list(tokenize = QuadgramTokenizer))
Quadgram.tdm.2 <- TermDocumentMatrix(US.Corpus[2], control = list(tokenize = QuadgramTokenizer))
Quadgram.tdm.3 <- TermDocumentMatrix(US.Corpus[3], control = list(tokenize = QuadgramTokenizer))
Quadgram.tdm <- c(Quadgram.tdm.1,Quadgram.tdm.2,Quadgram.tdm.3)

# Calculate frequencies of tokens
bigram.freq <- rowapply_simple_triplet_matrix(Bigram.tdm,sum)
trigram.freq <- rowapply_simple_triplet_matrix(Trigram.tdm,sum)
quadgram.freq <- rowapply_simple_triplet_matrix(Quadgram.tdm,sum)

bigram.name.1 <- sapply(strsplit(names(bigram.freq), ' '), function(a) a[1])
bigram.name.2 <- sapply(strsplit(names(bigram.freq), ' '), function(a) a[2])

trigram.name.1 <- sapply(strsplit(names(trigram.freq), ' '),function(a) a[1])
trigram.name.2 <- sapply(strsplit(names(trigram.freq), ' '),function(a) a[2])
trigram.name.3 <- sapply(strsplit(names(trigram.freq), ' '),function(a) a[3])

quadgram.name.1 <- sapply(strsplit(names(quadgram.freq), ' '),function(a) a[1])
quadgram.name.2 <- sapply(strsplit(names(quadgram.freq), ' '),function(a) a[2])
quadgram.name.3 <- sapply(strsplit(names(quadgram.freq), ' '),function(a) a[3])
quadgram.name.4 <- sapply(strsplit(names(quadgram.freq), ' '),function(a) a[4])

# Convert lists into data frames
bigramDF <- data.frame(names(bigram.freq),bigram.freq,bigram.name.1,bigram.name.2,stringsAsFactors = F)
trigramDF <- data.frame(names(trigram.freq),trigram.freq,paste(trigram.name.1,trigram.name.2),trigram.name.3,stringsAsFactors = F)
quadgramDF <- data.frame(names(quadgram.freq),quadgram.freq,paste(quadgram.name.1,quadgram.name.2,quadgram.name.3),quadgram.name.4,stringsAsFactors = F)

names(bigramDF) <- c('bigram','freq','Unigram','name')
names(trigramDF) <- c('trigram','freq','bigram','name')
names(quadgramDF) <- c('quadgram','freq','trigram','name')

# Calclate relative frequencies
## Bigrams
bigram.prob <- merge(x = bigramDF, y = UnigramGrp, by = "Unigram", all.x = TRUE)
bigram.prob.norm <- mutate(bigram.prob, rel.freq = ifelse(Freq==0,0,freq/Freq))
bigram.prob.norm$ID<-seq.int(nrow(bigram.prob.norm)) # Add id

## Add-one smoothing
bigram.prob.norm <- mutate(bigram.prob.norm,AD_count=(as.numeric(freq)+1), AD_prob=(as.numeric(freq)+1)/(Freq+V))

## Goodman-Turing discounting -Calculate frequencies of frequencies count
bigram.fof <- aggregate(bigram ~ freq, bigramDF, length)
colnames(bigram.fof) <- c("freq","n")
bigram.fof <- bigram.fof[order(bigram.fof$freq),]
max.freq <- max(bigram.fof$freq)
DT <- data.table(bigram.fof)
setkey(DT, freq)
DT[ DT[,list(freq-1L,n)], n_next:=i.n, roll=+Inf]
bigram.fof.df <- data.frame(DT)
## Set N for max frequency = 0
bigram.fof.df <- mutate(bigram.fof.df, n_next=ifelse(freq==max.freq,0,n_next))
bigram.fof.df <- mutate(bigram.fof.df, GT=n_next/n)
bigram.prob.norm.sm <- merge(x = bigram.prob.norm, y = bigram.fof.df, by = "freq", all.x = TRUE)
# Define trashhold for GT smoothing
k=5
bigram.prob.norm.sm <- mutate(bigram.prob.norm.sm, PGT=ifelse(freq>k,rel.freq,GT/as.numeric(Freq)))

## Trigrams
trigram.prob <- merge(x = trigramDF, y = bigramDF, by = "bigram", all.x = TRUE)
trigram.prob.norm <- mutate(trigram.prob, rel.freq = ifelse(freq.y==0,0,freq.x/freq.y))
trigram.prob.norm$ID<-seq.int(nrow(trigram.prob.norm)) # Add id

## Add-one smoothing
trigram.prob.norm <- mutate(trigram.prob.norm,AD_count=(as.numeric(freq.x)+1), AD_prob=(as.numeric(freq.x)+1)/(freq.y+V))

## Goodman-Turing discounting -Calculate frequencies of frequencies count
trigram.fof <- aggregate(trigram ~ freq, trigramDF, length)
colnames(trigram.fof) <- c("freq.x","n")
trigram.fof <- trigram.fof[order(trigram.fof$freq.x),]
max.freq <- max(trigram.fof$freq.x)
DT <- data.table(trigram.fof)
setkey(DT, freq.x)
DT[ DT[,list(freq.x-1L,n)], n_next:=i.n, roll=+Inf]
trigram.fof.df <- data.frame(DT)
colnames(trigram.fof.df) <- c("freq.x","n","n_next")
## Set N for max frequency = 0
trigram.fof.df <- mutate(trigram.fof.df, n_next=ifelse(freq.x==max.freq,0,n_next))
trigram.fof.df <- mutate(trigram.fof.df, GT=n_next/n)
trigram.prob.norm.sm <- merge(x = trigram.prob.norm, y = trigram.fof.df, by = "freq.x", all.x = TRUE)
# Define trashhold for GT smoothing
k=5
trigram.prob.norm.sm <- mutate(trigram.prob.norm.sm, PGT=ifelse(freq.x>k,rel.freq,GT/as.numeric(freq.y)))

## Quadgrams
quadgram.prob <- merge(x = quadgramDF, y = trigramDF, by = "trigram", all.x = TRUE)
quadgram.prob.norm <- mutate(quadgram.prob, rel.freq = ifelse(freq.y==0,0,freq.x/freq.y))
quadgram.prob.norm$ID<-seq.int(nrow(quadgram.prob.norm)) # Add id

## Add-one smoothing
quadgram.prob.norm <- mutate(quadgram.prob.norm,AD_count=(as.numeric(freq.x)+1), AD_prob=(as.numeric(freq.x)+1)/(freq.y+V))

## Goodman-Turing discounting -Calculate frequencies of frequencies count
quadgram.fof <- aggregate(quadgram ~ freq, quadgramDF, length)
colnames(quadgram.fof) <- c("freq.x","n")
quadgram.fof <- quadgram.fof[order(quadgram.fof$freq.x),]
max.freq <- max(quadgram.fof$freq.x)
DT <- data.table(quadgram.fof)
setkey(DT, freq.x)
DT[ DT[,list(freq.x-1L,n)], n_next:=i.n, roll=+Inf]
quadgram.fof.df <- data.frame(DT)
colnames(quadgram.fof.df) <- c("freq.x","n","n_next")
## Set N for max frequency = 0
quadgram.fof.df <- mutate(quadgram.fof.df, n_next=ifelse(freq.x==max.freq,0,n_next))
quadgram.fof.df <- mutate(quadgram.fof.df, GT=n_next/n)
quadgram.prob.norm.sm <- merge(x = quadgram.prob.norm, y = quadgram.fof.df, by = "freq.x", all.x = TRUE)
# Define trashhold for GT smoothing
k=5
quadgram.prob.norm.sm <- mutate(quadgram.prob.norm.sm, PGT=ifelse(freq.x>k,rel.freq,GT/as.numeric(freq.y)))

trigram.prob.norm.sm <- trigram.prob.norm.sm[,c(2,3,4,8,11,15)]
quadgram.prob.norm.sm <- quadgram.prob.norm.sm[,c(2,3,4,8,11,15)]

saveRDS(unigram.prob.norm.sm, file = "unigram_prob.rda")
saveRDS(bigram.prob.norm.sm, file = "bigram_prob.rda")
saveRDS(trigram.prob.norm.sm, file = "trigram_prob.rda")
saveRDS(quadgram.prob.norm.sm, file = "quadgram_prob.rda")

#View(unigram.prob.norm.sm)
#View(bigram.prob.norm.sm)
#View(trigram.prob.norm.sm)
#View(quadgram.prob.norm.sm)

# Shannon test

# Load probality objects
unigram.prob.norm.sm <- readRDS("unigram_prob.rda")
bigram.prob.norm.sm <- readRDS("bigram_prob.rda")
trigram.prob.norm.sm <- readRDS("trigram_prob.rda")
quadgram.prob.norm.sm <- readRDS("quadgram_prob.rda")


CreateRandomSequence<-function(n)
{
#     # Initialize    
#     search.token <- "<s>"
#     my.sentence <- search.token
#     
#     while(search.token!="</s>"){
#         search.token <- as.character(unigram.prob.norm.sm[sample(1:nrow(unigram.prob.norm.sm))[1],]$Unigram)
#         my.sentence <- paste0(my.sentence," ",search.token)
#     }
    
  library(distr)
    
  # Get random star ngram
  if (n==2){
    search.token <- "<s>"
  } else if (n==3){
    ngram.sample <- filter(trigram.prob.norm.sm, grepl("<s>",bigram))
    search.token <- ngram.sample[sample(nrow(ngram.sample))[1],]$bigram
  } else if (n==4){
    ngram.sample <- filter(quadgram.prob.norm.sm, grepl("<s>",trigram))
    search.token <- ngram.sample[sample(nrow(ngram.sample))[1],]$trigram
  }
  
  # Initialize
  my.sentence <- search.token
  
  # Loop
  while(as.character(data.frame(strsplit(search.token," "))[n-1,])!="</s>"){
    if (n==2){ random.ngram <- filter(bigram.prob.norm.sm, Unigram==search.token)
    } else if (n==3){ random.ngram <- filter(trigram.prob.norm.sm, bigram==search.token)
      if (nrow(random.ngram)==0) {
        random.ngram <- filter(bigram.prob.norm.sm, bigram==search.token)
      }
    } else if (n==4) {random.ngram <- filter(quadgram.prob.norm.sm, trigram==search.token)}
    
    # Normalize factor to probabilities
#     norm.factor <- sum(random.ngram$PGT)
#     random.ngram <- mutate(dt, PGT=PGT/norm.factor)
    
    valid.id <- if (nrow(random.ngram)==1)
    {c(random.ngram$ID,1)}else{
      c(as.numeric(random.ngram$ID),1)
    }
    
    valid.rel.freq <- if (nrow(random.ngram)==1)
    {c(random.ngram$rel.freq,1-random.ngram$rel.freq)}else{
      c(as.numeric(random.ngram$rel.freq),1-sum(random.ngram$rel.freq))
    }
      
    dist.ngram <- DiscreteDistribution (supp = valid.id, prob = valid.rel.freq)
    rDist <- r(dist.ngram)  ## Random number generation
    newID <- rDist(1)
    
    if (n==2) {new.token <- filter(bigram.prob.norm.sm,ID==newID)$bigram
    } else if (n==3) {new.token <- filter(trigram.prob.norm.sm,ID==newID)$trigram
    }else if (n==4) {new.token <- filter(quadgram.prob.norm.sm,ID==newID)$quadgram}
    
    search.token <- paste(as.character(data.frame(strsplit(new.token," "))[2:n,]), collapse=' ')
    
    my.sentence <- paste(my.sentence, paste(as.character(data.frame(strsplit(new.token," "))[n,]), collapse=' '),collapse=" ")
  }
  
  my.sentence <-  gsub(pattern="<s>", replacement= "", my.sentence)
  my.sentence <-  gsub(pattern="</s>", replacement= "", my.sentence)
  
  return(paste0(my.sentence,"."))
}

## Creating Shannon text
k=2
shannon.text <- ""
for (n in 1:10)
{
    shannon.text <- paste(str_trim(CreateRandomSequence(k)),str_trim(shannon.text), collapse=".")
}
shannon.text


