require("tm")
require("RWeka")
require("stringr")
require("dplyr")
require("plyr")
require("qdap")
require("data.table")
require("NLP")
require("openNLP")
require("slam")

# Load probality objects
unigram.prob.norm.sm <- readRDS("unigram_prob.rda")
bigram.prob.norm.sm <- readRDS("bigram_prob.rda")
trigram.prob.norm.sm <- readRDS("trigram_prob.rda")
quadgram.prob.norm.sm <- readRDS("quadgram_prob.rda")
corpus.dictionary <- readRDS("corpus.dictionary.rda")

PrepareInput<-function(x)
{
  x <- paste0(x,".",collapse=NULL)
  x <- gsub(pattern="!", replacement= ".", x)  # remove exclamtion marks
  x <- gsub(pattern="(\\w)\\1{2}", replacement="", x) # remove all repeated characters more than a double (language exaggerations)
  
  x <- gsub(pattern="â€¦", replacement= "", x) # elipsis
  x <- gsub(pattern="â€“", replacement= "", x) # long hyphen
  x <- gsub(pattern="â€™", replacement= "", x) # curly apostrophe
  x <- gsub(pattern="â€œ", replacement= "", x) # curly open quote
  x <- gsub(pattern="/â€[[:cntrl:]]/", replacement= "", x) # curly close quote
  x <- gsub(pattern="http\\w+", replacement= "", x) # http address
  x <- gsub(pattern="âu*", replacement= "", x) # other escapes
  x <- gsub(pattern="[[:digit:]]", replacement= "", x) # other escapes
  x <- gsub(pattern="'", replacement= "", x)  # remove apostrophes
  x <- gsub(pattern="[]$*+.?[^{|(\\#%&~_=✬!,:;❵\")}@-]", replacement= "", x) # remove punctation
  x <- gsub(pattern="http\\w+", replacement= "", x)  # remove http refs
  x <- gsub(pattern="(ftp|http)(s?)://.*\\b", replacement= "", x)  # remove URL
  x <- gsub(pattern="RT |via ", replacement= "", x)  # remove witter tags
  x <- gsub(pattern="[@][a-zA-Z0-9_]{1,15}", replacement= "", x)  # remove Twitter usernames
  x <- gsub(pattern="\\b[A-Za-z0-9._-]*[@](.*?)[.].{1,3}\\b", replacement= "", x)  # remove emails
  x <- gsub(pattern="[^\x0A\x0D\x20-\x7E]", replacement="", x) # replace all non-ASCII characters
  x <- Corpus(VectorSource(x))
  x <- tm_map(x, PlainTextDocument)
  x <- tm_map(x, content_transformer(tolower)) # Case folding
  x <- tm_map(x, content_transformer(removeNumbers))
  x <- tm_map(x, content_transformer(removePunctuation))
  x <- Corpus(VectorSource(x))
  x <- x[[1]]
  if (nchar(str_trim(x))>1){
    x <- RemoveUnknownWords(x)
  }

  x <- paste0("<s> ",x)
  
  return(x)
}

RemoveUnknownWords<-function(ngram)
{  
  token.delim <- " \\r\\n\\t.,;:\"()?!"
  Unigram.sentence <- NGramTokenizer(Corpus(VectorSource(ngram))[[1]], Weka_control(min = 1, max = 1,delimiters = token.delim))
  Tab_unigram.sentence <- data.frame(table(Unigram.sentence))
  colnames(Tab_unigram.sentence) <- c("Unigram","Freq")
  df1 <- Tab_unigram.sentence[,1]
  df2 <- corpus.dictionary[,1]
  df1 <- data.frame(df1)
  df2 <- data.frame(df2)
  df1 <- mutate(df1, Unigram.pat=paste0(" ",df1," "))
  df2 <- mutate(df2, Unigram.pat=paste0(" ",Unigram," "))
  
  pattern.filter <- as.character(filter(join(df1, df2,type = "left", by="Unigram.pat"),is.na(Unigram)==TRUE)$Unigram.pat)
  sentence.clean <- mgsub(pattern.filter," <unk> ",paste0(" ",ngram," "))
  
  return(sentence.clean)
}

# Back-off pobability
GetNgramProbSimpleBackoff<-function(ngram, mode)
{
  df <- data.frame(strsplit(ngram," "))
  n <- nrow(df)
  
  
  if (n==4){
    tri.discount <- 0.5
    bi.discount <- 0.25
    uni.discount <- 0.25
    
    quad.gram <- paste(data.frame(strsplit(ngram," "))[1:4,1], collapse = " ")
    tri.gram <- paste(data.frame(strsplit(ngram," "))[2:4,1], collapse = " ")
    bi.gram <- paste(data.frame(strsplit(ngram," "))[3:4,1], collapse = " ")
    uni.gram <- paste(data.frame(strsplit(ngram," "))[4:4,1], collapse = " ")
    
    dt <- filter(quadgram.prob.norm.sm, quadgram==quad.gram)
    
    if (mode==1){
      prob.quadgram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
    } else if (mode==2){
      prob.quadgram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
    } else if (mode==3){
      prob.quadgram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
    }        
    
    if(prob.quadgram>0){
      total.prob <- prob.quadgram
    }else{
      dt <- filter(trigram.prob.norm.sm, trigram==tri.gram)
      
      if (mode==1){
        prob.trigram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
      } else if (mode==2){
        prob.trigram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
      } else if (mode==3){
        prob.trigram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
      }        
      
      if(prob.trigram>0){
        total.prob <- tri.discount*prob.trigram
      }else{
        dt <- filter(bigram.prob.norm.sm, bigram==bi.gram)
        
        if (mode==1){
          prob.bigram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
        } else if (mode==2){
          prob.bigram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
        } else if (mode==3){
          prob.bigram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
        } 
        
        if(prob.bigram>0){
          total.prob <-bi.discount * prob.bigram
        }else{
          dt <- filter(unigram.prob.norm.sm,Unigram==uni.gram)
          
          if (mode==1){
            prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
          } else if (mode==2){
            prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
          } else if (mode==3){
            prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
          } 
          if(prob.unigram>0){
            total.prob <- uni.discount * prob.unigram
          } else
          {
            dt <- filter(unigram.prob.norm.sm,Unigram=="<unk>")
            if (mode==1){
              prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
            } else if (mode==2){
              prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
            } else if (mode==3){
              prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
            }
            total.prob <- uni.discount * prob.unigram
          }
        }
      }
    }
  }else if (n==3){
    bi.discount <- 0.75
    uni.discount <- 0.25
    
    tri.gram <- paste(data.frame(strsplit(ngram," "))[1:3,1], collapse = " ")
    bi.gram <- paste(data.frame(strsplit(ngram," "))[2:3,1], collapse = " ")
    uni.gram <- paste(data.frame(strsplit(ngram," "))[3:3,1], collapse = " ")
    
    dt <- filter(trigram.prob.norm.sm, trigram==tri.gram)
    if (mode==1){
      prob.trigram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
    } else if (mode==2){
      prob.trigram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
    } else if (mode==3){
      prob.trigram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
    }        
    if(prob.trigram>0){
      total.prob <- prob.trigram
    }else{
      dt <- filter(bigram.prob.norm.sm, bigram==bi.gram)
      if (mode==1){
        prob.bigram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
      } else if (mode==2){
        prob.bigram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
      } else if (mode==3){
        prob.bigram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
      } 
      if(prob.bigram>0){
        total.prob <- bi.discount * prob.bigram
      }else{
        dt <- filter(unigram.prob.norm.sm,Unigram==uni.gram)
        if (mode==1){
          prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
        } else if (mode==2){
          prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
        } else if (mode==3){
          prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
        }
        if(prob.unigram>0){
          total.prob <- uni.discount * prob.unigram
        } else
        {
          dt <- filter(unigram.prob.norm.sm,Unigram=="<unk>")
          if (mode==1){
            prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
          } else if (mode==2){
            prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
          } else if (mode==3){
            prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
          }
          total.prob <- uni.discount * prob.unigram
        }
      }
    }
  }else if (n==2){
    uni.discount <- 0.5
    
    bi.gram <- paste(data.frame(strsplit(ngram," "))[1:2,1], collapse = " ")
    uni.gram <- paste(data.frame(strsplit(ngram," "))[2:2,1], collapse = " ")
    
    dt <- filter(bigram.prob.norm.sm, bigram==bi.gram)
    if (mode==1){
      prob.bigram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
    } else if (mode==2){
      prob.bigram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
    } else if (mode==3){
      prob.bigram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
    } 
    if(prob.bigram>0){
      total.prob <- prob.bigram
    }else{
      dt <- filter(unigram.prob.norm.sm,Unigram==uni.gram)
      if (mode==1){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
      } else if (mode==2){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
      } else if (mode==3){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
      } 
      if(prob.unigram>0){
        total.prob <- uni.discount* prob.unigram
      } else
      {
        dt <- filter(unigram.prob.norm.sm,Unigram=="<unk>")
        if (mode==1){
          prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
        } else if (mode==2){
          prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
        } else if (mode==3){
          prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
        }
        total.prob <- uni.discount * prob.unigram
      }
    }    
  }else if (n==1){
    uni.gram <- paste(data.frame(strsplit(ngram," "))[1:1,1], collapse = " ")
    
    dt <- filter(unigram.prob.norm.sm,Unigram==uni.gram)
    if (mode==1){
      prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
    } else if (mode==2){
      prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
    } else if (mode==3){
      prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
    } 
    if(prob.unigram>0){
      total.prob <- prob.unigram
    } else
    {
      dt <- filter(unigram.prob.norm.sm,Unigram=="<unk>")
      if (mode==1){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
      } else if (mode==2){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
      } else if (mode==3){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
      }
      total.prob <- prob.unigram
    }
  }
  
  return (total.prob)
}

# Interpolation probability
GetNgramProbInterpolation<-function(ngram, mode)
{
  df <- data.frame(strsplit(ngram," "))
  n <- nrow(df)
  # Set interpolation factors
  quad.factors <- c(0.625,0.125,0.125,0.125)
  tri.factors <- c(0.5,0.25,0.225)
  bi.factors <- c(0.75,0.25)
  
  if (n==4){
    quad.gram <- paste(data.frame(strsplit(ngram," "))[1:4,1], collapse = " ")
    tri.gram <- paste(data.frame(strsplit(ngram," "))[2:4,1], collapse = " ")
    bi.gram <- paste(data.frame(strsplit(ngram," "))[3:4,1], collapse = " ")
    uni.gram <- paste(data.frame(strsplit(ngram," "))[4:4,1], collapse = " ")
    
    if (mode==1){
      prob.quadgram <- as.numeric(filter(quadgram.prob.norm.sm, quadgram==quad.gram)$rel.freq)
      prob.trigram <- as.numeric(filter(trigram.prob.norm.sm, trigram==tri.gram)$rel.freq)
      prob.bigram <- as.numeric(filter(bigram.prob.norm.sm, bigram==bi.gram)$rel.freq)
      prob.unigram <- as.numeric(filter(unigram.prob.norm.sm,Unigram==uni.gram)$rel.freq)
    } else if (mode==2){
      prob.quadgram <- as.numeric(filter(quadgram.prob.norm.sm, quadgram==quad.gram)$AD_prob)
      prob.trigram <- as.numeric(filter(trigram.prob.norm.sm, trigram==tri.gram)$AD_prob)
      prob.bigram <- as.numeric(filter(bigram.prob.norm.sm, bigram==bi.gram)$AD_prob)
      prob.unigram <- as.numeric(filter(unigram.prob.norm.sm,Unigram==uni.gram)$AD_prob)    
    } else if (mode==3){
      prob.quadgram <- as.numeric(filter(quadgram.prob.norm.sm, quadgram==quad.gram)$PGT)
      prob.trigram <- as.numeric(filter(trigram.prob.norm.sm, trigram==tri.gram)$PGT)
      prob.bigram <- as.numeric(filter(bigram.prob.norm.sm, bigram==bi.gram)$PGT)
      prob.unigram <- as.numeric(filter(unigram.prob.norm.sm,Unigram==uni.gram)$PGT)    
    }
    
    prob.quadgram=ifelse(length(prob.quadgram)==0,0,prob.quadgram)
    prob.trigram=ifelse(length(prob.trigram)==0,0,prob.trigram)
    prob.bigram=ifelse(length(prob.bigram)==0,0,prob.bigram)
    prob.unigram=ifelse(length(prob.unigram)==0,0,prob.unigram)
    
    if(prob.unigram==0)
    {
      dt <- filter(unigram.prob.norm.sm,Unigram=="<unk>")
      if (mode==1){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
      } else if (mode==2){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
      } else if (mode==3){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
      }
    }
    
    total.prob <- quad.factors[1]*prob.unigram+quad.factors[2]*prob.bigram+quad.factors[3]*prob.trigram+quad.factors[4]*prob.quadgram
  } else if (n==3){
    tri.gram <- paste(data.frame(strsplit(ngram," "))[1:3,1], collapse = " ")
    bi.gram <- paste(data.frame(strsplit(ngram," "))[2:3,1], collapse = " ")
    uni.gram <- paste(data.frame(strsplit(ngram," "))[3:3,1], collapse = " ")
    
    if (mode==1){
      prob.trigram <- as.numeric(filter(trigram.prob.norm.sm, trigram==tri.gram)$rel.freq)
      prob.bigram <- as.numeric(filter(bigram.prob.norm.sm, bigram==bi.gram)$rel.freq)
      prob.unigram <- as.numeric(filter(unigram.prob.norm.sm,Unigram==uni.gram)$rel.freq)
    } else if (mode==2){
      prob.trigram <- as.numeric(filter(trigram.prob.norm.sm, trigram==tri.gram)$AD_prob)
      prob.bigram <- as.numeric(filter(bigram.prob.norm.sm, bigram==bi.gram)$AD_prob)
      prob.unigram <- as.numeric(filter(unigram.prob.norm.sm,Unigram==uni.gram)$AD_prob)    
    } else if (mode==3){
      prob.trigram <- as.numeric(filter(trigram.prob.norm.sm, trigram==tri.gram)$PGT)
      prob.bigram <- as.numeric(filter(bigram.prob.norm.sm, bigram==bi.gram)$PGT)
      prob.unigram <- as.numeric(filter(unigram.prob.norm.sm,Unigram==uni.gram)$PGT)    
    }
    
    prob.trigram=ifelse(length(prob.trigram)==0,0,prob.trigram)
    prob.bigram=ifelse(length(prob.bigram)==0,0,prob.bigram)
    prob.unigram=ifelse(length(prob.unigram)==0,0,prob.unigram)
    
    if(prob.unigram==0)
    {
      dt <- filter(unigram.prob.norm.sm,Unigram=="<unk>")
      if (mode==1){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
      } else if (mode==2){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
      } else if (mode==3){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
      }
    }
    
    total.prob <- tri.factors[1]*prob.unigram+tri.factors[2]*prob.bigram+tri.factors[3]*prob.trigram
  } else if (n==2){
    bi.gram <- paste(data.frame(strsplit(ngram," "))[1:2,1], collapse = " ")
    uni.gram <- paste(data.frame(strsplit(ngram," "))[2:2,1], collapse = " ")
    
    if (mode==1){
      prob.bigram <- as.numeric(filter(bigram.prob.norm.sm, bigram==bi.gram)$rel.freq)
      prob.unigram <- as.numeric(filter(unigram.prob.norm.sm,Unigram==uni.gram)$rel.freq)
    } else if (mode==2){
      prob.bigram <- as.numeric(filter(bigram.prob.norm.sm, bigram==bi.gram)$AD_prob)
      prob.unigram <- as.numeric(filter(unigram.prob.norm.sm,Unigram==uni.gram)$AD_prob)    
    } else if (mode==3){
      prob.bigram <- as.numeric(filter(bigram.prob.norm.sm, bigram==bi.gram)$PGT)
      prob.unigram <- as.numeric(filter(unigram.prob.norm.sm,Unigram==uni.gram)$PGT)    
    }
    
    prob.bigram=ifelse(length(prob.bigram)==0,0,prob.bigram)
    prob.unigram=ifelse(length(prob.unigram)==0,0,prob.unigram)
    
    if(prob.unigram==0)
    {
      dt <- filter(unigram.prob.norm.sm,Unigram=="<unk>")
      if (mode==1){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
      } else if (mode==2){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
      } else if (mode==3){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
      }
    }
    
    total.prob <- bi.factors[1]*prob.unigram+bi.factors[2]*prob.bigram
  } else if (n==1){
    uni.gram <- paste(data.frame(strsplit(ngram," "))[1:1,1], collapse = " ") 
    if (mode==1){
      prob.unigram <- as.numeric(filter(unigram.prob.norm.sm,Unigram==uni.gram)$rel.freq)
    } else if (mode==2){
      prob.unigram <- as.numeric(filter(unigram.prob.norm.sm,Unigram==uni.gram)$AD_prob)    
    } else if (mode==3){
      prob.unigram <- as.numeric(filter(unigram.prob.norm.sm,Unigram==uni.gram)$PGT)    
    }
    prob.unigram=ifelse(length(prob.unigram)==0,0,prob.unigram)
    
    if(prob.unigram==0)
    {
      dt <- filter(unigram.prob.norm.sm,Unigram=="<unk>")
      if (mode==1){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$rel.freq),0)
      } else if (mode==2){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$AD_prob),0)
      } else if (mode==3){
        prob.unigram <- ifelse(nrow(dt)>0,as.numeric(dt$PGT),0)
      }
    }
    
    total.prob <- prob.unigram
  }
  
  return (total.prob)
}


########################################################################################################
## Word prediction

GetLastWordfromNGram<-function(ngram)
{
  dt <- data.frame(strsplit(ngram," "))
  last.word <- paste(dt[nrow(dt),])
  
  return(last.word)
}

PredictNextWord<-function(ngram.main, probmode, method, s.limit)
{
  # ngram.main <- "to take a"
  # ngram.main <- RemoveUnknownWords(ngram.main)
  #      
  # probmode <- 1
  # method <- 0
  
    df <- data.frame(strsplit(ngram.main," "))
    n <- nrow(df)
    #s.limit <- 20
    
    if (n==3){
        search.dt.4 <- filter(quadgram.prob.norm.sm,trigram==paste0(df[1:3,], collapse = " "))
        search.dt.3 <- filter(trigram.prob.norm.sm,bigram==paste0(df[2:3,], collapse = " "))
        search.dt.2 <- filter(bigram.prob.norm.sm,Unigram==paste0(df[3:3,], collapse = " "))
        colnames(search.dt.4)[3] <- "name"
        colnames(search.dt.3)[3] <- "name"
        colnames(search.dt.2)[4] <- "name"
        search.dt.4 <- search.dt.4[c("name","rel.freq","AD_prob","PGT")]
        search.dt.3 <- search.dt.3[c("name","rel.freq","AD_prob","PGT")]
        search.dt.2 <- search.dt.2[c("name","rel.freq","AD_prob","PGT")]
        search.dt <- rbind(search.dt.4,search.dt.3,search.dt.2)
        
        if (probmode==1){
            search.dt <- head(arrange(search.dt, desc(rel.freq)),s.limit)["name"] 
        } else if (probmode==2){
            search.dt <- head(arrange(search.dt, desc(AD_prob)),s.limit)["name"] 
        } else if (probmode==3){
            search.dt <- head(arrange(search.dt, desc(PGT)),s.limit)["name"]     
        }
        search.dt <- unique(search.dt)
        
        if(nrow(search.dt)==0){
            search.dt.3 <- filter(trigram.prob.norm.sm,bigram==paste0(df[2:3,], collapse = " "))
            search.dt.2 <- filter(bigram.prob.norm.sm,Unigram==paste0(df[3:3,], collapse = " "))
            colnames(search.dt.3)[3] <- "name"
            colnames(search.dt.2)[4] <- "name"
            search.dt.3 <- search.dt.3[c("name","rel.freq","AD_prob","PGT")]
            search.dt.2 <- search.dt.2[c("name","rel.freq","AD_prob","PGT")]
            search.dt <- rbind(search.dt.3,search.dt.2)
            
            if (probmode==1){
                search.dt <- head(arrange(search.dt, desc(rel.freq)),s.limit)["name"] 
            } else if (probmode==2){
                search.dt <- head(arrange(search.dt, desc(AD_prob)),s.limit)["name"] 
            } else if (probmode==3){
                search.dt <- head(arrange(search.dt, desc(PGT)),s.limit)["name"]     
            }
            search.dt <- unique(search.dt)
        } 
        if(nrow(search.dt)==0){
            search.dt <- filter(bigram.prob.norm.sm,Unigram==paste0(df[3:3,], collapse = " "))
            if (probmode==1){
                search.dt <- head(arrange(search.dt, desc(rel.freq)),s.limit)["name"]  
            } else if (probmode==2){
                search.dt <- head(arrange(search.dt, desc(AD_prob)),s.limit)["name"]    
            } else if (probmode==3){
                search.dt <- head(arrange(search.dt, desc(PGT)),s.limit)["name"]    
            }
        }
        if(nrow(search.dt)==0){
            search.dt <- filter(unigram.prob.norm.sm,Unigram==df[3:3,])
            if (probmode==1){
                search.dt <- head(arrange(search.dt, desc(rel.freq)),s.limit)["name"]  
            } else if (probmode==2){
                search.dt <- head(arrange(search.dt, desc(AD_prob)),s.limit)["name"]    
            } else if (probmode==3){
                search.dt <- head(arrange(search.dt, desc(PGT)),s.limit)["name"]    
            }
        }
    } else if (n==2){    
        search.dt.3 <- filter(trigram.prob.norm.sm,bigram==paste0(df[1:2,], collapse = " "))
        search.dt.2 <- filter(bigram.prob.norm.sm,Unigram==paste0(df[2:2,], collapse = " "))
        colnames(search.dt.3)[3] <- "name"
        colnames(search.dt.2)[4] <- "name"
        search.dt.3 <- search.dt.3[c("name","rel.freq","AD_prob","PGT")]
        search.dt.2 <- search.dt.2[c("name","rel.freq","AD_prob","PGT")]
        search.dt <- rbind(search.dt.3,search.dt.2)
        
        if (probmode==1){
            search.dt <- head(arrange(search.dt, desc(rel.freq)),s.limit)["name"] 
        } else if (probmode==2){
            search.dt <- head(arrange(search.dt, desc(AD_prob)),s.limit)["name"] 
        } else if (probmode==3){
            search.dt <- head(arrange(search.dt, desc(PGT)),s.limit)["name"]     
        }
        search.dt <- unique(search.dt)
        if(nrow(search.dt)==0){
            search.dt <- filter(bigram.prob.norm.sm,Unigram==paste0(df[2:2,], collapse = " "))
            if (probmode==1){
                search.dt <- head(arrange(search.dt, desc(rel.freq)),s.limit)["name"]  
            } else if (probmode==2){
                search.dt <- head(arrange(search.dt, desc(AD_prob)),s.limit)["name"]    
            } else if (probmode==3){
                search.dt <- head(arrange(search.dt, desc(PGT)),s.limit)["name"]    
            }      
        }
        if(nrow(search.dt)==0){
            search.dt <- filter(unigram.prob.norm.sm,Unigram==df[2:2,])
            if (probmode==1){
                search.dt <- head(arrange(search.dt, desc(rel.freq)),s.limit)["name"]  
            } else if (probmode==2){
                search.dt <- head(arrange(search.dt, desc(AD_prob)),s.limit)["name"]    
            } else if (probmode==3){
                search.dt <- head(arrange(search.dt, desc(PGT)),s.limit)["name"]    
            }
        }
    } else if (n==1){
        search.dt <- filter(unigram.prob.norm.sm,Unigram==df[1:1,])
        if (probmode==1){
            search.dt <- head(arrange(search.dt, desc(rel.freq)),s.limit)["name"]  
        } else if (probmode==2){
            search.dt <- head(arrange(search.dt, desc(AD_prob)),s.limit)["name"]    
        } else if (probmode==3){
            search.dt <- head(arrange(search.dt, desc(PGT)),s.limit)["name"]    
        }
    }
    
    search.dt <- mutate(search.dt, search.gram=paste0(ngram.main," ",name))[c("search.gram")]
    search.dt$ID<-seq.int(nrow(search.dt)) # Add id
    
    if (method==0){
        prob.dt <- data.frame(unlist(lapply(search.dt[,'search.gram'], GetNgramProbSimpleBackoff, mode=probmode)))
    } else if (method==1){
        prob.dt <- data.frame(unlist(lapply(search.dt[,'search.gram'], GetNgramProbInterpolation, mode=probmode)))
    }
    
    prob.dt$ID<-seq.int(nrow(prob.dt)) # Add id
    colnames(prob.dt) <- c("prob","ID")
    prob.dt <- merge(x = search.dt, y = prob.dt, by = "ID", all.x = TRUE)
    prob.dt <- arrange(prob.dt, desc(prob), search.gram)
    #word.list <- data.frame(prob.dt$search.gram)
    word.list <- data.frame(unlist(lapply(prob.dt[,"search.gram"],GetLastWordfromNGram)))
    colnames(word.list)[1] <- "token"
    word.list <- head(filter(word.list, !grepl('<',token)),10)
    
    return(word.list)
}
