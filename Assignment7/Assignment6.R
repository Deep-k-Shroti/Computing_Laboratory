library(SnowballC)
library(tm)
library(NLP)

readFile <- function(fileName){
  filePointer <- file(fileName)
  mytxt <- readLines(filePointer)
  mytable <- read.table(text=gsub("^([^:]*:)|:", "\\1", mytxt), sep = ":", quote = "")
  mytable$id <- rep(1:(nrow(mytable)/8), each = 8)
  res <- reshape(mytable, direction = "wide", timevar = "V1", idvar = "id")
  close(filePointer)
  return(res)
}

stemDocumentfix <- function(x)
{
  PlainTextDocument(paste(stemDocument(unlist(strsplit(as.character(x), " "))),collapse=' '))
}

uniVocabulary <- function(dataframe){
  wordVector <- vector(mode="character")
  # wordVector <- vector(mode="character",length = 1000)
  k=1
  for (i in 1: nrow(dataframe)){
    word <- unlist(strsplit(dataframe[i, ]," "))
    #print (word)
    for (j in 1:length(word)){
      if(!(word[j] %in% wordVector)){
        if(!(word[j] == "")){
          wordVector[k] <- word[j]
          k=k+1
        }
      }
    }
  }
  wordVector <- sort(wordVector, decreasing = FALSE)
  return(wordVector)
}

makeNonFrame <- function(data){
  data <- Corpus(VectorSource(data))
  dataframe<-data.frame(text=unlist(sapply(data, `[`, "content")), 
                        stringsAsFactors=F)
  return(dataframe)
}


makeDataFrame <- function(data){
  data <- Corpus(VectorSource(data))
  dataNew <- tm_map(data, stemDocumentfix)
  dataNew <- tm_map(dataNew, removeWords, stopwords("english"))
  dataNew <- tm_map(dataNew, removePunctuation)
  dataNew <- tm_map(dataNew, removeNumbers)
  dataNew <- tm_map(dataNew, tolower)
  dataNew <- tm_map(dataNew, stripWhitespace)
  dataNew <- tm_map(dataNew, PlainTextDocument)
  dataNew <- tm_map(dataNew, removePunctuation)
  dataframe<-data.frame(text=unlist(sapply(dataNew, `[`, "content")), 
                        stringsAsFactors=F)
  return(dataframe)
}


makeUniDtm <- function(dataFrame, Vocab){
  numberDocument <- nrow(dataFrame)
  numberVocab <-length(Vocab)
  dtm <- matrix(data = 0, nrow = numberDocument, ncol = numberVocab)
  colnames(dtm) <- Vocab
  #Vocab
  #dtm
  
  for (i in 1:numberDocument){
    docWord <- unlist(strsplit(dataFrame[i, ]," "))
    #print(docWord)
    for (j in 1:length(docWord)){
      #print(docWord[j])
      if(docWord[j] %in% colnames(dtm)){
        # print(docWord[j])
        # print(dtm[i ,docWord[j]])
        dtm[i, docWord[j]] =  dtm[i,docWord[j]]+1
      }
    }
  }
  return (dtm)
}

makeUniTfIdf <- function(termMatrix){
  colSums(termMatrix != 0)
  uniIdf <<- (nrow(termMatrix)/(1+colSums(termMatrix != 0)))
  
  tf_idf_mul <- sweep(termMatrix,MARGIN=2,uniIdf,`*`)
  tf_idf_mul <- replace(tf_idf_mul, tf_idf_mul == 0, 1)
  tf_idf <- log(tf_idf_mul, base = exp(1))
  return(tf_idf)
}

makeBiTfIdf <- function(termMatrix){
  colSums(termMatrix != 0)
  biIdf <<- (nrow(termMatrix)/(1+colSums(termMatrix != 0)))
  
  tf_idf_mul <- sweep(termMatrix,MARGIN=2,biIdf,`*`)
  tf_idf_mul <- replace(tf_idf_mul, tf_idf_mul == 0, 1)
  tf_idf <- log(tf_idf_mul, base = exp(1))
  return(tf_idf)
}


biVocabulary <- function(dataframe){
  k=1
  wordVector <- vector(mode="character")
  for (i in 1: nrow(dataframe)){
    word <- unlist(strsplit(dataframe[i, ]," "))
    #print (word)
    for (j in 2:length(word)){
      if(!(word[j-1] == "")){
        biWord <- paste(word[j-1],word[j], sep = " ")
        print(biWord)
        if(!(biWord %in% wordVector)){
          wordVector[k] <- biWord
          k=k+1
        }
      }
    }
  }
  wordVector <- sort(wordVector, decreasing = FALSE)
  return(wordVector)
}

makeBiDtm <- function(dataFrame, Vocab){
  numberDocument <- nrow(dataFrame)
  numberVocab <-length(Vocab)
  dtm <- matrix(data = 0, nrow = numberDocument, ncol = numberVocab)
  colnames(dtm) <- Vocab
  #Vocab
  #dtm
  
  for (i in 1:numberDocument){
    docWord <- unlist(strsplit(dataFrame[i, ]," "))
    
    for (j in 2:length(docWord)){
      if(!(docWord[j-1] == "")){
        biDocWord <- paste(docWord[j-1],docWord[j], sep = " ")
        #print(biDocWord)
        
        if(biDocWord %in% colnames(dtm)){
          # print(docWord[j])
          # print(dtm[i ,docWord[j]])
          dtm[i,biDocWord] =  dtm[i,biDocWord]+1
        }
      }
    }
  }
  return (dtm)
}


filepath <- "test.txt"
fileData = readFile(filepath)

summary = fileData$'V2.review/summary'
text = fileData$'V2.review/text'
data <- paste(summary, text, sep=" ")
#data <- summary

uniDataFrame <- makeDataFrame(data)
uniVocab <- uniVocabulary(uniDataFrame)
#uniVocab
uniTermMatrix <- makeUniDtm(uniDataFrame, uniVocab)
#uniTermMatrix
uniTfIdf <- makeUniTfIdf(uniTermMatrix)
#uniTfIdf

png(file="unigram.png")
hMapUni <-  heatmap(uniTfIdf, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))
dev.off()




biDataFrame <- makeDataFrame(data)
biVocab <- biVocabulary(biDataFrame)
#biVocab



biTermMatrix <- makeBiDtm(biDataFrame, biVocab)
#biTermMatrix
biTfIdf <- makeBiTfIdf(biTermMatrix)
#biTfIdf

png(file="bigram.png")
hMapUni <-  heatmap(biTfIdf, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))
dev.off()



#####################################QUERY############################################
readquery <- function(){
  query <- readline(prompt = "Enter a Query:")
  return(as.character(query)) 
}

makeQueryTfIdf = function(termMatrix,idf){
  tf_idf_mul <- sweep(termMatrix,MARGIN=2,idf,`*`)
  tf_idf_mul <- replace(tf_idf_mul, tf_idf_mul == 0, 1)
  tf_idf <- log(tf_idf_mul, base = exp(1))
  return(tf_idf)
}



query <- readquery()
queryDataFrame <- makeDataFrame(query)


uniQueryTermMatrix <- makeUniDtm(queryDataFrame, uniVocab)
uniQueryTermMatrix
uniQueryTfIdf <- makeQueryTfIdf(uniQueryTermMatrix,uniIdf)
uniQueryTfIdf

biQueryTermMatrix <- makeBiDtm(queryDataFrame, biVocab)
biQueryTermMatrix
biQueryTfIdf <- makeQueryTfIdf(biQueryTermMatrix,biIdf)
biQueryTfIdf
 

coSine <- function(matrix1, matrix2){
  product  = sum(matrix1 * matrix2)
 # print("Product:")
 # print(product)
  if((sqrt(sum(matrix1^2))*sqrt(sum(matrix1^2))) != 0){
    product = product/(sqrt(sum(matrix1^2))*sqrt(sum(matrix1^2)))
  }else{
    product <- 0
  }
  return (product)
}


cosineSimilarity <- function( queryTfIdf, tf_idf ){
  rankVector <- vector(mode="double")
  for ( i in 1:nrow(tf_idf) ){
    rankVector[i] <- coSine( queryTfIdf, tf_idf[i,])
    #print(queryTfIdf)
    #print(tf_idf[i,])
  }
  return (rankVector)
}


display <- function(rankTotal,uniDataFrame,count){
  displayVector <- vector(mode="double")
  rankTotalSorted <- unique((sort(rankTotal, decreasing = TRUE)))
  #print(rankTotalSorted)
  for(i in 1:length(rankTotal)){
    #displayVectorPt[i] <- match(rankTotalSorted[i],rankTotal)
    displayVector <- c(displayVector, which(rankTotal == rankTotalSorted[i], arr.ind = TRUE))
    #print(displayVectorPt[i])
  }
  
  print("The Query Result is")
  resultCount = 1
  for(j in displayVector){
    if(resultCount > count){
      break
    }
    #print(j)
    print(uniDataFrame$text[j])
    resultCount = resultCount+1
  }
}

rankUni <- cosineSimilarity(uniQueryTfIdf,uniTfIdf)
rankUni
rankBi <- cosineSimilarity(biQueryTfIdf,biTfIdf)
rankBi

alpha = 0.2
beta = 0.8

rankTotal = alpha*rankUni + beta*rankBi
print(rankTotal)
rawFrame <- makeNonFrame(data)
display(rankTotal,rawFrame,2)


