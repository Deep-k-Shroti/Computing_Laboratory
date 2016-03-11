library(NLP)
library(SnowballC)
library(tm)


readFile <- function(fileName){
  filePointer <- file(fileName)
  mytxt <- readLines(filePointer)
  mytable <- read.table(text=gsub("^([^:]*:)|:", "\\1", mytxt), sep = ":", quote = "")
  mytable$id <- rep(1:(nrow(mytable)/8), each = 8)
  res <- reshape(mytable, direction = "wide", timevar = "V1", idvar = "id")
  close(filePointer)
  return(res)
}

readFileSumTxt <- function(filePath,pattern){
  myText <- readChar(filePath,  file.info(filePath)$size)
  myText1 <- unlist(strsplit(myText, "\r*\n"))
  dataList <- (grep(pattern, myText1,value = TRUE))
  data <- lapply(dataList, function(x){strsplit(x, ":")[[1]][2]})
  dataFinal <- lapply(seq(1,length(data),2),function(x){paste(data[x],data[x+1]) }  )
  return(unlist(dataFinal))
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

#setwd("~/Desktop/Compu Lab/Text Mining")
filePath <- "test.txt"
fileData = readFileSumTxt(filePath,"^(review/summary)|^(review/text)")

#fileData = readFile(filePath)
#summary = fileData$'V2.review/summary'
#text = fileData$'V2.review/text'
#data <- paste(fileData, text, sep=" ")
#data <- summary
#uniDataFrame <- makeDataFrame(data)

uniDataFrame <- makeDataFrame(fileData)
uniVocab <- uniVocabulary(uniDataFrame)
#uniVocab
uniTermMatrix <- makeUniDtm(uniDataFrame, uniVocab)
#uniTermMatrix
uniTfIdf <- makeUniTfIdf(uniTermMatrix)
#uniTfIdf

png(file="unigram.png")
hMapUni <-  heatmap(uniTfIdf, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))
dev.off()




#biDataFrame <- makeDataFrame(data)
biDataFrame <- makeDataFrame(fileData)
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

rawFrame <- makeNonFrame(fileData)
####Output L1
print("Output R1")
alpha = 1
beta= 0
rankTotal = alpha*rankUni + beta*rankBi
display(rankTotal,rawFrame,2)


####Output L2
print("Output R2")
alpha = 0.3
beta = 0.7
rankTotal = alpha*rankUni + beta*rankBi
#print(rankTotal)
display(rankTotal,rawFrame,2)


readFileNew <- function(filePath,pattern){
  myText <- readChar(filePath,  file.info(filePath)$size)
  myText1 <- unlist(strsplit(myText, "\r*\n"))
  dataList <- (grep(pattern, myText1,value = TRUE))
  # print(dataList)
  data <- lapply(dataList, function(x){strsplit(x, ":")[[1]][2]})
  return(unlist(data))
}

userId = readFileNew(filePath,"^(review/userId)")
productId = readFileNew(filePath,"^(product/productId)")
helpfulness = readFileNew(filePath,"^(review/helpfulness)")
lapply(strsplit(helpfulness, "/"), as.numeric)
helpfulness <- matrix(unlist(lapply(strsplit(helpfulness, "/"), as.numeric)), ncol = 2, byrow = T)
helpfulness <- helpfulness[,1] / helpfulness[,2] #GET HELPFULNESS AS FRACTION
helpfulness[is.nan(helpfulness)] <- 0 #Replace NaNs in rhlp by zero
rating = readFileNew(filePath,"^(review/score)")
rating <- as.numeric(rating)
write.table(data.frame(userId, productId, rating, helpfulness), file = 'data2.csv', row.names=FALSE,col.names=TRUE,sep=",")


uniqueProductId <- unique(productId)
uniqueUserId <- unique(userId)
V <- matrix(data = 1/length(uniqueUserId), nrow = 1, ncol = length(uniqueUserId))
colnames(V) <- uniqueUserId
M <- matrix(data = 0 , nrow = length(uniqueUserId), ncol = length(uniqueUserId))
dfNew <- data.frame(userId, productId, rating, helpfulness)
#rm(userId, productId,helpfulness,rating)
#userId <- read.csv(file = "data2.csv")$userId
#productId <- read.csv(file = "data2.csv")$productId
#rating <- read.csv(file = "data2.csv")$rating
#helpfulness <- read.csv(file = "data2.csv")$helpfulness
#dataFameUidPid <- data.frame(read.csv(file = "data2.csv")$userId, read.csv(file = "data2.csv")$productId)


uniqueProductIdIndex <- lapply(uniqueProductId, function(x) which(productId %in% x))
names(uniqueProductIdIndex) <- uniqueProductId
uniqueProductIdIndex


uniqueUserIdIndex <- lapply(uniqueUserId, function(x) which(userId %in% x))
names(uniqueUserIdIndex) <- uniqueUserId
uniqueUserIdIndex
U<-uniqueUserIdIndex

for(temp in uniqueProductId){
  #print(length(uniqueProductIdIndex[[temp]]))
  if(length(uniqueProductIdIndex[[temp]]) >= 2){
    
    #print(uniqueProductIdIndex[[temp]])
    c <- (combn(uniqueProductIdIndex[[temp]], 2))
    print(c)
    for(i in 1: ncol(c)){
      d <- which(uniqueUserId %in% dfNew[c[1,i],1])
      k <- which(uniqueUserId %in% dfNew[c[2,i],1])
      
      
      if(dfNew[c[1,i],3] == 0 || dfNew[c[2,i],3] == 0 || dfNew[c[1,i],4] == 0 ){
        M[d,k] <- M[d,k] + 0.1
      }else{
        M[d,k] <- M[d,k] + (5 - abs(dfNew[c[1,i],3] - dfNew[c[2,i],3]))/(5*(length(uniqueProductIdIndex[[temp]]) - 1))
      }
      
      
      if(dfNew[c[1,i],3] == 0 || dfNew[c[2,i],3] == 0 || dfNew[c[2,i],4] == 0 ){
        M[k,d] <- M[k,d] + 0.1
      }else{
        M[k,d] <- M[k,d] + (5 - abs(dfNew[c[2,i],3] - dfNew[c[1,i],3]))/(5*(length(uniqueProductIdIndex[[temp]]) - 1))
      }
    }
  }
}

M 
M <- sweep(M,MARGIN=1,rowSums(M)+0.000001,`/`)
#M <- sweep(M,MARGIN=1,rowSums(M),`/`)
#M <- replace(MNew, is.na(MNew), 0)
M

r <- t(V)
while(1){
  rNew = M %*% r
  #print((rNew - r))
  print(max(abs(rNew - r)))
  if(max(abs(rNew - r)) < 0.00001)
    break
  r = rNew
}


rNew
rankTotal

