Sys.setenv (HADOOP_HOME="/opt/cloudera/parcels/CDH/lib/hadoop")
Sys.setenv (HADOOP_CMD="/opt/cloudera/parcels/CDH/lib/hadoop/bin/hadoop")
Sys.setenv (HADOOP_STREAMING= "/opt/cloudera/parcels/CDH-5.1.0-1.cdh5.1.0.p0.53/lib/hadoop-0.20-mapreduce/contrib/streaming/hadoop-streaming-2.3.0-mr1-cdh5.1.0.jar")

library(rmr2)
setwd("/home/mtech/12EC35013/Desktop/Hadoop Assignment")
#inputFile = "/user/mtech/12EC35013/Mydata.csv"
inputFile = "/user/mtech/12EC35013/lastfm_dataset_modified.csv"
QueryOutputFile = "/user/mtech/12EC35013/myOutput/output.txt"
system("hadoop fs -rm -R -skipTrash /user/mtech/12EC35013/myOutput")

###################################################################################################################

####################################################Query1#########################################################
firstQuery = function(input, output){
  wc.map=
    function(., lines){
      
      songID <- vector(mode = "integer")
      title <- vector(mode = "integer")
      for(line in lines){
        lineVector <- unlist(strsplit(line, split = ","))    
        countTag <- length(unlist(strsplit(x = lineVector[2],split = ";")))
        if(length(unlist(strsplit(x = lineVector[2],split = ";"))) > NUM_TAGS){
          songID <- c(songID,lineVector[4])
          title <- c(title,lineVector[3])
        }
      }
      keyval(songID, title)
    }
  
  
  wc.reduce =
    function(word, counts ){
      #keyval(word, sum(counts))
    }
  
  mapreduce(
    input = input ,
    output = output,
    input.format = "text",
    map = wc.map,
    #reduce = wc.reduce,
    combine = NULL
  )
}

###################################################################################################################

####################################################Query2#########################################################
secondQuery = function(input, output){
  wc.map=
    function(., lines){
      artistList <- vector(mode = "integer")
      songTitleList <- vector(mode = "integer")
      for(line in lines){
        lineVector <- unlist(strsplit(line , split= ","))
        artist <- tolower((unlist(strsplit(lineVector[1],split =";"))))
        artist <- gsub("^\\s+|\\s+$", "", artist)
        lengthArtist <- length(artist)
        songTitle <- paste(lineVector[3], "(",lineVector[4],")", sep = "")
        songTitle <- rep(songTitle,lengthArtist) 
        artistList <- c(artistList,artist)
        songTitleList <- c(songTitleList,songTitle)
      }
      keyval(artistList,songTitleList)   
    }
  
  wc.reduce =
    function(artistList, songTitleList ){
      if(length(songTitleList)> NUM_SONGS){
        return(keyval(artistList, paste(songTitleList, collapse=";")))
      }
    }
  
  mapreduce(
    input = input ,
    output = output,
    input.format = "text",
    map = wc.map,
    reduce = wc.reduce,
    combine = F
  )
}

###################################################################################################################

####################################################Query3#########################################################
thirdQuery = function(input, output){
  wc.map=
    function(., lines){
      artistList <- vector(mode = "integer")
      songTitleList <- vector(mode = "integer")
      for(line in lines){
        lineVector <- unlist(strsplit(line, split = ",")) 
        
        if(length(unlist(strsplit(x = lineVector[2],split = ";"))) > NUM_TAGS){
          artist <- tolower((unlist(strsplit(lineVector[1],split =";"))))
          artist <- gsub("^\\s+|\\s+$", "", artist)
          lengthArtist <- length(artist)
          songTitle <- paste(lineVector[3], "(",lineVector[4],")", sep = "")
          songTitle <- rep(songTitle,lengthArtist) 
          
          artistList <- c(artistList,artist)
          songTitleList <- c(songTitleList,songTitle)
        }
      }
      keyval(artistList,songTitleList)
    }
    
  wc.reduce =
    function(artistList, songTitleList ){
      if(length(songTitleList)> NUM_SONGS){
        return(keyval(artistList, paste(songTitleList, collapse=";")))
      } 
    }
  
  mapreduce(
    input = input ,
    output = output,
    input.format = "text",
    map = wc.map,
    reduce = wc.reduce,
    combine = F
  )
}


###################################################################################################################

####################################################Query4#########################################################
fourthQuery = function(input, output){
  wc.map=
    function(., lines){
      artistList <- vector(mode = "integer")
      songTitleList <- vector(mode = "integer")
      for(line in lines){
        lineVector <- unlist(strsplit(line , split= ","))
        artist <- tolower((unlist(strsplit(lineVector[1],split =";"))))
        artist <- gsub("^\\s+|\\s+$", "", artist)
        lengthArtist <- length(artist)
        songTitle <- rep(lineVector[3],lengthArtist) 
        artistList <- c(artistList,artist)
        songTitleList <- c(songTitleList,songTitle)
      }
      keyval(artistList,songTitleList)   
    }
  
  wc.reduce =
    function(artistList, songTitleList ){
      return(keyval(artistList, paste(songTitleList, collapse=";")))
    }
  
  mapreduce(
    input = input ,
    output = output,
    input.format = "text",
    map = wc.map,
    reduce = wc.reduce,
    combine = F
  )
}

#######################################################################TODO:GREP
artistSearch <- function(artistQueryName){
  artist <- gsub("^\\s+|\\s+$", "", tolower(artistQueryName))
  indexOflist <- indice[tolower(substr(artistQueryName,1,1))]
  indices <- grep(artistQueryName, names(finalIndex[[indexOflist]]))
  artist_songs <- vector(mode = "character")
  if(length(indices) < 1 ){
    return("No Song Found")
  }else{
    songs <- unlist( lapply(results.df4[finalIndex[[indexOflist]][indices],2] , function(x) paste(results.df4[finalIndex[[indexOflist]][indices],1],unlist( strsplit(toString(x), ";")), sep=" : ") ) )
  }
  return (songs)
}


#artistSearch <- function(artistQueryName){
# artist <- gsub("^\\s+|\\s+$", "", tolower(artistQueryName))
#  indexOflist <- indice[tolower(substr(artistQueryName,1,1))]
#  print(indexOflist)
#  if(artistQueryName %in% names(finalIndex[[indexOflist]])){
#    songs <- unlist(strsplit(results.df4[finalIndex[[indexOflist]][artistQueryName],2],split=";"))
#    artist_song <- paste("Artist:",results.df4[finalIndex[[indexOflist]][artistQueryName],1],"      Song:",results.df4[finalIndex[[indexOflist]][artistQueryName],2], sep = " ")
#    return (artist_song)
#  }else{
#    return ("Artist not found in the database")
#  }
#}

while(1){
  message("Please Enter a QUERY:") 
  message("1 ==> Print titles and ids of the songs which have more than NUM_TAGS") 
  message("2 ==> Print  names  of  the   artists,  along  with  song  names  and  song  ids,  who  have  sung  of  more than NUM_SONGS songs.") 
  message("3 ==> Print artists,  along  with  song  names  and  song  ids which have more than NUM_TAGS and have  sung more than NUM_SONGS songs")
  message("4 ==> Print names  of  all  the songs  sung  by that artist.")  
  message("5 ==> Exit")
  QUERY_NUM <- readline()
  
  if(QUERY_NUM == 1){
    print("You have selected Query 1")
    message("Enter NUM_TAGS:")
    NUM_TAGS <- readline()
    
    firstQuery(input = inputFile, output = QueryOutputFile)
    results.df1 <- as.data.frame (from.dfs(QueryOutputFile), stringsAsFactors=F)
    colnames(results.df1) <- c ('Title', 'SongID')
    write.csv(results.df1,"Query1.csv")
    system("hadoop fs -rm -R -skipTrash /user/mtech/12EC35013/myOutput")
    message("Output printed in Query1.csv")
    
  }else if(QUERY_NUM == 2){
    print("You have selected Query 2")
    message("Enter NUM_SONGS:")
    NUM_SONGS <- readline()
    
    secondQuery(input = inputFile, output = QueryOutputFile)
    results.df2 <- as.data.frame (from.dfs(QueryOutputFile), stringsAsFactors=F)
    colnames(results.df2) <- c ('Artist','Song')
    write.csv(results.df2,"Query2.csv")
    system("hadoop fs -rm -R -skipTrash /user/mtech/12EC35013/myOutput")
    message("Output printed in Query1.csv")
    
  }else if(QUERY_NUM == 3){
    print("You have selected Query 3")
    message("Enter NUM_TAGS:")
    NUM_TAGS <- readline()
    message("Enter NUM_SONGS:")
    NUM_SONGS <- readline()
    
    thirdQuery(input = inputFile, output = QueryOutputFile) 
    results.df3 <- as.data.frame (from.dfs(QueryOutputFile), stringsAsFactors=F)
    colnames(results.df3) <- c ('Artist','Song')
    write.csv(results.df3,"Query3.csv")
    system("hadoop fs -rm -R -skipTrash /user/mtech/12EC35013/myOutput")
    message("Output printed in Query1.csv")
    
  }else if(QUERY_NUM == 4){
    print("You have selected Query 4")
    
    fourthQuery(input = inputFile, output = QueryOutputFile) 
    results.df4 <- as.data.frame (from.dfs(QueryOutputFile), stringsAsFactors=F)
    #results.df[,3] <- c(1:nrow(results.df))
    colnames(results.df4) <- c ('Artist','Song')
    write.csv(results.df4,"Query4.csv")
    system("hadoop fs -rm -R -skipTrash /user/mtech/12EC35013/myOutput")
    
    letter <- paste(letters,LETTERS, sep ="")
    pattern <-unlist(lapply(paste(letters,LETTERS, sep =""), function(x){paste(c("^[",x,"].*"),collapse="")}))
    finalIndex <- list()
    for(i in 1:26){
      position <- grep(pattern[i],results.df4[,1])
      tempIndex <- position
      names(tempIndex) <- results.df4[position,1]
      tempIndex <-list(tempIndex)
      finalIndex <- append(finalIndex,tempIndex)
    }
    
    
    
    message("Enter an Artist name")
    ARTIST_NAME <- readline()
    
    #######################################################################TODO:write.csv(finalIndex,"Index.csv")
    indice <- c(1:26)
    names(indice) <- letters
    songResult<- artistSearch(ARTIST_NAME)
    print(songResult)
    write.table(c(LETTERS[1], finalIndex[[1]]) ,file = "Index.csv", sep = ",", append = FALSE)
    for (i in 2:length(finalIndex))
      write.table(c(LETTERS[i] , finalIndex[[i]]) , file = "Index.csv", sep = ",", append = TRUE)
    message("Output printed in Index.csv")
    
  }else if(QUERY_NUM == 5){
    break
  }else{
    print("Enter a valid Query Number")
  }
}
