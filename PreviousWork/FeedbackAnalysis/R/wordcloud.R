cleanExpressions = function(text)
{
  # load text mining package from R
  require('tm')

  setwd("D:/MRIP_dev/wd/")
  #Load the stopwords, spamwords
  load("./rpackages/textpreprocess/data/stopwordsMustang.RData")
  source("./rpackages/textpreprocess/R/cleanupFunctions.R")

  
  print("==================================================================")
  print("Cleaning up!")  	
  
  print("==================================================================")
  

    text<-removeExtendedAsciiChars(text)
    text<-removeWebsites(text)
    text<-removeEmailIds(text)
    #     text<-removePhoneNumbers(text)
    text<-removeDigits(text)
    text<-expandContractions(text)
    text<-removeUsernames(text)
    text<-removeControlChars(text)
    #text<-removeNonAlphabetChars(text)
    text<-compressRepetition(text)
    text<-separatePunctuators(text)
    #text<-removeHashtags(text,removeTags = FALSE)
    text<-removeByLength(text,lowerCutOff=2,upperCutOff=13)
    text<-removeQuotes(text)
    # remove words cannot be used for sentiment ming
    # because it removes NEGATIONS and BUT
    text = tolower(text)
    
    

    
    text<-removeWords(text,stopwords("english")) # tm package function
    text<-removeWords(text,stopwords("SMART")) # tm package function
    text<-removeWords(text,stopwords("spanish")) # tm package function
    text<-removeWords(text,stopwords("german")) # tm package function
    text<-removeWords(text,stopwords("dutch")) # tm package function
    text<-removeWords(text,stopwords("finnish")) # tm package function
    text<-removeWords(text,stopwords("french")) # tm package function
    text<-removeWords(text,stopwords("spanish")) # tm package function
    text<-removeWords(text,wordsEng) # tm package function, public list
    text<-removeWords(text,wordsLong) # tm package function, public list
    text<-removeWords(text,wordsMySql) # tm package function, public list

    text<-removePunctuations(text)
    text<-compressWhiteSpaces(text)

  return(text)
}

# Load TweetArray_TopicModeling, which is stored in TweetArray_TopicModeling.RData
load("./comments.RData")

empComm = data_eeshan[,1]
manComm = data_eeshan[,2]

require(snowfall)

# Initialize maximum processing from CPU's
sfSetMaxCPUs()	

# Initialize parallel processing
sfInit(parallel=TRUE,cpus=4)

# Use snowfall processing commands
cleanEmpComm <- sfLapply(empComm, cleanExpressions)
cleanManComm <- sfLapply(manComm, cleanExpressions)
# Stop parallel execution
sfStop()

empVec = paste(unlist(cleanEmpComm),collapse=" ")
manVec = paste(unlist(cleanManComm),collapse=" ")
empTab = sort(table(strsplit(empVec,split=" ")),decreasing=TRUE)
manTab = sort(table(strsplit(manVec,split=" ")),decreasing=TRUE)
empTab = empTab[-which(names(empTab)=="")]
manTab = manTab[-which(names(manTab)=="")]
empTab = empTab[-which(names(empTab)=="null")]
manTab = manTab[-which(names(manTab)=="null")]
empDF = data.frame(name = names(empTab)[1:200],count = empTab[1:200])
manDF = data.frame(name = names(manTab)[1:200],count = manTab[1:200])
# empDF = as.matrix(data.frame(name = names(empTab)[1:200],count = empTab[1:200]))
# manDF = as.matrix(data.frame(name = names(manTab)[1:200],count = manTab[1:200]))
buzzwords = merge(x = empDF,y = manDF, by = "name", all=TRUE)
buzzwords[,2] = replace(as.numeric(buzzwords[,2]),as.numeric(which(is.na(buzzwords[,2]))),0)
buzzwords[,3] = replace(as.numeric(buzzwords[,3]),as.numeric(which(is.na(buzzwords[,3]))),0)

# Get top words from buzzwords
buzzwords[,4] = sapply(1:dim(buzzwords)[1], FUN=function(x) buzzwords[x,2]+buzzwords[x,3])
buzzwords = buzzwords[ order(-buzzwords[,4]), ]
colnames(buzzwords) = c("names","ecount","mcount")
# Keep only top 1000 words and drop sum
buzzwords = buzzwords[1:100,-4]

# Function to give index of comments to be shown
giveOccuranceIndex = function(word,VecE,VecM){
  wordOccurancesE = sapply(VecE, FUN=function(x) length(grep(word,x)))
  occuranceIndexE = as.list(which(wordOccurancesE>0))
  wordOccurancesM = sapply(VecM, FUN=function(x) length(grep(word,x)))
  occuranceIndexM = as.list(which(wordOccurancesM>0))
  occuranceList = list(word=word,emp_occurances=occuranceIndexE,man_occurances=occuranceIndexM)
  return(occuranceList)
}

topwords = as.vector(buzzwords[,1])
# Create JSON for word Occurances
WordOccurances = lapply(topwords,giveOccuranceIndex,cleanEmpComm,cleanManComm)
occurancesList = list(WORDS = WordOccurances,MANCOMMENTS = as.list(manComm), EMPCOMMENTS = as.list(empComm))
occuranceJSON = toJSON(occurancesList)
write(occuranceJSON,file="occuranceJSON.txt")

write.csv(empDF,file="empWordCloud.csv")
write.csv(manDF,file="manWordCloud.csv")
write.csv(buzzwords,file="buzzwords.csv",row.names=FALSE)

