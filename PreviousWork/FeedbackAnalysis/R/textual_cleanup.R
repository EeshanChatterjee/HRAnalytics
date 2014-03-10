# Transform the data

load("./feedback_textData.RData")


# Create Employee Nos
employees = unique(feedback_textData[,1])
numEmployees = length(employees)
empVec = sapply(1:numEmployees, FUN=function(x) paste(c('E',x),collapse=""))
empSpan = sapply(1:dim(feedback_textData)[1], FUN=function(i) empVec[which(employees == feedback_textData[i,1])])
feedback_textData$Employee <- empSpan

# Create Manager Nos
managers = unique(feedback_textData[,2])
numManagers = length(managers)
manVec = sapply(1:numManagers, FUN=function(x) paste(c('M',x),collapse=""))
manSpan = sapply(1:dim(feedback_textData)[1], FUN=function(i) manVec[which(managers == feedback_textData[i,2])])
feedback_textData$Current.Manager <- manSpan

save(feedback_textData,file="feedback_textData.RData")

names = read.csv('./empNames.csv',header=TRUE)
nameList = unlist(strsplit(as.character(unlist(names)),split=' '))
# remove Initials
removeByLength<- function(text,lowerCutOff,upperCutOff){
  pattern <- paste("\\b[a-zA-Z0-9]{1,",lowerCutOff,
                   "}\\b|\\b[a-zA-Z0-9]{",upperCutOff,",}\\b", sep="")
  text <- gsub(pattern, "", text)
  return(text)
}
nameList = as.character(sapply(nameList,removeByLength,2,20))
# remove dots, spaces and empty strings
nameList = gsub("\\.| ","",nameList)
nameList = nameList[which(nameList!="")]

stopwords = unique(nameList)


cleanupColIndex = c(3,4,6,7,8,9)

removeWordsInCol = function(x){ 
  feedback_textData[,x] <<- removeWords(as.vector(feedback_textData[,x]),stopwords)
  return()
}

sapply(cleanupColIndex,removeWordsInCol)

removeTilde = function(x){
  feedback_textData[,x] <<- gsub("~","",as.vector(feedback_textData[,x]))
  return()
}

sapply(cleanupColIndex,removeTilde)

# Create Factor IDs
factors = unique(feedback_textData[,10])
numFactors = length(factors)
facVec = sapply(1:numFactors, FUN=function(x) paste(c('F',x),collapse=""))
facSpan = sapply(1:dim(feedback_textData)[1], FUN=function(i) facVec[which(factors == feedback_textData[i,10])])
feedback_textData$Behavior <- facSpan
feedback_textData = feedback_textData[,-10]

# Create Dim IDs
dims = unique(feedback_textData[,5])
numDims = length(dims)
dimVec = sapply(1:numDims, FUN=function(x) paste(c('D',x),collapse=""))
dimSpan = sapply(1:dim(feedback_textData)[1], FUN=function(i) dimVec[which(dims == feedback_textData[i,5])])
feedback_textData$Dimension <- dimSpan


# Assign Easier Colnames to DF
colnames(feedback_textData) = c("Employee","Manager","FlowEcomm","FlowMcomm","Dimension","Emp.Dim.Comments","Mgr.Dim.Comments","Emp.Beh.Comments","Mgr.Beh.Comments")


save(feedback_textData,file = "feedback_textDataClean.RData")
