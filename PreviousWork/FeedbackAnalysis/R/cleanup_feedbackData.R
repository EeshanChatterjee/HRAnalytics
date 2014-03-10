wordFreqAnalysis = function(text){
  text = unlist(strsplit(text, split=' ',perl = TRUE))
  datatable = table(text)
  return(datatable)
}


con = file('./Appraisal/Appraisal.txt')
open(con)
text = readLines(con, warn = TRUE,n=-1L)
close(con)

xx = read.csv("./Appraisal/Appraisal.csv", header=TRUE)
text = unique(unlist(xx[,c(7,9,11,12,16,18)]))

# text = gsub('\\t'," ",text)
text = gsub('~'," ",text)
# text = gsub('\\.',"",text)
text = removePunctuations(text)

# Remove everything except nouns, verbs and adjectives
text = cleanByPOS(text)

datatable = wordFreqAnalysis(text)
dataFrame = as.data.frame(datatable)
names(dataFrame) = c("Terms","freq")
orderedDataFrame = dataFrame[with(dataFrame, order(freq)), ]


text = gsub('MuSigma...+? ',"",text)


cleanByPOS = function(text){
  require(openNLP)
  require(openNLPmodels.en)
  
  taggedText = tagPOS(text)
  splitText = strsplit(taggedText,split=' ')[[1]]
  
  returnTAG = function(taggedWord){
    tagIndices =  regexpr("/NN",taggedWord)
    return(tagIndices)
  }
  
  validWordsIndices = which(returnTAG(splitText)>0)
  validSplitText = splitText[validWordsIndices]
  validText = paste(gsub("/.+","",validSplitText),collapse=' ')
  
  return(validText)
}

removePunctuations<- function(text){
  text = gsub('[[:punct:]]', ' ', text)
  return(text)
}

# Function to reshape the data

yy = reshape(feedback_catData,idvar="Employee",direction="wide",timevar="Behavior",v.names=c("Emp.Beh.Rating","Mgr.Beh.Rating"))

###############################################################

eid = c("E1","E1","E1","E1","E1","E1","E2","E2","E2","E2","E2","E2")
dim = c("D1","D1","D1","D2","D2","D3","D1","D1","D1","D2","D2","D3")
chr = c("C1","C1","C2","C3","C4","C5","C1","C1","C2","C3","C4","C5")
fac = c("F1","F2","F3","F4","F5","F6","F1","F2","F3","F4","F5","F6")
fv = c(1,0,2,3,1,2)
fc = c("SC","SC","SC","SC","SC","SC")
mv = c(1,1,2,3,2,1)
mc = c("MC","MC","MC","MC","MC","MC")

myDF = data.frame(eid = eid,dim = dim,chr = chr,fac = fac,fv = fv,fc = fc,mv = mv,mc = mc)
reshape(myDF,idvar="eid",direction="wide",v.names=c("fv","fc","mv","mc"),timevar="fac")
