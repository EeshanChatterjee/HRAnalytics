col1Check = regexpr("^MuSigma",xx[,1])
col2Check = regexpr("^MuSigma",xx[,2])

C1VI = which(col1Check<0)
C2VI = which(col2Check<0)

# nValIndex = union(C1VI,C2VI)
# xx = xx[-nValIndex,]

col4Check = regexpr("^MuSigma",xx[,4])
C4VI = which(col4Check<0)
col5Check = regexpr("^MuSigma",xx[,5])
C5VI = which(col5Check<0)

# For col 6, get the 1st word
flow = sapply(1:dim(xx)[1], FUN = function(i) gsub("[[:punct:]]"," ",as.character(xx[i,6])))
flow = sapply(flow,FUN=function(i) tolower(strsplit(as.character(i),split=c(" "))[[1]][1]))
flowValidIndex6 = c("control","arousal","flow","anxiety","apathy","boredom","worry","unexcited")
C6VI = which(!flow%in%flowValidIndex6)

# For col 8, get the 1st word
flow = sapply(1:dim(xx)[1], FUN = function(i) gsub("[[:punct:]]"," ",as.character(xx[i,8])))
flow = sapply(flow,FUN=function(i) tolower(strsplit(as.character(i),split=c(" "))[[1]][1]))
flowValidIndex8 = c("control","arousal","flow","anxiety","apathy","boredom","worry","unexcited")
C8VI = which(!flow%in%flowValidIndex8)

# nValIndex = union(C6VI,C8VI)
# xx = xx[-nValIndex,]

# For col 10, only 3 dims possible
dimValidVec = c("Creating Value for the Customer","Building the Mu Sigma Ecosystem","Manages Self and Relationships")
C10VI = which(!xx[,10]%in%dimValidVec)
# xx = xx[-C10VI,]

# For col 15
resValVec = c("Demonstrates Independently","Demonstrates with Guidance","Does not Demonstrate","No Opportunity Presented")
C15VI = which(!xx[,15]%in%resValVec)

# For col 17
resValVec = c("Demonstrates Independently","Demonstrates with Guidance","Does not Demonstrate","No Opportunity Presented")
C17VI = which(!xx[,17]%in%resValVec)

# nValIndex = union(C15VI,C17VI)
# xx = xx[-nValIndex,]

nValIndex = union(union(union(union(union(union(union(union(C1VI,C2VI),C4VI),C5VI),C6VI),C8VI),C10VI),C15VI),C17VI)
eids = xx[nValIndex,4]
uEids = unique(eids)
dropList = which(xx[,4]%in%uEids)
xx = xx[-dropList,]
