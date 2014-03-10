# Variables

load("./formattedFeedback_cat.RData")

# Number of employees
numEmployees = dim(formattedFeedback_cat)[1] # <Needs to be dynamic>
# Number of factors
numFactors = (dim(formattedFeedback_cat)[2]-4)/2 
# Number of flows
numFlows = length(unique(formattedFeedback_cat[,3]))
# Number of Dimentions
numDims = 3
# Table of probabilities of choices for each factor
FactorProbabilities = data.frame(Factor = rep(NA,numFactors), prob0 = rep(NA,numFactors), prob1 = rep(NA,numFactors),  prob2 = rep(NA,numFactors), prob3 = rep(NA,numFactors))
# Table of probabilities of choices for each flow
FlowProbabilities = data.frame(Flow = rep(NA,numFlows), Prob = rep(NA,numFlows))
# Global Response levels
respLevels = as.numeric(levels(factor(unlist(formattedFeedback_cat[,5:dim(formattedFeedback_cat)[2]]))))
# Global Flow levels
flowLevels = as.numeric(levels(factor(unlist(formattedFeedback_cat[,3:4]))))
# Global Indexes and Dimention-Factor Maps
EfactorColIndex = which(regexpr("ResE",colnames(formattedFeedback_cat))>0)
EflowColIndex = which(regexpr("FlowE",colnames(formattedFeedback_cat))>0)
MrespIndex = which(regexpr("ResM",colnames(formattedFeedback_cat))>0)
dim1ColIndex = c(1:12,32)
dim2ColIndex = c(13:24,33)
dim3ColIndex = c(25:31,34)

EfactorColIndex = which(regexpr("ResE",colnames(formattedFeedback_cat))>0)
EflowColIndex = which(regexpr("FlowE",colnames(formattedFeedback_cat))>0)

#====================================================================
#------------------------------------------------------------------------------------------
# For Employees' Flow Probability given Generating Set
#------------------------------------------------------------------------------------------
# Function to calculate probabilities for chioces within factors
# And populate the Factor probability table
calcFactorProbabilities = function(){
  
  # Function to return the probabilities of individual choices in a Factor
  returnProbfromFac = function(colIndex){
    vec = as.vector(formattedFeedback_cat[,colIndex])
    len = length(vec)
    levels = as.numeric(levels(factor(vec)))
    probVec = sapply(respLevels, function(x) length(which(vec==x))/len)
    probVec = c(colnames(formattedFeedback_cat)[colIndex],probVec)
    return(probVec)
  }
  
  # Function to return the Flow Probabilities
  returnProbfromFlow = function(colIndex){
    vec = as.numeric(formattedFeedback_cat[,colIndex])
    len = length(vec)
    probVec = sapply(flowLevels, function(x) length(which(vec==x))/len)
    return(probVec)
  }
  
  # Function to add data to the factor probability table
  addFactorData = function(facVec){
    facVec = unlist(facVec)
    index = length(which(!is.na(FactorProbabilities[,1]))) + 1
    FactorProbabilities[index,] <<- facVec
  }
  
  # Function to add data to the flow probability table
  addFlowData = function(flowVec){
    flowProb = data.frame(Flow = flowLevels,Prob = flowVec)
    FlowProbabilities <<- flowProb
  }
  #------------------------------------------------------
  # Populating DFs
  #------------------------------------------------------
  # Call functions to calculate the factor probabilities and populate the dataframe.
  FacProbVec = lapply(EfactorColIndex,returnProbfromFac)
  sapply(FacProbVec,addFactorData)
  
  FlowProbVec = returnProbfromFlow(EflowColIndex)
  addFlowData(FlowProbVec)
  
}

calcFactorProbabilities()


# Function to calculate flow probability given a generating set for an Employee
calcFlowProbGivenGenSet = function(flow, choiceVec){
      #-------------------------------------------------------------------
      # Function for using only Dim Buckets.
      #-------------------------------------------------------------------
      # Function to get probability of a generating set given a flow
    calcDimsetProbGivenFlow = function(flow,choiceVec){
      setOccurances = which(apply(dimBuckets,1,FUN=function(x) length(which(x == choiceVec))== numDims))
      flowVec = which(formattedFeedback_cat$FlowE == flow)
      flowCount = length(flowVec)
      setWithFlowVec = intersect(setOccurances,flowVec)
      setWithFlowCount = length(setWithFlowVec)
      if(setWithFlowCount==0){
        setProbGivenFlow = 0
      } else { 
        setProbGivenFlow = setWithFlowCount/flowCount
      }
      return(setProbGivenFlow)
    }
  
    calcSetProbSum = function(choiceVec){
      SetProbSum = sum(sapply(c(1:numFlows)-1,calcDimsetProbGivenFlow,choiceVec))
      return(SetProbSum)
    }
 
    dimsetProbGivenFlow = calcDimsetProbGivenFlow(flow, choiceVec)
    setProb = calcSetProbSum(choiceVec)

    flowProbGivenSet = (dimsetProbGivenFlow)/setProb

return(flowProbGivenSet)
}

#-------------------------------------------------------------------
# Calculating Dimention Buckets for each employee
#-------------------------------------------------------------------

calcDimBuckets = function(x){
  empResD1 = mean(as.numeric(formattedFeedback_cat[x,EfactorColIndex[dim1ColIndex]]))
  empResD2 = mean(as.numeric(formattedFeedback_cat[x,EfactorColIndex[dim2ColIndex]]))
  empResD3 = mean(as.numeric(formattedFeedback_cat[x,EfactorColIndex[dim3ColIndex]]))
  
  giveBucket = function(x){
    if(x>0.85){
      buc = 1
    } else if(x>0.7125){
      buc = 2
    } else {
      buc = 3
    }
    return(buc)
  }
  
  b.D1 = giveBucket(empResD1)
  b.D2 = giveBucket(empResD2)
  b.D3 = giveBucket(empResD3)
  
  return(c(b.D1,b.D2,b.D3))
#   return(c(empResD1,empResD2,empResD3))
}

dimBuckets = t(as.matrix(sapply(1:numEmployees, calcDimBuckets)))
colnames(dimBuckets) = c("b.D1","b.D2","b.D3")
formattedFeedback_cat = cbind(formattedFeedback_cat,dimBuckets)
#-------------------------------------------------------------------

#-------------------------------------------------------------------
# Calculating Flow Probability given Dim Bucket Set for each employee
#-------------------------------------------------------------------

giveFlowProb = function(x){
  return(calcFlowProbGivenGenSet(formattedFeedback_cat[x,EflowColIndex],formattedFeedback_cat[x,73:75]))
}
EmpFlowProbVec = sapply(1:numEmployees,giveFlowProb)

EmpFlowProbdiv = 1-EmpFlowProbVec

EmpFlowProbDiv = data.frame(Manager = formattedFeedback_cat[,2],Employee = formattedFeedback_cat[,1],FlowProb=EmpFlowProbdiv)
#--------------------------------------------------------------------
#====================================================================


#====================================================================
#--------------------------------------------------------------------
# Employee- Manager Divergence
#--------------------------------------------------------------------

EChoiceColIndex = which(regexpr("ResE",colnames(formattedFeedback_cat))>0)
MChoiceColIndex = which(regexpr("ResM",colnames(formattedFeedback_cat))>0)

cohensKappa <-function(A)
{
  require(micEcon)
  require(R.utils)
  dimnames(A) = list(seq(1:dim(A)[1]),c("emp","mgr"))
  crosstab = xtabs(~emp+mgr, data = A,drop.unused.levels = FALSE)
  crosstab = as.matrix(as.data.frame.matrix(crosstab))
  
  if(dim(crosstab)[1]!=dim(crosstab)[2]){
    maxdim = max(dim(crosstab)[1],dim(crosstab)[2])
    probdim = which(dim(crosstab)<maxdim)
    dimdiff = maxdim - dim(crosstab)[probdim]
    
    for(i in 1:dimdiff){
      if(probdim == 1){
        temp = c(rownames(crosstab),"NA")
        crosstab = rbind(crosstab,c(rep(NA,dim(crosstab)[2])))
        rownames(crosstab) = temp
      } else {
        temp = c(colnames(crosstab),"NA")
        crosstab = cbind(crosstab,c(rep(NA,dim(crosstab)[1])))
        colnames(crosstab) = temp
      }
    }
    
    for(i in 1:maxdim){
      if(rownames(crosstab)[i]!=colnames(crosstab)[i]){
        if(probdim == 1){
          temp = insert(rownames(crosstab), i, values=colnames(crosstab)[i])
          crosstab = insertRow( crosstab, i, v = c(rep(0)) )
          rownames(crosstab) = temp
        } else {
          temp = insert(colnames(crosstab), i, values=rownames(crosstab)[i])
          crosstab = insertCol( crosstab, i, v = c(rep(0)) )
          colnames(crosstab) = temp
        }
      }
      
    }
    
    rows = dim(crosstab)[1]
    cols = dim(crosstab)[2]
    if(rows>maxdim){
      crosstab = crosstab[-c((maxdim+1):rows),]
    }
    if(cols>maxdim){
      crosstab = crosstab[,-c((maxdim+1):cols)]
    }
    
  }
  
  N = dim(A)[1]
  p_mgr = colSums(crosstab)/N
  p_emp = rowSums(crosstab)/N
#   p_e = p_mgr[1]*p_emp[1] + p_mgr[2]*p_emp[2]
#   p_a = (crosstab[1,1] + crosstab[2,2])/N
  p_e = p_mgr%*%p_emp
  p_a = sum(diag(crosstab))/N
  return((p_a-p_e)/(1-p_e))
}

calcCohensKappa = function(i){ 
empManMat = matrix(c(as.numeric(formattedFeedback_cat[i,EChoiceColIndex]),as.numeric(formattedFeedback_cat[i,MChoiceColIndex])),ncol=2)
kappa = cohensKappa(empManMat)
print(i)
return(kappa)
}
  
kappascores = sapply(1:numEmployees,calcCohensKappa)
kappaDiv = 1-kappascores
empManDiv = data.frame(Manager = formattedFeedback_cat[,2],Employee = formattedFeedback_cat[,1],Divergence=kappaDiv)

#--------------------------------------------------------------------
#====================================================================

#--------------------------------------------------------------------
# JSON creation function
#--------------------------------------------------------------------

createScatterplotJSON = function(df){
  
  # Function to create the employee detail list
  getEmpDet = function(i){
    emp = list()
    emp$eno = as.vector(df[i,2])
    emp$val = as.numeric(df[i,3])
    return(emp)
  }
  
  # Function to create list of employees under a manager
  getEmpByMan = function(M){
    manIndex = which(df[,1]==M)
    manList = lapply(manIndex,getEmpDet)
    manDet = list(Manager_id = M,Employees=manList)
    return(manDet)
  }
  
  managers = as.character(unique(df[,1]))
  plotList = lapply(managers,getEmpByMan)
  plotJson = toJSON(plotList)
  return(plotJson)
}

barchartJSON<-function(sent){

  # Function to create the employee detail list
  getEmpDet = function(i){
    emp = list()
    emp$eno = as.vector(sent[i,2])
    emp$value = as.numeric(sent[i,3])
    emp$Manager_id = as.vector(sent[i,1])
    return(emp)
  }
  numEmp = length(unique(sent[,2]))
  barlist = lapply(1:numEmp, getEmpDet)
  barJSON = toJSON(barlist)
  return(barJSON)
}

TreemapJSON<-function(df){

  # Function to create the employee detail list
  getEmpDet = function(i){
    emp = list()
    emp$name = as.vector(df[i,2])
    emp$sent_value_employee = as.numeric(df[i,3])
    emp$sent_value_manager = as.numeric(df[i,4])
    return(emp)
  }
  # Function to create list of employees under a manager
  getEmpByMan = function(M){
    manIndex = which(df[,1]==M)
    manList = lapply(manIndex,getEmpDet)
    manDet = list(name = M,children=manList)
    return(manDet)
  }
  managers = as.character(unique(df[,1]))
  managerList = lapply(managers,getEmpByMan)
  treemapList = list(name = "Head", children = managerList)
  json<-toJSON(treemapList)
  return(json)
}


#--------------------------------------------------------------------
# Create JSONS
#--------------------------------------------------------------------

load("./sentData.RData")
sentData = data[,c(2,1,3,4)]
rm(data)
df = sentData
mans = unique(sentData[,1])
expandedMans = sapply(1:length(mans), FUN=function(x) paste(c("Manager",x),collapse=""))
df[,1] = sapply(1:dim(df)[1], FUN=function(x) expandedMans[which(mans==df[x,1])])


flowJson = createScatterplotJSON(EmpFlowProbDiv)
kappaJson = createScatterplotJSON(empManDiv)
diffChJson = createScatterplotJSON(sentData)
barJSON = barchartJSON(sentData)
treemapJSON = TreemapJSON(df)

write(flowJson,file="flowJson.txt")
write(kappaJson,file="kappaJson.txt")
write(diffChJson,file="DiffJson.txt")
write(barJSON,file="barchartJSON.txt")
write(treemapJSON,file="treemapJSON.txt")




con = file("./scatterplot.json")
open(con)
json = readLines(con,n=-1L)
close(con)

xx = fromJSON(json)

