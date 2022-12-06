library(rpart)								#include the rpart library
library(dplyr) 
set.seed(as.integer(Sys.time()))					#set the seed for a random generator using the current time
indata <- read.csv("tableData.csv", header=FALSE)			#read the input file into indata

data<-indata[-1,]
print(indata)
print("what")
classData <- indata[c(0:1)]	#store a mapping between classifications and original line numbers or row names(classData is a data frame)

print(data)
print("classData")
print(classData)
rand <- sample(nrow(data))						#generate a random order for indata's rows
data <- data[rand,]							    #scramble indata based on this random order
#print(data)						    

minimumSplit <- 3							        #set the minSplit to be 25
minimumBucket <- minimumSplit/3					#minBucket will be 3

k <- 10
#use 10 folds for validation
numElems <-(nrow(data)/k)		 #find the size of each fold  
print(numElems)
valid <- 0								     #initialize valid to be 0

print("--- K-FOLD Testing ---------------------------------------")
for(i in 1:k)	#loop once for each fold
{

  print("----------------------------------------")
  msg <- paste(i, "-FOLD", sep="")
  print(msg)
  
  #print(sprintf("data start point: %d - %d",i*numElems-numElems+1, i*numElems))
  
  testData <- data[(i*numElems-numElems+2):(i*numElems), ]	#pull out the test fold
  #print(testData)
  modelData <- data[1:(i*numElems-numElems+2) , ]		#pull out everything before the test data for the model
  temp <- data[(i*numElems):nrow(data), ]			#pull out everything after the test data
  mData <- rbind(modelData, temp)				#     and put it in the model
  mData%>% filter(if_all(everything(), ~ !is.na(.x)))
  #print("modelData")
  #print(modelData)
  #modelData<- na.omit(modelData)
  print("creating model now: ")
  #create the model using modelData
  fit <- rpart(V1~., method="class", data=mData ,control=rpart.control(minsplit = minimumSplit, minBucket = minimumBucket, maxcompete = 4))#,surrogatestyle=0,maxsurrogate=0
  print("model done")
  #print(fit)
  #summary(fit)
  
  plot(fit, uniform=TRUE, main="CART Data")	#plot the model margin=0.15
  text(fit, use.n=TRUE, all=TRUE, cex=0.8)				                        #make it look a bit nicer
  post(fit, file="tree.ps", title="CART Data")	          #output a file of the plot
  
  testData<-na.omit(testData)
  print(nrow(testData))
  preds <- predict(fit, newdata = testData)					#predict classifications for the test data
  predFrame <- as.data.frame(preds)				#store it in a data frame
  predCols <- colnames(predFrame)					#get the possible predicted values
  #predFrame[0,] <-0
  corCount   <- 0							
  wrongCount <- 0						#
  
  for(j in 1:nrow(predFrame))					#loop over each prediction
  {
    print("pred frame incoming")
    print(predFrame[j,])
    print("number")
    print(j)
    #print("checking")
    rowNum <- row.names(predFrame[j, ])			#find the row name
    maxCol <- which.max(predFrame[j,])			#find which value the instance was predicted to be
    
   #print(sprintf(" being compared:  classdata[rownum]: %s", classData[rowNum,]))
    #print(sprintf("predCols[maxCol]: %s", predCols[maxCol]))
    #readline()
    
    if(classData[rowNum,] == predCols[maxCol])		#if the predicted value matches the actual classification
    {
      corCount <- corCount + 1				#update the correct count
      msg <- paste("!!!!  RIGHT(actual:pred) - [", classData[rowNum,], predCols[maxCol],"] - ", predCols[maxCol],"  rightcount: ",corCount, sep=" ")
      print(msg)
    }
    else
    {
      wrongCount <- wrongCount + 1
      msg <- paste("XXXXX    WRONG(actual:pred) - [", classData[rowNum,], predCols[maxCol],"] - ", predCols[maxCol],"  wrongcount: ",wrongCount, sep=" ")
      print(msg)
    }
    
    
  } # END for each test in this fold
  
  print(sprintf("Number correct is: %2d", corCount)); 
  print(sprintf("Number wrong is: %2d",   wrongCount)); 
  corCount <- corCount / (corCount+wrongCount)			#calculate the average correct for this fold
  print(sprintf("Percent correct is %3.4f", corCount*100)); 
  print("------------------------------------------------")
  
  #readline("Pause...")
  
  valid <- valid + corCount					#add it to valid
} # END for each fold

print(sprintf("Number correct is: %2d", corCount)) 
print(sprintf("Number wrong is: %2d",   wrongCount))
valid <- valid / k			#calculate the average correctness of all folds
msg <- paste("OVERALL correctness rate: ", valid*100, "%", sep="")
print(msg)				    


