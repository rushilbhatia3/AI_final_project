library(rpart)								#include the rpart library

set.seed(as.integer(Sys.time()))					#set the seed for a random generator using the current time
indata <- read.csv("tt4-8.csv", header=FALSE)			#read the input file into indata
bacName <- indata[0:1,]
numElems <-nrow(indata)
data <- indata[-c(1]

rand <- sample(nrow(data))						#generate a random order for indata's rows
data <- data[rand,]							    #scramble indata based on this random order
classData <- data[1]							    #store a mapping between classifications and original line numbers or row names(classData is a data frame)

minimumSplit <- 25							        #set the minSplit to be 25
minimumBucket <- minimumSplit/3					#minBucket will be 3

k <- 10									       #use 10 folds for validation
numElems <-as.integer(nrow(data)/k)		 #find the size of each fold
print(numElems)
valid <- 0								     #initialize valid to be 0

print("--- K-FOLD Testing ---------------------------------------")
for(i in 2:k)	#loop once for each fold
{
  print("----------------------------------------")
  msg <- paste(i, "-FOLD", sep="")
  print(msg)
  
  testData <- data[(i*numElems-numElems+1):(i*numElems), ]	#pull out the test fold
  modelData <- data[1:(i*numElems-numElems+1) , ]		#pull out everything before the test data for the model
  temp <- data[(i*numElems):nrow(data), ]			#pull out everything after the test data
  modelData <- rbind(modelData, temp)				#     and put it in the model
  print(modelData)
  print("creating model now: ")
  #create the model using modelData
  fit <- rpart(V1~., method="class", data=modelData ,control=rpart.control(minsplit = minimumSplit, minBucket = minimumBucket))#surrogatestyle=0,maxsurrogate=0
  print("model done")
  print(fit)
  plot(fit, uniform=TRUE, main="CART Data", margin=0.2)	#plot the model
  text(fit, use.n=TRUE, all=TRUE, cex=.8)				                        #make it look a bit nicer
  #post(fit, file="tree.ps", title="CART for Federalist Data")	          #output a file of the plot
  
  preds <- predict(fit, testData)					#rpredict classifications for the test data
  predFrame <- as.data.frame(preds)				#store it in a data frame
  predCols <- colnames(predFrame)					#get the possible predicted values
  
  corCount   <- 0							
  wrongCount <- 0						#
  
  for(j in 1:nrow(predFrame))					#loop over each prediction
  {
    rowNum <- row.names(predFrame[j, ])			#find the row name
    maxCol <- which.max(predFrame[j,])			#find which value the instance was predicted to be
    
    print(classData[rowNum,])
    print(predCols[maxCol])
    #readline()
    
    if(classData[rowNum,] == predCols[maxCol])		#if the predicted value matches the actual classification
    {
      corCount <- corCount + 1				#update the correct count
    }
    else
    {
      wrongCount <- wrongCount + 1
      msg <- paste("    WRONG(actual:pred) - [", rowNum, classData[rowNum,],"] - ", predCols[maxCol], sep=" ")
      print(msg)
    }
    
    
  } # END for each test in this fold
  
  print(sprintf("Number correct is: %2d", corCount)); 
  print(sprintf("Number wrong is: %2d",   wrongCount)); 
  corCount <- corCount / (corCount+wrongCount)			#calculate the average correct for this fold
  print(sprintf("Percent correct is %3.2f", corCount*100)); 
  print("------------------------------------------------")
  
  #readline("Pause...")
  
  valid <- valid + corCount					#add it to valid
} # END for each fold


valid <- valid / k			#calculate the average correctness of all folds
msg <- paste("OVERALL correctness rate: ", valid*100, "%", sep="")
print(msg)				    


