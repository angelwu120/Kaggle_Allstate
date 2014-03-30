#!/usr/bin/Rscript

rm(list=ls())

# directories
baseDir='/Users/nicolegoebel/Dropbox/Courses/TIM209/2014/Kaggle_Allstate/'
dataDir=paste(baseDir,"data/", sep="")      #data dir
dataDirOut=paste(baseDir,"out/", sep="") 
dataDirFigs=paste(baseDir,"figs/", sep="")
codeDir=paste(baseDir,"Rcode/", sep="")
sourceFile=paste(baseDir,"Rcode/TIM209_2014_FINAL_Goebel.R", sep="")
setwd(codeDir)
#install.packages("chron")
#install.packages("mlogit")
#install.packages("foreign")
#install.packages("nnet")
#install.packages("ggplot2")
#install.packages("reshape2")
#library(chron)
library(randomForest)
library(mlogit)
library(foreign)
library(nnet)
library(ggplot2)
library(plyr)
require(MASS)
#require(Hmisc)
require(reshape2)
library(rpart)
library(data.table)
# source(sourceFile)
# source("/Users/nicolegoebel/Dropbox/Courses/TIM209/2014/Kaggle_Allstate/Rcode/TIM209_2014_FINAL_Goebel.R")

loadData <- function(fName, datName, dataDir){
	  # read in datadf
	  # fName = 'train.csv', datName="trainData"
	  # fName = 'test_V2.csv', datName="testData"
    fileName <- paste(dataDir,datName,".RData",sep="")
    if (file.exists(fileName)){    # if RData file already exists, load it
      load(fileName)
      cat("loaded Rdata files", "\n")
    }
    else {                         # if RData file does not exist, load and process original data
      cat("loading .csv files", "\n")
      df <- read.csv2(file = paste(dataDir,fName,sep=""), nrows=-1, sep=",", header=TRUE, na.strings=c("NA"," "))
      df$customer_ID <- as.character(df$customer_ID)     # customer_ID (id)
      df$time      <- as.POSIXct(df$time,format="%H:%M") # time (H:M)
      df$C_previous     <- as.factor(df$C_previous)      # previous option for product C (0=nothing, 1-4)
      df$location <- as.factor(df$A)                     # location
      df$A  <- as.factor(df$A)                           # coverage options A
      df$B  <- as.factor(df$B)                           # coverage options B
      df$C  <- as.factor(df$C)                           # coverage options C
      df$D  <- as.factor(df$D)                           # coverage options D
      df$E  <- as.factor(df$E)                           # coverage options E
      df$F  <- as.factor(df$F)                           # coverage options F
      df$G  <- as.factor(df$G)                           # coverage options G	 
      df$risk_factor <- as.factor(df$risk_factor)        # risk_factor	  
      # convert days to chars
      df$day <- factor(df$day, levels = c(0,1,2,3,4,5,6), labels = c("Mon", "Tue", "Wed", "Thu","Fri","Sat","Sun"))	  
      df$homeowner <- factor(df$homeowner, levels = c(0, 1), labels = c("No", "Yes"))
      df$married_couple <- factor(df$married_couple, levels = c(0, 1), labels = c("No", "Yes"))
      #ofile = paste(dataDirOut,datName,".csv", sep="")
      # find columns with missing values and replace with most frequent value
      miss <- colSums(is.na(df))==0
      vars <- names(which(miss==F))
      #missing <- names(df)[lapply(df, function(x)all(is.na(x)))]
      # replace NA's with most frequent value
      vars = c("risk_factor","duration_previous")
      for (var in vars){
        t <- table(df[,var])
        tt <- which(t==max(t))
        df[,var][which(is.na(df[,var])==TRUE)] <- tt[[1]]
      }
      # replace NAs for C_previous with C policy option 
      inds=which(is.na(df$C_previous))  # get indices where C_previous is missing
      for (ind in inds){
        df$C_previous[ind] <- df$C[ind]
      }
      #impute car_value by correlation with car_age
      # use car_age as an approximation for missing car_value 
      # aggregate car_age as a function of customer_ID and car_value
      tmpAgg2 <- aggregate(car_age~customer_ID +car_value,data=df, mean)
      # make a table for frequencies
      tb <- as.data.frame(table(tmpAgg2[,c("car_value","car_age")]))
      ages=unique(tb$car_age)     #get unique ages
      Vals<-c()
      # create a table from which to extract car_value based on car_age
      for (age in ages){
        tmpdf <- subset(tb, car_age==age)
        Vals <- rbind(Vals,c(age,as.character(tmpdf$car_value[which(tmpdf$Freq==max(tmpdf$Freq))])))
      }
      Vals=as.data.frame(Vals)
      names(Vals) <- c("car_age", "car_value")
      inds=which(df$car_value=="")  # get indices where car_value is missing
      # fill in missing values for car_value based on relationship to car_age (created above)
      for (ind in inds){
        tmpAge <- df$car_age[ind]
        tmpVal <- Vals$car_value[which(Vals$car_age==tmpAge)]
        df$car_value[ind] <- tmpVal
      }
    }	  
    # Average age
    df[,"age_average"] <- rowMeans(df[,c("age_youngest", "age_oldest")])
    df$output <- paste(df$A,df$B,df$C,df$D,df$E,df$F,df$G,sep="")   #vectorize policies in column called output
    #write.csv(df, ofile, row.names=FALSE)   # save generated dataset
    save(df, file=paste(dataDir,datName,".RData",sep=""))
    cat("Saved Rdata files", "\n")
    return(df)
}
# Exploratory Data Analysis
EDA <- function(trainData,testData, dataDirFigs){
  # exploratory data analysis
  vars <- c('day','state','group_size','car_age', 'car_value','risk_factor','age_oldest','age_youngest',
            'duration_previous','C_previous', 'A', 'B', 'C', 'D', 'E', 'F', 'G')
  varInds <- vector()
  for (i in 1:length(vars)){
    var = vars[i]
    print(var)
    varInds[i] <- which(names(trainData)==var)
    txt <- sprintf('%s/AllRecords_histcomp_%s.png',dataDirFigs, var)
    png(txt)
    #par(mfrow=c(1,1), oma = c(0, 0, 3, 0))
    par(mfrow=c(2,1))
    #mainText <- sprintf("Train Data: %s\n %g customers, %g purchases, %g transactions", var, 
    #                    length(unique(trainData$customer_ID)), sum(trainData$record_type), length(trainData$record_type))
    mainText <- sprintf("Train Data for record_type=1: %s", var)
    hist(as.numeric(trainData[,varInds[i]]), main=mainText)
    mainText <- sprintf("Test Data for record_type=1: %s", var)
    hist(as.numeric(testData[,varInds[i]]), main=mainText)
    #mtext(txt, outer = TRUE, cex=1.2)
    dev.off()
  }	
  # does every customer purchase?
  trainSUM <- sum(trainData$record_type)	
  trainLength <- length(unique(trainData$customer_ID))
  trainTransactions <- length(testData$record_type)
  testSUM <- sum(testData$record_type)
  testLength <- length(unique(testData$customer_ID))
  cat('Number of purchases in training set=',trainSUM,'VS number of customers=',trainLength, '\n')
  cat('Average number of transactions per purchase (in training set)=', trainTransactions/trainSUM, '\n')
  #cat('Number of purchases in test set=%g',testSUM,'VS number of customers=%g \n',testLength)
}

# Exploratory Data Analysis for training data only
EDAtrain <- function(trainData,dataDirFigs){
  # exploratory data analysis
  vars <- c('day','state','group_size','car_age', 'car_value','risk_factor','age_oldest','age_youngest',
            'duration_previous','C_previous', 'A', 'B', 'C', 'D', 'E', 'F', 'G')
  varInds <- vector()
  for (i in 1:length(vars)){
    var = vars[i]
    print(var)
    varInds[i] <- which(names(trainData)==var)
    txt <- sprintf('%s/OneRecords_histTrain_%s.png',dataDirFigs, var)
    png(txt)
    mainText <- sprintf("Train Data for record_type=1: %s", var)
    hist(as.numeric(trainData[,varInds[i]]), main=mainText)
    dev.off()
  }	
}

saveSubmission <- function(data, fname){
  options(stringsAsFactors=TRUE)
  write.csv(data, file=paste(dataDirOut,fname, ".csv", sep=""), quote=F, row.names=F)
  checkSub <- read.csv(file = paste(dataDirOut,fname, ".csv", sep=""),colClasses="character")
}

##  Models
logRegression <- function(trainData, testData, policy="A", features="car_risk"){
	# treat as 7 separate classification problems? For each policy, use a feature (or more)
  # Choose features for logistic regression---------------------------------------
  targPolicy <- which(names(trainData)==policy)
	targFeatures=which(names(trainData)==features[1])
	if (length(features) > 1){
		for (i in 2:length(features)){
			targFeatures <- cbind(targFeatures, which(names(trainData)==features[i]))
		}
	}
	
	#dfTrain <- as.data.frame(cbind(y,x))
  #-----------using multinom (must calculate p value separately)------------------:
  # choose the level of our outcome that we wish to use as our baseline and specify this in the relevel function
  #reflev <- as.factor(min(as.integer(unique(trainData[,policy]))))  #tried to automate reflev
  #reflev <- as.factor(unique(trainData[,policy])[1])
  trainData[,paste(policy,"2", sep="")] <- relevel(trainData[,policy], ref=1)
  #trainData$B2 <- relevel(trainData$B, ref=reflev)
  #trainData$B2 <- relevel(trainData$B, ref = "0")
  #y <- trainData[,paste(policy,"2", sep="")]
  #x <- trainData[,c(targFeatures)]
  #yx <- cbind(y,x)
  #mod.logr <- multinom(y~x, data=yx, family="binomial")
  mod.logr <- multinom(paste(policy,"2",sep="") ~ car_value + risk_factor, data = trainData)
  #mod.logr <- multinom(paste("trainData$", policy, "2", sep="") ~ trainData[,c(targFeatures)], data=trainData, family="binomial")
  summary(mod.logr)
  # Wald Test is used to calculate p-values.
  z <- summary(mod.logr)$coefficients/summary(mod.logr)$standard.errors
  # 2-tailed z test
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  ## extract the coefficients from the model and exponentiate
  exp(coef(mod.logr))
  # calculate predicted probabilities for each of our outcome levels using the fitted function. 
  # start by generating the predicted probabilities for the observations in our dataset and viewing the first few rows
  head(pp <- fitted(mod.logr))
  # examine the changes in predicted probability associated with one of our two variables
  # by creating small datasets varying one variable while holding the other constant. 
  # first do this holding one var at its mean and examining the predicted probabilities for each level of the other var
  dses <- data.frame(risk_factor = c("1", "2", "3", "4"), car_value = "a")
  predict(mod.logr, newdata = dses, "probs")
  #--------using mlogit()------------------------------------------------------------
  # 1. make sure y var is a factor
  trainData1 <- trainData
  reflev <- min(unique(trainData[,policy]))
  trainData1[,policy] <-as.factor(trainData1[,policy])
  trainData1 <- trainData1[,c(policy, features)]
  # 2. expand y like you would for dummy coding a categorical variable for normal regr
  trainData2 <- mlogit.data(trainData1, varying=NULL, choice=policy, shape="wide")
  # 3. Next proceed with model analysis using mlogit. Note reference category=0 or 1
  #model <- sprintf('mlogit(%s ~ %g | %s, data=trainData2, reflevel=%s, method = %s, 
  #                 print.level=0)',policy, reflev, features, reflevChar, meth)
  model <- mlogit(policy ~ reflev | features, data=trainData2, 
                  reflevel=reflevChar, method="nr", print.level=0)
  expCoef <- exp(coef(model))
  return(list(model=model, expCoef=expCoef))
  #---------- glm() for binomial log regression--------
  #glm.logr <- glm(as.factor(B) ~ car_age + car_value + risk_factor, family = binomial, data = trainDataSub)
  #summary(glm.logr)
  #---------- polr() eg ------------------------
  #mod.polr <- polr(formula = as.factor(A)~location+group_size+homeowner+car_age+car_value+risk_factor+age_oldest+age_youngest+married_couple+C_previous+duration_previous+cost, data=trainDataSub, Hess=TRUE)
  #lapply(dat[, c("A", "state", "group_size")], table)
  #return(list(model=model, p=p))
}

rpartModel <- function(policy, dfTrain, dfTest, Type="class"){
  browser()
  mod <- rpart(A ~., data=dfTrain, method = Type)
  pred  <- predict(policy, dfTest, type=Type)

  print(names(model))   #see what is in the model
  print(importance(model)) # calculate average variable importance
  txt <- sprintf('varImpt_Trees.png')
  png(txt)
  varImp=varImpPlot(model, main = "Average Importance Plots")
  #varImpPlot(model) # plot variable importance
  dev.off()
  #margins.rf=margin(model, trainDataTmp[,-c(targindex)])
  #png('marginsPlot.png')
  #hist(margins.rf,main="Margins of Random Forest")
  #dev.off()
  #png('boxPlot.png')
  #boxplot(margins.rf~trainDataTmp[,-c(targindex)], main="Margins of Random Forest for churn dataset by class")
  #dev.off()
  # The error rate over the trees is obtained as follows:
  # The multidimensional scaling plot is obtained as follows where k = number of dimensions.
  #png('MDSplot.png')
  #MDSplot(model, trainDataTmp[,-c(targindex)], k=1)
  #dev.off()
}
main <- function(){
	trainData <- loadData(fName = "train.csv", datName = "trainData", dataDir)
	testData  <- loadData(fName = "test_V2.csv", datName = "testData", dataDir)
	# get data only where there are purchases (record_type==1)
  trainDataSub <- subset(trainData, record_type==1 )
  # get maximum shopping point for trainDataSub and add as feature
  dt=as.data.table(trainData)
  tmpMSP<- as.data.frame(dt[, .I[shopping_pt== max(shopping_pt)], by = customer_ID], names=c("customer_ID","max_shopping_pt"))
  names(tmpMSP) <- c("customer_ID","max_shopping_pt")
  trainDataSub <- merge(trainDataSub, tmpMSP, by.x="customer_ID", by.y="customer_ID")
  # take last shopping point of test data set and rename "max_shopping_pt". 
  dt=as.data.table(testData)
  testDataSub <- dt[dt[, .I[shopping_pt== max(shopping_pt)], by = customer_ID]$V1]
	testDataSub <- rename(testDataSub, c("shopping_pt"="max_shopping_pt"))
  testDataSub <- as.data.frame(testDataSub)
  
	# put ages into groups to reduce number of levels
	# age_oldest
  breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)
	testDataSub[,"age_oldest_bin"] <- findInterval(testDataSub$age_oldest,breaks)
	trainDataSub[,"age_oldest_bin"] <- findInterval(trainDataSub$age_oldest,breaks)
	# age_yougest
	testDataSub[,"age_youngest_bin"] <- findInterval(testDataSub$age_youngest,breaks)
	trainDataSub[,"age_youngest_bin"] <- findInterval(trainDataSub$age_youngest,breaks)
	# age_mean
	testDataSub[,"age_average_bin"] <- findInterval(testDataSub$age_average,breaks)
	trainDataSub[,"age_average_bin"] <- findInterval(trainDataSub$age_average,breaks)
	# car_age
	testDataSub[,"car_age_bin"] <- findInterval(testDataSub$age_average,breaks)
	trainDataSub[,"car_age_bin"] <- findInterval(trainDataSub$age_average,breaks)
  #-----------------baselineSubmission------------------------------------
  #rpart.A <- rpart(A ~ risk_factor + car_value, data=trainDataSub,method="class")
  #predict.A <- predict(rpart.A, testDataSub, type="class")
	#rpart.B <- rpart(B ~ risk_factor + car_value, data=trainDataSub,method="class")
	#predict.B <- predict(rpart.B, testDataSub, type="class")
	#rpart.C <- rpart(C ~ risk_factor + car_value, data=trainDataSub,method="class")
	#predict.C <- predict(rpart.C, testDataSub, type="class")
	#rpart.D <- rpart(D ~ risk_factor + car_value, data=trainDataSub,method="class")
	#predict.D <- predict(rpart.D, testDataSub, type="class")
	#rpart.E <- rpart(E ~ risk_factor + car_value, data=trainDataSub,method="class")
	#predict.E <- predict(rpart.E, testDataSub, type="class")
	#rpart.F <- rpart(F ~ risk_factor + car_value, data=trainDataSub,method="class")
	#predict.F <- predict(rpart.F, testDataSub, type="class")
	#rpart.G <- rpart(G ~ risk_factor + car_value, data=trainDataSub,method="class")
	#predict.G <- predict(rpart.G, testDataSub, type="class")
	#rpart.combo <- as.data.frame(paste(predict.A, predict.B, predict.C, predict.D, predict.E, predict.E, predict.F, predict.G,sep="") )
	#finalSub <- as.data.frame(cbind(testDataSub$customer_ID, rpart.combo))
  #names(finalSub)<-c("customer_ID", "plan")
	#names(rpart.combo) <- c("plan")
  #fname="rpart.riskFactorCarValue"
  #saveSubmission(finalSub, fname)
  browser()
  #----------------------------submission with all variables (the binned versions) using rpart --------------
  ##dfTrain <- trainDataSub[,c("A", "day","location","state","group_size","homeowner", "car_age", "car_value", "risk_factor", "age_oldest", "age_youngest", "married_couple", "C_previous", "duration_previous", "age_average", "cost", "max_shopping_pt")]
	##dfTest <- testDataSub[,c("day","location","state","group_size","homeowner", "car_age", "car_value", "risk_factor", "age_oldest", "age_youngest", "married_couple", "C_previous", "duration_previous", "age_average", "cost", "max_shopping_pt")]
	##dfTrain <- trainDataSub[,c("A", "day","location","group_size","homeowner", "car_age", "car_value", "risk_factor", "age_oldest", "age_youngest", "married_couple", "C_previous", "duration_previous", "age_average", "cost", "max_shopping_pt")]
	##dfTest <- testDataSub[,c("day","location","group_size","homeowner", "car_age", "car_value", "risk_factor", "age_oldest", "age_youngest", "married_couple", "C_previous", "duration_previous", "age_average", "cost", "max_shopping_pt")]
	dfTest <- testDataSub[,c("day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "age_oldest_bin", "age_youngest_bin", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
	dfTrain <- trainDataSub[,c("A", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "age_oldest_bin", "age_youngest_bin", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
	mod.A <- rpart(A~., data=dfTrain, method = "class")
	dfTrain <- trainDataSub[,c("B", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "age_oldest_bin", "age_youngest_bin", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
  mod.B <- rpart(B~., data=dfTrain, method = "class")
	dfTrain <- trainDataSub[,c("C", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "age_oldest_bin", "age_youngest_bin", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
  mod.C <- rpart(C~., data=dfTrain, method = "class")
	dfTrain <- trainDataSub[,c("D", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "age_oldest_bin", "age_youngest_bin", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
  mod.D <- rpart(D~., data=dfTrain, method = "class")
	dfTrain <- trainDataSub[,c("E", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "age_oldest_bin", "age_youngest_bin", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
  mod.E <- rpart(E~., data=dfTrain, method = "class")
	dfTrain <- trainDataSub[,c("F", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "age_oldest_bin", "age_youngest_bin", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
  mod.F <- rpart(F~., data=dfTrain, method = "class")
	dfTrain <- trainDataSub[,c("G", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "age_oldest_bin", "age_youngest_bin", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
  mod.G <- rpart(G~., data=dfTrain, method = "class")
	pred.A  <- predict(mod.A, dfTest, type="class")
	pred.B  <- predict(mod.B, dfTest, type="class")
	pred.C  <- predict(mod.C, dfTest, type="class")
	pred.D  <- predict(mod.D, dfTest, type="class")
	pred.E  <- predict(mod.E, dfTest, type="class")
	pred.F  <- predict(mod.F, dfTest, type="class")
	pred.G  <- predict(mod.G, dfTest, type="class")
	submit <- as.data.frame(paste(pred.A, pred.B, pred.C, pred.D, pred.E, pred.E, pred.F, pred.G,sep="") )
  names(submit) <- c("plan")
  customer_ID <- testDataSub$customer_ID
  finalSub <- cbind(customer_ID, submit)
	fname="rpart.AllFeaturesReduced2"
	saveSubmission(finalSub, fname)
	#----------------------------submission with all variables (the binned versions) using randomForest ------
  numOfTrees=100  
  dfTest <- testDataSub[,c("day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "age_oldest_bin", "age_youngest_bin", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
	dfTrain <- trainDataSub[,c("A", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "age_oldest_bin", "age_youngest_bin", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]	
	mod.A <- randomForest(A~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
	dfTrain <- trainDataSub[,c("B", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "age_oldest_bin", "age_youngest_bin", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
	mod.B <- randomForest(B~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
	dfTrain <- trainDataSub[,c("C", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "age_oldest_bin", "age_youngest_bin", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
	mod.C <- randomForest(C~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
	dfTrain <- trainDataSub[,c("D", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "age_oldest_bin", "age_youngest_bin", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
	mod.D <- randomForest(D~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
	dfTrain <- trainDataSub[,c("E", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "age_oldest_bin", "age_youngest_bin", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
	mod.E <- randomForest(E~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
	dfTrain <- trainDataSub[,c("F", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "age_oldest_bin", "age_youngest_bin", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
	mod.F <- randomForest(F~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
	dfTrain <- trainDataSub[,c("G", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "age_oldest_bin", "age_youngest_bin", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
	mod.G <- randomForest(G~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
	pred.A  <- predict(mod.A, dfTest, type="class")
	pred.B  <- predict(mod.B, dfTest, type="class")
	pred.C  <- predict(mod.C, dfTest, type="class")
	pred.D  <- predict(mod.D, dfTest, type="class")
	pred.E  <- predict(mod.E, dfTest, type="class")
	pred.F  <- predict(mod.F, dfTest, type="class")
	pred.G  <- predict(mod.G, dfTest, type="class")
	submit <- as.data.frame(paste(pred.A, pred.B, pred.C, pred.D, pred.E, pred.E, pred.F, pred.G,sep="") )
	names(submit) <- c("plan")
	customer_ID <- testDataSub$customer_ID
	finalSub <- cbind(customer_ID, submit)
	fname <- sprintf('randomForest.AllFeaturesReduced_%gTrees', numOfTrees)
	saveSubmission(finalSub, fname)
  #print(names(model))   #see what is in the model
	#print(importance(model)) # calculate average variable importance
	txt <- sprintf('varImptA_%gTrees.png', numOfTrees)
	png(txt)
  par(mfrow=c(1,1))#, oma = c(0, 0, 3, 0))
	varImpA=varImpPlot(mod.A, main = "Policy A")
  dev.off()
	txt <- sprintf('varImptB_%gTrees.png', numOfTrees)
	png(txt)
	varImpB=varImpPlot(mod.B, main = "Policy B")
	dev.off()
	txt <- sprintf('varImptC_%gTrees.png', numOfTrees)
	png(txt)
	varImpC=varImpPlot(mod.C, main = "Policy C")
	dev.off()
	txt <- sprintf('varImptD_%gTrees.png', numOfTrees)
	png(txt)
	varImpD=varImpPlot(mod.D, main = "Policy D")
	dev.off()
	txt <- sprintf('varImptE_%gTrees.png', numOfTrees)
	png(txt)
	varImpE=varImpPlot(mod.E, main = "Policy E")
	dev.off()
	txt <- sprintf('varImptF_%gTrees.png', numOfTrees)
	png(txt)
	varImpF=varImpPlot(mod.F, main = "Policy F")
	dev.off()
	txt <- sprintf('varImptG_%gTrees.png', numOfTrees)
	png(txt)
	varImpG=varImpPlot(mod.G, main = "Policy G")
	dev.off()

  #-------Random Forest With Reduced Number of Features Based on Variable Importance --------
	numOfTrees=1000  
	dfTest <- testDataSub[,c("cost","location")]
	dfTrain <- trainDataSub[,c("A", "cost","location")]
	mod.A <- randomForest(A~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
	pred.A  <- predict(mod.A, dfTest, type="class")
	dfTest <- testDataSub[,c("cost","location","max_shopping_pt")]
	dfTrain <- trainDataSub[,c("B", "cost","location","max_shopping_point")]
	mod.B <- randomForest(B~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
	pred.B  <- predict(mod.B, dfTest, type="class")
	dfTest <- testDataSub[,c("C_previous", "location", "risk_factor", "cost")]
	dfTrain <- trainDataSub[,c("C", "C_previous","location", "risk_factor", "cost")]
	mod.C <- randomForest(C~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
	pred.C  <- predict(mod.C, dfTest, type="class")
	dfTest <- testDataSub[,c("C_previous","max_shopping_pt", "cost")]
	dfTrain <- trainDataSub[,c("D", "C_previous","max_shopping_pt","cost")]
  mod.D <- randomForest(D~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
	pred.D  <- predict(mod.D, dfTest, type="class")
	dfTest <- testDataSub[,c("cost","location","max_shopping_pt","C_previous", "risk_factor", "duration_previous")]
	dfTrain <- trainDataSub[,c("E", "cost","location","max_shopping_pt","C_previous", "risk_factor", "duration_previous")]
	mod.E <- randomForest(E~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
	pred.E  <- predict(mod.E, dfTest, type="class")
	dfTest <- testDataSub[,c("cost","location","max_shopping_pt","duration_previous", "C_previous", "risk_factor")]
	dfTrain <- trainDataSub[,c("F", "cost","max_shopping_pt","duration_previous","C_previous", "risk_factor")]
	mod.F <- randomForest(F~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
	pred.F  <- predict(mod.F, dfTest, type="class")
	dfTest <- testDataSub[,c("cost","max_shopping_pt","risk_factor","duration_previous","C_previous")]
	dfTrain <- trainDataSub[,c("G", "cost","max_shopping_pt","location","duration_previous","C_previous")]
	mod.G <- randomForest(G~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
	pred.G  <- predict(mod.G, dfTest, type="class")
	submit <- as.data.frame(paste(pred.A, pred.B, pred.C, pred.D, pred.E, pred.E, pred.F, pred.G,sep="") )
	names(submit) <- c("plan")
	customer_ID <- testDataSub$customer_ID
	finalSub <- cbind(customer_ID, submit)
	fname <- sprintf('randomForest.SelectFeaturesReduced_%gTrees', numOfTrees)
	saveSubmission(finalSub, fname)
	#print(names(model))   #see what is in the model
	#print(importance(model)) # calculate average variable importance
	txt <- sprintf('varImptA_%gTrees.png', numOfTrees)
	png(txt)
	par(mfrow=c(1,1))#, oma = c(0, 0, 3, 0))
	varImpA=varImpPlot(mod.A, main = "Policy A")
	dev.off()
	txt <- sprintf('varImptB_%gTrees.png', numOfTrees)
	png(txt)
	varImpB=varImpPlot(mod.B, main = "Policy B")
	dev.off()
	txt <- sprintf('varImptC_%gTrees.png', numOfTrees)
	png(txt)
	varImpC=varImpPlot(mod.C, main = "Policy C")
	dev.off()
	txt <- sprintf('varImptD_%gTrees.png', numOfTrees)
	png(txt)
	varImpD=varImpPlot(mod.D, main = "Policy D")
	dev.off()
	txt <- sprintf('varImptE_%gTrees.png', numOfTrees)
	png(txt)
	varImpE=varImpPlot(mod.E, main = "Policy E")
	dev.off()
	txt <- sprintf('varImptF_%gTrees.png', numOfTrees)
	png(txt)
	varImpF=varImpPlot(mod.F, main = "Policy F")
	dev.off()
	txt <- sprintf('varImptG_%gTrees.png', numOfTrees)
	png(txt)
	varImpG=varImpPlot(mod.G, main = "Policy G")
	dev.off()
	browser()
	#-------Gradient Boost Model -------------------------
	#numOfTrees=100
	#GBM_SHRINKAGE  =  0.05   
	#GBM_DEPTH  =  4
	#GBM_MINOBS  =  50dfTest <- testDataSub[,c("day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
  #dfTest <- testDataSub[,c("day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
  #dfTrainA <- trainDataSub[,c("A", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]  
  #dfTrainB <- trainDataSub[,c("B", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
  #dfTrainC <- trainDataSub[,c("C", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
  #dfTrainD <- trainDataSub[,c("D", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
  #dfTrainE <- trainDataSub[,c("E", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor",  "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
  #dfTrainF <- trainDataSub[,c("F", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor",  "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
  #dfTrainG <- trainDataSub[,c("G", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost", "max_shopping_pt")]
	#gbm.A=gbm(A~ ., distribution  ="multinomial", dfTrainA, n.trees = numOfTrees, shrinkage  =  GBM_SHRINKAGE, interaction.depth=GBM_DEPTH,n.minobsinnode  =  GBM_MINOBS,verbose  =  TRUE, keep.data=FALSE)
	#gbm.B=gbm(B~ ., distribution  ="multinomial", dfTrainB, n.trees = numOfTrees, shrinkage  =  GBM_SHRINKAGE, interaction.depth=GBM_DEPTH,n.minobsinnode  =  GBM_MINOBS,verbose  =  TRUE, keep.data=FALSE)
	#gbm.C=gbm(C~ ., distribution  ="multinomial", dfTrainC, n.trees = numOfTrees, shrinkage  =  GBM_SHRINKAGE, interaction.depth=GBM_DEPTH,n.minobsinnode  =  GBM_MINOBS,verbose  =  TRUE, keep.data=FALSE)
	#gbm.D=gbm(D~ ., distribution  ="multinomial", dfTrainD, n.trees = numOfTrees, shrinkage  =  GBM_SHRINKAGE, interaction.depth=GBM_DEPTH,n.minobsinnode  =  GBM_MINOBS,verbose  =  TRUE, keep.data=FALSE)
	#gbm.E=gbm(E~ ., distribution  ="multinomial", dfTrainE, n.trees = numOfTrees, shrinkage  =  GBM_SHRINKAGE, interaction.depth=GBM_DEPTH,n.minobsinnode  =  GBM_MINOBS,verbose  =  TRUE, keep.data=FALSE)
	#gbm.F=gbm(F~ ., distribution  ="multinomial", dfTrainF, n.trees = numOfTrees, shrinkage  =  GBM_SHRINKAGE, interaction.depth=GBM_DEPTH,n.minobsinnode  =  GBM_MINOBS,verbose  =  TRUE, keep.data=FALSE)
	#gbm.G=gbm(G~ ., distribution  ="multinomial", dfTrainG, n.trees = numOfTrees, shrinkage  =  GBM_SHRINKAGE, interaction.depth=GBM_DEPTH,n.minobsinnode  =  GBM_MINOBS,verbose  =  TRUE, keep.data=FALSE)

  #pred.A  <- predict(gbm.A, dfTest, n.trees=numOfTrees, type="link")
  #pred.B  <- predict(gbm.B, dfTest, n.trees=numOfTrees, type="response")
  #pred.C  <- predict(gbm.C, dfTest, n.trees=numOfTrees, type="response")
  #pred.D  <- predict(gbm.D, dfTest, n.trees=numOfTrees, type="response")
  #pred.E  <- predict(gbm.E, dfTest, n.trees=numOfTrees, type="response")
  #pred.F  <- predict(gbm.F, dfTest, n.trees=numOfTrees, type="response")
  #pred.G  <- predict(gbm.G, dfTest, n.trees=numOfTrees, type="response")
  #submit <- as.data.frame(paste(pred.A, pred.B, pred.C, pred.D, pred.E, pred.E, pred.F, pred.G,sep="") )
  #names(submit) <- c("plan")
  #customer_ID <- testDataSub$customer_ID
  #finalSub <- cbind(customer_ID, submit)
  #fname <- sprintf('GBM.SelectFeaturesReduced_%gTrees', numOfTrees)
  #saveSubmission(finalSub, fname)
	#gbm.fit seems to have a bug when mixing numeric and factor variables
	#list  variable  importance   
	gbmsum.A=summary(gbm.A,numOfTrees)
	gbmsum.B=summary(gbm.B,numOfTrees)
	gbmsum.C=summary(gbm.C,numOfTrees)
	gbmsum.D=summary(gbm.D,numOfTrees)
	gbmsum.E=summary(gbm.E,numOfTrees)
	gbmsum.F=summary(gbm.F,numOfTrees)
	gbmsum.G=summary(gbm.G,numOfTrees)
	#barplot(t(xx[,2]), names=t(xx[,1]), cex.names=0.24, main = paste("Allstate: variable importance\n", date()))
	#text(10, 10,paste(xx[,1],"=",round(xx[,2], 2), collapse="\n"), cex=0.75)  
  #----------- Another random forest based on variable of importance as determined by GBM --------
  numOfTrees=10000  
  dfTest <- testDataSub[,c("location", "cost")]
  dfTrain <- trainDataSub[,c("A", "location","cost")]
  mod.A <- randomForest(A~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
  pred.A  <- predict(mod.A, dfTest, type="class")
  dfTest <- testDataSub[,c("cost","location","car_age_bin", "C_previous", "duration_previous")]
  dfTrain <- trainDataSub[,c("B", "cost","location","car_age_bin", "C_previous", "duration_previous")]
  mod.B <- randomForest(B~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
  pred.B  <- predict(mod.B, dfTest, type="class")
  dfTest <- testDataSub[,c("C_previous", "location", "car_age_bin")]
  dfTrain <- trainDataSub[,c("C", "C_previous","location", "car_age_bin")]
  mod.C <- randomForest(C~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
  pred.C  <- predict(mod.C, dfTest, type="class")
  dfTest <- testDataSub[,c("C_previous","risk_factor", "location", "homeowner", "cost")]
  dfTrain <- trainDataSub[,c("D", "C_previous","risk_factor", "location", "homeowner", "cost")]
  mod.D <- randomForest(D~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
  pred.D  <- predict(mod.D, dfTest, type="class")
  dfTest <- testDataSub[,c("cost","location","car_age_bin","C_previous", "risk_factor")]
  dfTrain <- trainDataSub[,c("E", "cost","location","car_age_bin","C_previous", "risk_factor")]
  mod.E <- randomForest(E~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
  pred.E  <- predict(mod.E, dfTest, type="class")
  dfTest <- testDataSub[,c("cost","location","duration_previous", "C_previous", "risk_factor")]
  dfTrain <- trainDataSub[,c("F", "cost","location","duration_previous","C_previous", "risk_factor")]
  mod.F <- randomForest(F~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
  pred.F  <- predict(mod.F, dfTest, type="class")
  dfTest <- testDataSub[,c("cost","risk_factor","duration_previous","C_previous", "location", "car_age_bin")]
  dfTrain <- trainDataSub[,c("G","cost","risk_factor","duration_previous","C_previous", "location", "car_age_bin")]
  mod.G <- randomForest(G~., data=dfTrain, importance=TRUE, ntrees=numOfTrees)
  pred.G  <- predict(mod.G, dfTest, type="class")
  submit <- as.data.frame(paste(pred.A, pred.B, pred.C, pred.D, pred.E, pred.E, pred.F, pred.G,sep="") )
  names(submit) <- c("plan")
  customer_ID <- testDataSub$customer_ID
  finalSub <- cbind(customer_ID, submit)
  fname <- sprintf('randomForest.SelectFeaturesGBM_%gTrees', numOfTrees)
  saveSubmission(finalSub, fname)

  txt <- sprintf('varImptAgbm_%gTrees.png', numOfTrees)
  png(txt)
  par(mfrow=c(1,1))#, oma = c(0, 0, 3, 0))
  varImpA=varImpPlot(mod.A, main = "Policy A")
  dev.off()
  txt <- sprintf('varImptBgbm_%gTrees.png', numOfTrees)
  png(txt)
  varImpB=varImpPlot(mod.B, main = "Policy B")
  dev.off()
  txt <- sprintf('varImptCgbm_%gTrees.png', numOfTrees)
  png(txt)
  varImpC=varImpPlot(mod.C, main = "Policy C")
  dev.off()
  txt <- sprintf('varImptDgbm_%gTrees.png', numOfTrees)
  png(txt)
  varImpD=varImpPlot(mod.D, main = "Policy D")
  dev.off()
  txt <- sprintf('varImptEgbm_%gTrees.png', numOfTrees)
  png(txt)
  varImpE=varImpPlot(mod.E, main = "Policy E")
  dev.off()
  txt <- sprintf('varImptFgbm_%gTrees.png', numOfTrees)
  png(txt)
  varImpF=varImpPlot(mod.F, main = "Policy F")
  dev.off()
  txt <- sprintf('varImptGgbm_%gTrees.png', numOfTrees)
  png(txt)
  varImpG=varImpPlot(mod.G, main = "Policy G")
  dev.off()
  #--------- attempt at multinomial regression----------------------------------------------
	dfTest <- testDataSub[,c("day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost")]
	dfTrain <- trainDataSub[,c("A", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost")]	
  dfTrain$A2 <- relevel(dfTrain[,"A"], ref=1, Hess=T) 
  mod.A <- multinom(A2 ~ ., data = dfTrain, Hess=T)
	dfTrain <- trainDataSub[,c("B", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost")]	
	dfTrain$B2 <- relevel(dfTrain[,"B"], ref=1, Hess=T) 
	mod.B <- multinom(B2 ~ ., data = dfTrain)
	dfTrain <- trainDataSub[,c("C", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost")]	
	dfTrain$C2 <- relevel(dfTrain[,"C"], ref=1, Hess=T) 
	mod.C <- multinom(C2 ~ ., data = dfTrain)
	dfTrain <- trainDataSub[,c("D", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost")]	
	dfTrain$D2 <- relevel(dfTrain[,"D"], ref=1, Hess=T) 
	mod.D <- multinom(D2 ~ ., data = dfTrain)
	dfTrain <- trainDataSub[,c("E", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost")]	
	dfTrain$E2 <- relevel(dfTrain[,"E"], ref=1, Hess=T) 
	mod.E <- multinom(E2 ~ ., data = dfTrain, Hess=T)
	dfTrain <- trainDataSub[,c("F", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost")]	
	dfTrain$F2 <- relevel(dfTrain[,"F"], ref=1, Hess=T) 
	mod.F <- multinom(F2 ~ ., data = dfTrain, Hess=T)
	dfTrain <- trainDataSub[,c("G", "day","location","group_size","homeowner", "car_age_bin", "car_value", "risk_factor", "married_couple", "C_previous", "duration_previous", "age_average_bin", "cost")]	
	dfTrain$G2 <- relevel(dfTrain[,"G"], ref=1, Hess=T) 
	mod.G <- multinom(G2 ~ ., data = dfTrain)
  
  pred.A<-predict(mod.A, newdata = all(names(dfTrain)[-1] %in% names(dfTest)), "class")
  pred.B<-predict(mod.B, newdata = dfTest, "class")
  pred.C<-predict(mod.C, newdata = dfTest, "class")
  pred.D<-predict(mod.D, newdata = dfTest, "class")
  pred.E<-predict(mod.E, newdata = dfTest, "class")
  pred.F<-predict(mod.F, newdata = dfTest, "class")
  pred.G<-predict(mod.G, newdata = dfTest, "class")
	submit <- as.data.frame(paste(pred.A, pred.B, pred.C, pred.D, pred.E, pred.E, pred.F, pred.G,sep="") )
	pred.A<-names(submit) <- c("plan")
	customer_ID <- testDataSub$customer_ID
	finalSub <- cbind(customer_ID, submit)
	fname <- sprintf('randomForest.SelectFeaturesGBM_%gTrees', numOfTrees)
	saveSubmission(finalSub, fname)
  summary(mod.logr)
  # Wald Test is used to calculate p-values.
  z <- summary(mod.logr)$coefficients/summary(mod.logr)$standard.errors
  # 2-tailed z test
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  ## extract the coefficients from the model and exponentiate
  exp(coef(mod.logr))
  # calculate predicted probabilities for each of our outcome levels using the fitted function. 
  # start by generating the predicted probabilities for the observations in our dataset and viewing the first few rows
  head(pp <- fitted(mod.logr))
  # examine the changes in predicted probability associated with one of our two variables
  # by creating small datasets varying one variable while holding the other constant. 
  # first do this holding one var at its mean and examining the predicted probabilities for each level of the other var
  dses <- data.frame(risk_factor = c("1", "2", "3", "4"), car_value = "a")
  predict(mod.logr, newdata = dses, "probs")
}
#head(trainData)
#head(testData)