rm(list=ls())

### the main function used in the parallel computation 
Mainfun<-function(j, alldata, allparticipants, allcombinations){
  
  ### import all needed packages
  library(randomForest)  
  library(ROCR)
  library(bestglm)
  library(gtools)
  library(bst)
  library(caret)
  library(pROC)
  library(plyr)
  library(ipred)
  library(e1071)
  library(kernlab)
  library(klaR)
  library(MASS)
  library(C50)
  library(glmnet)
  
  #Best subset selection
  scaleContinuous <- function(data) {
    binary = apply(data, 2, function(x) {all(x %in% 0:1)}) 
    data[!binary] = scale(data[!binary])
    return(data)
  }
  
  test.index <- which(alldata$subject %in% allcombinations[j,])
  train.index <- which(!(alldata$subject %in% allcombinations[j,]))
  
  X <- scaleContinuous(alldata[train.index,-seq(1,6)])
  y <- alldata[train.index, 4]
  Xy <- cbind(X, fatigue.level = y)
  out1 <- bestglm(Xy, IC = "BIC", intercept = FALSE)
  ##select the best GLM model 
  best.temp <- out1$BestModels[1,]
  BEST <- colnames(best.temp)[best.temp==TRUE]
  BEST <- c("subject", "fatiguestate1", "fatiguestate", BEST)
  
  ###for LASSO
  #X1 <-alldata[train.index,-seq(1,6)]
  #X<- as.matrix(scaleContinuous(X1))
  #y <- as.matrix(alldata[train.index, 4])
  #cvfit <- glmnet::cv.glmnet(X, y,alpha=1, intercept=FALSE)
  #Coefficients<-data.frame(as.matrix(coef(cvfit,  s =   cvfit$lambda.min)))
  #Coefficients1<- data.frame(feature = rownames(Coefficients), coef = Coefficients[,1])
  #BEST<- as.vector(Coefficients1[!rowSums(Coefficients1 == 0), 1])[-1]
  #BEST <- c("subject", "fatiguestate1", "fatiguestate", BEST)
  ###
  
  if((length(BEST)<=4) )  {
    return (list(0,0,0,0))
  } else{
    
  Train.0 <- which(alldata$fatiguestate1[train.index]==0)
  Train.1 <- which(alldata$fatiguestate1[train.index]==1)
    
  accuracy <- rep(NA, 200)
  sen <- rep(NA, 200)
  spe <- rep(NA, 200)
    
  for (k in 1:2){
    set.seed((1994+k))
    index.train <- c(sample(Train.0, 99, replace=TRUE), sample(Train.1, 99, replace=TRUE))
    test <- alldata[test.index, BEST]
    train <- alldata[index.train, BEST]
    fatigued.states<- sum(test$fatiguestate1)
    not.fatigued.states<- nrow(test)-sum(test$fatiguestate1)
    valid.participant<-allcombinations[j,]
      
    train_x <- scaleContinuous(train[,-c(1,2,3)])
    dataset <- data.frame(cbind(class=train[,c("fatiguestate")],train_x))
    train.colsd <- apply(train[,-c(1,2,3)], 2, sd)
    train.colmeans <- apply(train[,-c(1,2,3)], 2, mean)
    prepare_test <- test[,-c(1,2,3)]
      
    ### prepare the test data
    ### Center and Scale the features in the test data using the training data
    for (o in 1: ncol(prepare_test)){
        prepare_test[,o] <- (prepare_test[,o]-train.colmeans[o])/train.colsd[o]
    }
      
    test_forfeatureselection <- data.frame(cbind(class=test[,c("fatiguestate")],prepare_test))
    test_forfeatureselection$class <- as.factor(test[,c("fatiguestate")])
    dataset_test <- data.frame(test_forfeatureselection)
    testsamplesize <- nrow(dataset_test) 
      
    ### RF model
    set.seed((123+k))
    mod <- train(class ~ ., method="rf", data = dataset)
    pred <- predict(mod, dataset_test)

    ### Check the performance of the model
    comparison <- table(dataset_test$class,pred)
    accuracy[k] <- (comparison[1,1]+comparison[2,2])/testsamplesize
    if(not.fatigued.states==0){
        spe[k] <- 0
      }else{
        spe[k] <- comparison[2,2]/ (not.fatigued.states)
    }
      
    if(fatigued.states==0){
        sen[k] <- 0
      }else{
        sen[k] <- comparison[1,1]/(fatigued.states)
      }
    }
    
    ### prepare to export the output
    model <- c(j, length(BEST[-c(1,2,3)]),c(allcombinations[j,]), round(c(mean(accuracy), sd(accuracy), mean(sen), sd(sen), mean(spe), sd(spe)),4))
    metric <- round(c(mean(accuracy), sd(accuracy), mean(sen), sd(sen), mean(spe), sd(spe)),4)
    best_features1 <-  BEST[-c(1,2,3)]
    validation.participant <- valid.participant
    
    ### save output values as a list
    Optimal_result <- list(model, metric, best_features1, validation.participant)
    return(Optimal_result)
  }
}

library(snow)   ### for the parallel computation
library(readr)  ### for read_csv online
library(xlsx)   ### save outputs into an excel file

### define the group of the stratified sampling 
g1 <- c("P1","P2","P4","P6","P8","P12","P13","P14","P15","P16","P17","P18","P19") 

### data preparation
myfile <- "https://raw.githubusercontent.com/zahrame/FatigueManagement.github.io/master/WLK_13p.csv"
alldata <- read.csv(url(myfile))
allparticipants <- cbind(c(g1))
allcombinations2 <- t(data.frame(combn(c(g1), 2)))
allcombinations <- apply(allcombinations2, 2, as.character)


### set up a timer to see how much time spend on the parallel computation!
time.begin <- proc.time()[3]

cl <- makeCluster(3, type="SOCK") ### number of cores in your computer
ncases <- nrow(allcombinations) 
Result <- parSapply(cl, 1:ncases, Mainfun, alldata, allparticipants, allcombinations)
stopCluster(cl)
time.end <- proc.time()[3]-time.begin
paste("It took", time.end, "seconds to run the program.")

### Save output values in a dataframe and important features for each case in a list
sen_spe <- data.frame(matrix(0, nrow = ncases, ncol = 10))
colnames(sen_spe) <- c("model","#features","par1","par2","accuracy", "sd(accuracy)","sensitivity","sd(sensitivity)", "specificity", "sd(specificity)") 
best_features <- list()
participant.valid <- list()
metric <- list()

for (i in 1:ncases){
  sen_spe[i,] <- as.vector(Result[[(4*i-3)]])
  metric[[i]] <- Result[[(4*i-2)]]
  best_features[[i]] <- Result[[(4*i-1)]]
  participant.valid[[i]] <- Result[[(4*i)]]
}

metric1 <- (data.frame(plyr::ldply(metric, rbind)))
metric2 <- metric1[complete.cases(metric1), ]
mean.model <- apply(metric2, 2, mean) 
sd.model <- apply(metric2, 2, sd) 
median.model <- apply(metric2, 2, median) 
q.model <- apply( metric2, 2, quantile ,probs=c(0.25,0.75))
#consistency<-mean(abs(sen_spe[,7]-sen_spe[,10]))

all.metrics <- rbind(mean.model,sd.model,median.model, q.model)
colnames(all.metrics) <- c("accuracy", "sd(accuracy)", "sensitivity","sd(sensitivity)","specificity", "sd(specificity)")
model_features <- (data.frame(plyr::ldply(best_features, rbind)))

model <- "rf_bootstrap_size99"
par <- "2out_"
feature.selection <- "BIC_"
task <- "WLK"

### export outputs to an excel file
Myfile <- paste(task,"_", model,"_", par,"_", feature.selection,".xlsx", sep="")
write.xlsx(sen_spe, file = Myfile, sheetName=paste("sen_spe", sampling, sep=""), row.names=FALSE)
write.xlsx(model_features, file = Myfile, sheetName=paste("features", sampling, sep=""), append=TRUE, row.names=FALSE)
write.xlsx(all.metrics, file = Myfile, sheetName=paste("all metrics", sampling, sep=""), append=TRUE, row.names=TRUE)