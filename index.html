---
title: "A Data Analytic Framework for Physical Fatigue Management Using Wearable Sensors"
author:
- Zahra Sedighi Maman^[Adelphi University, [zmaman@adelphi.edu](mailto:zmaman@adelphi.edu)]
- Ying-Ju Tessa Chen^[University of Dayton, [ychen4@udayton.edu](mailto:ychen4@udayton.edu)]
- Amir Baghdadi^[University of Calgary, [amirbagh@buffalo.edu](mailto::amirbagh@buffalo.edu)]
- Seamus Lombardo^[Massachusetts Institute of Technology, [seamuslo@mit.edu](mailto::seamuslo@mit.edu)]
- Lora A. Cavuoto^[University at Buffalo, [loracavu@buffalo.edu](mailto::loracavu@buffalo.edu)]
- Fadel M. Megahed^[Miami University, [fmegahed@miamioh.edu](mailto:fmegahed@miamioh.edu)]
date: "`r format(Sys.time(), '%B %d, %Y')`"
output:
  html_document: 
    code_folding: hide
    number_sections: yes
    toc: yes
    toc_float: yes
  word_document:
    toc: yes
  pdf_document: 
    toc: yes
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```
This page documents the ... 

Our detiled code is available at: https://github.com/zahrame/Fatigue-Management


The reader can **show** any code chunk by clicking on the *code* button. We chose to make the default for the code hidden since we: (a) wanted to improve the readability of this document; and (b) assumed that the readers will not be interested in reading every code chunk.

Figure 1 provides an overview of our the proposed fatigue management data-driven framework.

![Figure 1: The Proposed Framework for Fatigue Management](https://user-images.githubusercontent.com/13205340/74370811-bcc21500-4da5-11ea-86b9-c097d861414d.png)


# Section: Loading Data and Data Preparation
## Section: Required Packages
The snippet below presents the set of **R** packages that are utilized in different sections in this study. These packages are used for feature selection, model development, and parallel computing.

```{r, load_libraries, message=FALSE, eval=FALSE, cache=TRUE, error=FALSE, warning=FALSE}
rm(list=ls()) # clear global environment
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
  library(nnet)
  library(klaR)
  library(MASS)
  library(C50)
  library(snow)   ### for the parallel computation
  library(readr)  ### for read_csv online
  library(xlsx)   ### save outputs into an excel file
```

## Section: Data Collection, Data Preprocessing, Creating Training and Testing Sets
In this snippet below, we load the data file used in this study, including the 15 participants for the Material Handling Task. The output of this step is a set of training and testing data that can be used for model development. We generate 105 training and test sets using 2-participants out cross-validation. The Processed Raw data with the defined features can be obtained through the github link presented in the code below.
```{r, message=FALSE, eval=FALSE, cache=TRUE, error=FALSE, warning=FALSE}

# data are loaded from github
### data preparation
myfile <- "https://raw.githubusercontent.com/zahrame/Fatigue-Management/master/MMH_features_mean_cv_kinematic_reduced_15p.csv"
alldata <- read.csv(url(myfile))
alldata <- read.csv("MMH_features_mean_cv_kinematic_reduced_15p.csv",header=TRUE)

#define the participants
g1 <- c("P1","P2","P3","P4","P5","P6",
        "P7","P8","P9","P10","P11","P12",
        "P13","P14","P15")

#define the combinations of 2-participants out for cross-validation
allparticipants <- cbind(c(g1))
allcombinations2 <- t(data.frame(combn(c(g1), 2)))
allcombinations <- apply(allcombinations2, 2, as.character)
```


## Section: Feature Scaling Function
In this snippet below, we define a function to scale the features, where it is implemented for continuous features. The scale function subtracts the mean value of  column X (feature) from each sample in column X and then divides by the column's standard deviation.

```{r, message=FALSE, eval=FALSE, error=FALSE, cache=TRUE, warning=FALSE}
## In the following lines, we scaled the features 
scaleContinuous <- function(data) {
    binary = apply(data, 2, function(x) {all(x %in% 0:1)}) 
    data[!binary] = scale(data[!binary])
    return(data)
  }
```


# Section: Feature Selction
In this section below, we implemented feature selection to reduce the dimensionality of our dataset. In order to do the feature selection. First we scaled the training set. Second, we used a) best subset selection using BIC as a criterion, and b) LASSO for feature selection. The output of this section is the set of important features for the training set. It should be noted that this procedure is repeated for each cross-validation set, 105 times in overall.

```{r , message=FALSE, eval=FALSE, cache=TRUE, error=FALSE}
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
```

# Section: Model Development
In this section, we developed several analytical models for the Manual Material Handing Task. The out put of this section is the best analytical model that has higher performance in predicting both fatigued and non-fatigued states. For each 105 cross-validation set, We first generated a bootstrapped samples from the training set (equal to the training set sample size, for example 234 for Manual Material Handling), and then we developed models using (bagging, boosting, random forest, SVM, logistic regression, and penalized logistic regression). We repeated the bootstarpping procedure for 200 times for each training set. We recorded the average of the performance metrics for each training set. 
For model development we used **train** function from **caret** package. Within the **train** function, for the model, we used **rf** for random forest, **treebag** for bagging, **gbm** for boosting, **svmRadial** for SVM, **glm** for logistic regression.
We used four performance metrics, Sensitivity, Specificity, Accuracy, and Consistency.

```{r , message=FALSE, eval=FALSE, cache=TRUE, error=FALSE}
Train.0 <- which(alldata$fatiguestate1[train.index]==0)
Train.1 <- which(alldata$fatiguestate1[train.index]==1)
  
accuracy <- rep(NA, 200)
sen <- rep(NA, 200)
spe <- rep(NA, 200)
  
for (k in 1:200){
    set.seed((1994+k))
    index.train <- c(sample(Train.0, 117, replace=TRUE), sample(Train.1, 117, replace=TRUE))
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
```

## Section: Consolidating Results of Analytical Models
In this section, we consolidated the output from model development section. It should be noted that all the commutations were performed using the **Ohio Super Computer**. The following codes merge the performance metrics, their descriptive statistics, and important features across all 105 cross-validation models into one excel file. In addition, the code below shows the output for random forest, using bootstrapping, with 2-participant out cross-validation for the Manual Material Handling task.

```{r , message=FALSE, eval=FALSE, cache=TRUE, error=FALSE}
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

all.metrics <- rbind(mean.model,sd.model,median.model, q.model)
colnames(all.metrics) <- c("accuracy", "sd(accuracy)", "sensitivity","sd(sensitivity)","specificity", "sd(specificity)")
model_features <- (data.frame(plyr::ldply(best_features, rbind)))

model <- "rf_"
sampling <- "bootstrap_"
par <- "2out_"
feature.selection <- "BIC_"
task <- "MMH"


### export outputs to an excel file
Myfile <- paste(task,"_", model,"_", par,"_", feature.selection,".xlsx", sep="")
write.xlsx(sen_spe, file = Myfile, sheetName=paste("sen_spe", sampling, sep=""), row.names=FALSE)
write.xlsx(model_features, file = Myfile, sheetName=paste("features", sampling, sep=""), append=TRUE, row.names=FALSE)
write.xlsx(all.metrics, file = Myfile, sheetName=paste("all metrics", sampling, sep=""), append=TRUE, row.names=TRUE)

```

# Section: Parallel Processing Implementation
The section below shows the job script written to submit a parallel processing code to the **Ohio Super Computer**. This code records the required amount of time to finish this job as well. 
```{r , message=FALSE, eval=FALSE, cache=FALSE, error=FALSE}
time.begin <- proc.time()[3]
cl <- makeCluster(, type="SOCK") ### number of cores 
ncases <- nrow(allcombinations) 
Result <- parSapply(cl, 1:ncases, Mainfun, alldata, allparticipants, allcombinations)
stopCluster(cl)
time.end <- proc.time()[3]-time.begin
paste("It took", time.end, "seconds to run the program.")
```

