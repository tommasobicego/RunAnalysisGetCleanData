###run_analysis.R list of functions:

run_analysis
function that writes on the CSV file dataset/mean_dev.csv the complete set of data (train + test), the test's set records are appended to the train's set ones.
    
    run_analysis <- function(){
      dtTrain <- giveMeTrainSet()
      dtTest <- giveMeTestSet()
      total <- rbind(dtTrain, dtTest)
      write.csv(x=total, file="dataset/mean_dev.csv")
    }

giveMeTestSet
functionn that returns the complete set of data contained by the directory test, and already formatted.
    
    giveMeTestSet <- function(){
      ytest <- readytest()
      Xtest <- readXtest()
      subject <- readSubjectTest()
      acti <- readActivityLabels()
      dt <- cbind(ytest, Xtest)
      dt <- cbind(subject, dt)
      acti <- acti[dt$y,"activity_label",with=FALSE]
      dt <- cbind(acti, dt)
      dt
    }

readytest
function that returns a data.table containing the values from the /test/y_test.txt file
    
    readytest <- function(){
      library(data.table)
      dt <- fread("dataset/test/y_test.txt", header=FALSE)
      setnames(dt, "V1", "y_label")
      dt
    }

readFeatures
function that returns a data.table containing the values from the /features.txt file
    
    readFeatures <- function(){
      library(data.table)
      dt <- fread("dataset/features.txt", header=FALSE, sep=" ")
      setnames(dt, "V2", "features")
      dt
    }

readXtest
function that returns a data.table containing the values about the means and the santard deviation from the /test/X_test.TXT file, the chosen columns are selected via the file with the features (the function readFeatures determines the names of the columns because there are many records on a row as there are features)
    
    readXtest <- function(){
      library(data.table)
      if (!file.exists("dataset/test/X_test_rf.TXT")){
        df <- read.csv(file="dataset/test/X_test.TXT", header=FALSE,
                       colClasses="character")
        for (i in 1:nrow(df)) {
          df[i,] <- gsub(" ", " ", df[i,])
          df[i,] <- sub(" ", "", df[i,])
        }
        write.table(df, file="dataset/test/X_test_rf.TXT", row.names=FALSE, col.names=FALSE, quote=FALSE)
      }
      dt <- fread("dataset/test/X_test_rf.TXT", header=FALSE, sep=" ")
      
      features <- readFeatures()
      for(i in 1:561){
        setnames(dt, paste("V",i,sep=""), as.character(features[i, "features", with=FALSE]))
      }
      dtMean <- dt[, which(grepl("mean", colnames(dt))), with=FALSE]
      dtStDev <- dt[, which(grepl("std", colnames(dt))), with=FALSE]
      dt <- cbind(dtMean, dtStDev)
      dt
    }

readSubjectTest
function that returns a data.table containing the values from the /test/subject_test.txt file
    
    readSubjectTest <- function(){
      library(data.table)
      dt <- fread("dataset/test/subject_test.txt", header=FALSE)
      setnames(dt, "V1", "subject")
      dt
    }

giveMeTrainSet
function that returns the complete set of data contained by the directory train, and already formatted.
    
    giveMeTrainSet <- function(){
      ytrain <- readytrain()
      Xtrain <- readXtrain()
      subject <- readSubjectTrain()
      acti <- readActivityLabels()
      dt <- cbind(ytrain, Xtrain)
      dt <- cbind(subject, dt)
      acti <- acti[dt$y,"activity_label",with=FALSE]
      dt <- cbind(acti, dt)
      dt
    }

readytrain
function that returns a data.table containing the values from the /train/y_train.txt file
    
    readytrain <- function(){
      library(data.table)
      dt <- fread("dataset/train/y_train.txt", header=FALSE)
      setnames(dt, "V1", "y_label")
      dt
    }

readXtrain
function that returns a data.table containing the values about the means and the santard deviation from the /train/X_train.TXT file, the chosen columns are selected via the file with the features (the function readFeatures determines the names of the columns because there are many records on a row as there are features)
    
    readXtrain <- function(){
      library(data.table)
      if (!file.exists("dataset/train/X_train_rf.TXT")){
        df <- read.csv(file="dataset/train/X_train.TXT", header=FALSE,
                       colClasses="character")
        for (i in 1:nrow(df)) {
          df[i,] <- gsub(" ", " ", df[i,])
          df[i,] <- sub(" ", "", df[i,])
        }
        write.table(df, file="dataset/train/X_train_rf.TXT", row.names=FALSE, col.names=FALSE, quote=FALSE)
      }
      dt <- fread("dataset/train/X_train_rf.TXT", header=FALSE, sep=" ")
      features <- readFeatures()
      for(i in 1:561){
        setnames(dt, paste("V",i,sep=""), as.character(features[i, "features", with=FALSE]))
      }
      dtMean <- dt[, which(grepl("mean", colnames(dt))), with=FALSE]
      dtStDev <- dt[, which(grepl("std", colnames(dt))), with=FALSE]
      dt <- cbind(dtMean, dtStDev)
      dt
    }

readActivityLabels
function that returns a data.table containing the values from the activity_labels.TXT file
    
    readActivityLabels <- function(){
      library(data.table)
      dt <- fread("dataset/activity_labels.TXT", header=FALSE, sep = " ")
      setnames(dt, c("V1","V2"), c("id_activity_label","activity_label") )
      dt
    }

readSubjectTrain
function that returns a data.table containing the values from the /train/subject_train.txt file
    
    readSubjectTrain <- function(){
      library(data.table)
      dt <- fread("dataset/train/subject_train.txt", header=FALSE)
      setnames(dt, "V1", "subject")
      dt
    }

#########################################################
###run_analysis_5.R functions:

run_analysis_5

returns the average of each variable for each activity and each subject as defined in the file mean_dev.csv created by the function run_analysis() as defined in the file run_analysis.R. The function writes this set into the CSV file dtKEnd.csv
    
    run_analysis_5 <- function(){
      library(data.table)
      dt <- fread("dataset/mean_dev.csv", sep=",")
      
      dtK <- dt[,c("activity_label","subject"), with=FALSE]
      dtK <- unique(dtK)
      unique(dtK)  
      dtKEnd <- NULL
      
      for (i in 1:nrow(dtK) ) {
        scr1 <- dt[dt$activity_label == dtK[i,]$activity_label & dt$subject == dtK[i,]$subject,]
        dcolMeans <- colMeans(scr1[,5:ncol(dt),with=FALSE] )   
        if(is.null(dtKEnd)){
          dtKEnd <- dcolMeans
        } else {
          dtKEnd <- rbind(dtKEnd,dcolMeans)
        }
      }
      
      dtKEnd <- cbind(dtK, dtKEnd)
      
      write.csv(dtKEnd, file ="dataset/dtKEnd.csv" )
    }


