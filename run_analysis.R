run_analysis <- function(){
  dtTrain <- giveMeTrainSet()
  dtTest <- giveMeTestSet()
  total <- rbind(dtTrain, dtTest) 
  #total[,means := rowMeans(total[,4:564,with=FALSE])]
  #total <- cbind(index = 1:10299,total)
  #setkey(total, index)
  #total[, sdev := sd(total[index,5:565,with=FALSE]), by=index]
  #head(total,5)
  write.csv(x=total, file="dataset/mean_dev.csv")
  
  
}

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

readytest <- function(){
  library(data.table)
  dt <- fread("dataset/test/y_test.txt", header=FALSE)
  setnames(dt, "V1", "y_label") 
  dt
}

readFeatures <- function(){
  library(data.table)
  dt <- fread("dataset/features.txt", header=FALSE, sep=" ")
  setnames(dt, "V2", "features") 
  dt
}

readXtest <- function(){
  library(data.table)
  if (!file.exists("dataset/test/X_test_rf.TXT")){
    df <- read.csv(file="dataset/test/X_test.TXT", header=FALSE, 
                   #                                         nrow=100, 
                   colClasses="character")
    for (i in 1:nrow(df))  {
      df[i,] <- gsub("  ", " ", df[i,])
      df[i,] <- sub(" ", "", df[i,])
    }
    write.table(df, file="dataset/test/X_test_rf.TXT", row.names=FALSE, col.names=FALSE, quote=FALSE)
  }
  dt <- fread("dataset/test/X_test_rf.TXT", header=FALSE, sep=" ") 
  
  features <- readFeatures()
  for(i in 1:561){
    #setnames(dt, paste("V",i,sep=""), as.character(i))
    setnames(dt, paste("V",i,sep=""), as.character(features[i, "features", with=FALSE]))
  }
  dtMean <- dt[, which(grepl("mean", colnames(dt))), with=FALSE]
  dtStDev <- dt[, which(grepl("std", colnames(dt))), with=FALSE]
  dt <- cbind(dtMean, dtStDev)
  dt
}

readSubjectTest <- function(){
  library(data.table)
  dt <- fread("dataset/test/subject_test.txt", header=FALSE)
  setnames(dt, "V1", "subject") 
  dt
}

###################################

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

readytrain <- function(){
  library(data.table)
  dt <- fread("dataset/train/y_train.txt", header=FALSE)
  setnames(dt, "V1", "y_label") 
  dt
}

readXtrain <- function(){
  library(data.table)
  if (!file.exists("dataset/train/X_train_rf.TXT")){
    df <- read.csv(file="dataset/train/X_train.TXT", header=FALSE, 
                   #                                         nrow=100, 
                   colClasses="character")
    for (i in 1:nrow(df))  {
      df[i,] <- gsub("  ", " ", df[i,])
      df[i,] <- sub(" ", "", df[i,])
    }
    write.table(df, file="dataset/train/X_train_rf.TXT", row.names=FALSE, col.names=FALSE, quote=FALSE)
  }
  dt <- fread("dataset/train/X_train_rf.TXT", header=FALSE, sep=" ")
  features <- readFeatures()
  for(i in 1:561){
    #     setnames(dt, paste("V",i,sep=""), as.character(i))
    setnames(dt, paste("V",i,sep=""), as.character(features[i, "features", with=FALSE]))
  }
  dtMean <- dt[, which(grepl("mean", colnames(dt))), with=FALSE]
  dtStDev <- dt[, which(grepl("std", colnames(dt))), with=FALSE]
  dt <- cbind(dtMean, dtStDev)
  dt
}

readActivityLabels <- function(){
  library(data.table)
  dt <- fread("dataset/activity_labels.TXT", header=FALSE, sep = " ")
  setnames(dt, c("V1","V2"), c("id_activity_label","activity_label") )
  #   dt[5,"activity_label",with=FALSE]
  dt
}

readSubjectTrain <- function(){
  library(data.table)
  dt <- fread("dataset/train/subject_train.txt", header=FALSE)
  setnames(dt, "V1", "subject") 
  dt
}
