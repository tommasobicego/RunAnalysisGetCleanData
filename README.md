###run_analysis.R list of functions:

run_analysis
function that writes on the CSV file dataset/mean_dev.csv the complete set of data (train + test), the test's set records are appended to the train's set ones.

giveMeTestSet
functionn that returns the complete set of data contained by the directory test, and already formatted.

readytest
function that returns a data.table containing the values from the /test/y_test.txt file

readFeatures
function that returns a data.table containing the values from the /features.txt file

readXtest
function that returns a data.table containing the values about the means and the santard deviation from the /test/X_test.TXT file, the chosen columns are selected via the file with the features (the function readFeatures determines the names of the columns because there are many records on a row as there are features)

readSubjectTest
function that returns a data.table containing the values from the /test/subject_test.txt file

giveMeTrainSet
functionn that returns the complete set of data contained by the directory train, and already formatted.

readytrain
function that returns a data.table containing the values from the /train/y_train.txt file

readXtrain
function that returns a data.table containing the values about the means and the santard deviation from the /train/X_train.TXT file, the chosen columns are selected via the file with the features (the function readFeatures determines the names of the columns because there are many records on a row as there are features)

readActivityLabels
function that returns a data.table containing the values from the activity_labels.TXT file

readSubjectTrain
function that returns a data.table containing the values from the /train/subject_train.txt file

#########################################################
###run_analysis_5.R functions:

run_analysis_5
returns the average of each variable for each activity and each subject as defined in the file mean_dev.csv created by the function run_analysis() as defined in the file run_analysis.R.
    
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


