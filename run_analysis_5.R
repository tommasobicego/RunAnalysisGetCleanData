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

