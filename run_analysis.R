library(data.table)

getColumnNames <- function(featureFileName) {
  features <- read.table(featureFileName, stringsAsFactors=FALSE)
  columns <- features$V2
  columns <- gsub("[(,)-]","",columns)
  columns <- c("Subject","Activity", columns)
  columns
}

createDataSet <- function (datasetKey, columnNames) {
  dataSetNames <- c(paste0(datasetKey,"/X_",datasetKey,sep=""), paste(datasetKey,"/subject_",datasetKey,sep=""),paste(datasetKey,"/y_",datasetKey,sep=""))
  df <- read.table(paste0(dataSetNames[1],".txt"))
  sub <- read.table(paste0(dataSetNames[2],".txt"))
  y <- read.table(paste0(dataSetNames[3],".txt"))
  df <- cbind(sub, y, df)      
  names(df) <- columnNames
  as.data.table(df)
}

filterDataSet <- function(completeDataSet, columnNames) {
  filteredColumnNames <- grep("mean|std|Subject|Activity",columnNames, ignore.case = T)
  filteredDataset <- subset(completeDataSet, TRUE, filteredColumnNames)
}

mergeTrainingTestData <- function(workingDirectory) {
  setwd(workingDirectory)
  columnNames <- getColumnNames("features.txt")
  training <- createDataSet("train", columnNames)
  test <- createDataSet("test", columnNames)
  completeDataSet <- rbind(test,training)
  
  filterDataSet(completeDataSet, columnNames)
}
prepareActivitySubjectDataSet <- function(filteredDataset) {
  averagesDataSet <- filteredDataset[, lapply(.SD, mean), by=list(Subject, Activity), .SDcols=3:ncol(filteredDataset)]
  setwd("../")
  write.table(averagesDataSet,"ActivitySubjectAverages.txt", row.names=FALSE)
}
run_analysis <- function(workingDirectory) {
  
  # Create second tidy data
  prepareActivitySubjectDataSet(mergeTrainingTestData(workingDirectory))
  
}
