library(data.table)

######################################################################################################## 
## Functions use to transform and tidy dataset
## getColumnNames - Retrieve column names
## createDataSet  - Create data set
## filterDataSet  - Filter Dataset based on particular columns
## prepareActivitySubjectDataSet - Prepare dataset for analysis
## mergeTrainingTestData  - Merge training and test datasets
## run_analysis - Run Analysis to create data sets
##
## Author: Woodley, R.
######################################################################################################## 

######################################################################################################### 
## Function: getColumnNames
## Synposis: From the feature file list of columns in the dataset, create a list of column names removing
##           any comma, dashes and paranthesis. 
##           The addition of the columns Subject and Activity are added.
## Args:
##  featureFileName: Feature File name containing the description of the columns.
#########################################################################################################
getColumnNames <- function(featureFileName) {
  features <- read.table(featureFileName, stringsAsFactors=FALSE)
  columns <- features$V2
  columns <- gsub("[(,)-]","",columns)
  columns <- c("Subject","Activity", columns)
  columns
}

######################################################################################################### 
## Function: createDataSet
## Synposis: Creates the dataset from the training/test directory.  The different datasets are read and 
##           merged into one data.table.
## Args:
##  datasetKey: Key that specifies either 'training' or 'test'
#########################################################################################################
createDataSet <- function (datasetKey, columnNames) {
  dataSetNames <- c(paste0(datasetKey,"/X_",datasetKey,sep=""), paste(datasetKey,"/subject_",datasetKey,sep=""),paste(datasetKey,"/y_",datasetKey,sep=""))
  df <- read.table(paste0(dataSetNames[1],".txt"))
  sub <- read.table(paste0(dataSetNames[2],".txt"))
  y <- read.table(paste0(dataSetNames[3],".txt"))
  df <- cbind(sub, y, df)      
  names(df) <- columnNames
  as.data.table(df)
}

######################################################################################################### 
## Function: filterDataSet
## Synposis: Created a subset of the merged data set that contains only the means and standard deviations
## Args:
##  completeDataSet: Dataset containing the test and training data.
##  columnNames:     The column names
#########################################################################################################
filterDataSet <- function(completeDataSet, columnNames) {
  filteredColumnNames <- grep("mean|std|Subject|Activity",columnNames, ignore.case = T)
  filteredDataset <- subset(completeDataSet, TRUE, filteredColumnNames)
}

######################################################################################################### 
## Function: mergeTrainingTestData
## Synposis: Merge the test and traingin datasets then call the filter to create the dataset with ONLY
##           the mean/std and Subject/Activity
## Args:
##  workingDirectory: Working directory where teh Samrtphone dataset is located.
#########################################################################################################
mergeTrainingTestData <- function(workingDirectory) {
  setwd(workingDirectory)
  columnNames <- getColumnNames("features.txt")
  training <- createDataSet("train", columnNames)
  test <- createDataSet("test", columnNames)
  completeDataSet <- rbind(test,training)
  
  filterDataSet(completeDataSet, columnNames)
}

######################################################################################################### 
## Function: prepareActivitySubjectDataSet
## Synposis: Create the dataset of averages grouped by Subject and Activity. 
## Args:
##  filteredDataset: Dataset containing only tehcolumns that we want to work with.
#########################################################################################################
prepareActivitySubjectDataSet <- function(filteredDataset) {
  averagesDataSet <- filteredDataset[, lapply(.SD, mean), by=list(Subject, Activity), .SDcols=3:ncol(filteredDataset)]
  setwd("../")
  write.table(averagesDataSet,"ActivitySubjectAverages.txt", row.names=FALSE)
}

######################################################################################################### 
## Function: run_analysis
## Synposis: Main analysis function
## Args: None
## Output: File named 'ActivitySubjectAverages.txt' created in the parent directory of the Smartphone 
##         dataset.
#########################################################################################################
run_analysis <- function() {
  
  workingDirectory="UCI HAR Dataset"
  # Create second tidy data
  prepareActivitySubjectDataSet(mergeTrainingTestData(workingDirectory))
  
}
