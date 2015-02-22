# GettingAndCleaningData

To run the analysis on the dataset:
* Download the Smartphone dataset (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip) and unzip into a directory (i.e.) /tmp
* Download the analysis script from the git repository (https://github.com/rgwoodley/GettingAndCleaningData/blob/master/run_analysis.R)
* In RStudio or R source the run_analysis.R file: **source \<path\_where_you\_downloaded\_script>/run_analysis.R**
* Set your working directory to the directory where you have unzipped teh data set: (i.e. **setwd("/tmp")** )
* From R: **run_analysis() (i.e run_analysis("/tmp/UCI HAR Dataset")
 

The resulting data set will be created in the /tmp directory (ActivitySubjectAverages.txt)


