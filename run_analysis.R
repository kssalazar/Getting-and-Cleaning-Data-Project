install.packages("plyr")
install.packages("data.table")
library(plyr)
library(data.table)

### 01. Get the data for the project
#   DOwnload data from the web
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")
#   Unzip dataset to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")
#   Get list of files
filePath <- file.path("./data" , "UCI HAR Dataset")
filesRaw <- list.files(filePath, recursive=TRUE)
filesRaw


### 02.Read raw data sets
#   Read training data sets
xTrain = read.table(file.path(filePath, "train", "X_train.txt"),header = FALSE)
yTrain = read.table(file.path(filePath, "train", "y_train.txt"),header = FALSE)
subjectTrain = read.table(file.path(filePath, "train", "subject_train.txt"),header = FALSE)
#   Read the test data sets
xTest = read.table(file.path(filePath, "test", "X_test.txt"),header = FALSE)
yTest = read.table(file.path(filePath, "test", "y_test.txt"),header = FALSE)
subjectTest = read.table(file.path(filePath, "test", "subject_test.txt"),header = FALSE)
#   Read features data set
features = read.table(file.path(filePath, "features.txt"),header = FALSE)
dim(features)
#   Read activity labels data
activityLabels = read.table(file.path(filePath, "activity_labels.txt"),header = FALSE)
head(activityLabels)
#   Assign column names
colnames(xTrain) <- features[,2] 
colnames(yTrain) <-"activityId"
colnames(subjectTrain) <- "subjectId"
colnames(xTest) <- features[,2] 
colnames(yTest) <- "activityId"
colnames(subjectTest) <- "subjectId"
colnames(activityLabels) <- c('activityId','activityType') 

### 1.Merges the training and the test sets to create one data set.
#   Merge the train and test data
mergeTrain = cbind(yTrain, subjectTrain, xTrain)
mergeTest  = cbind(yTest, subjectTest, xTest)
#   Append train and test to create one data set
setMerge = rbind(mergeTrain, mergeTest)


### 2.Extracts only the measurements on the mean and standard deviation for each measurement.
colNames = colnames(setMerge)
meanStd = (grepl("activityId" , colNames) | grepl("subjectId" , colNames) | grepl("mean.." , colNames) | grepl("std.." , colNames))
setMeanStd <- setMerge[ , meanStd == TRUE]


### 3.Uses descriptive activity names to name the activities in the data set
setActivityNames = merge(setMeanStd, activityLabels, by='activityId', all.x=TRUE)


### 4.Appropriately labels the data set with descriptive variable names.
#   Already done above

### 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
setMean <- aggregate(. ~subjectId + activityId, setActivityNames, mean)
setMean <- setMean[order(setMean$subjectId, setMean$activityId),]
#   Write output to text file
write.table(setMean, "setMean.txt", row.name=FALSE)
