## Mark Conrad
## 10/14/2016
## Coursera Getting and Cleaning Data Course Project

## This script will merge the training and the test sets to create one data set.
## It will extract only the measurements on the mean and standard deviaton for each measurement.
## It uses descriptive activity names to name the activities in the data set.
## It appropiately labels the data set with descriptive variable names.
## From the data set in step 4, creates a second, independent tidy data set with the average of each
## variable for each activity and each subject.

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")
library(dplyr)
library(data.table)
library(tidyr)


###Unzip DataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")

files <- file.path("./data", "UCI HAR Dataset")
# Read subject files
Subject_Train <- tbl_df(read.table(file.path(files, "train", "subject_train.txt")))
Subject_Test  <- tbl_df(read.table(file.path(files, "test" , "subject_test.txt" )))

# Read activity files
Activity_Train <- tbl_df(read.table(file.path(files, "train", "Y_train.txt")))
Activity_Test  <- tbl_df(read.table(file.path(files, "test" , "Y_test.txt" )))

#Read data files.
data_Train <- tbl_df(read.table(file.path(files, "train", "X_train.txt" )))
data_Test  <- tbl_df(read.table(file.path(files, "test" , "X_test.txt" )))
# for both Activity and Subject files this will merge the training and the test sets by row binding 
#and rename variables "subject" and "activityNum"
data_Subject <- rbind(Subject_Train, Subject_Test)
setnames(data_Subject, "V1", "subject")
data_Activity<- rbind(Activity_Train, Activity_Test)
setnames(data_Activity, "V1", "activityNum")

#combine the DATA training and test files
data_Table <- rbind(data_Train, data_Test)

# name variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")
data_Features <- tbl_df(read.table(file.path(files, "features.txt")))
setnames(data_Features, names(data_Features), c("featureNum", "featureName"))
colnames(data_Table) <- data_Features$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
alldataSubjAct<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)

# Reading "features.txt" and extracting only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

##enter name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

## create dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "Time", names(dataTable))
names(dataTable)<-gsub("^f", "Frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

write.table(dataTable, "TidyData.txt", row.name=FALSE)