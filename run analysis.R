# Getting-and-Cleaning-data-course-project
if (!getwd() == "./out-of-box-samples") {
    dir.create("./out-of-box-samples")
}
rm(list = ls(all = TRUE))
library(plyr) 
library(data.table) 
library(dplyr) 
get <- tempfile()
download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",get)
unzip(get, list = TRUE)

dataActivityTest <- read.table(unzip(get, "UCI HAR Dataset/test/y_test.txt"))
dataSubjectTest  <- read.table(unzip(get, "UCI HAR Dataset/test/X_test.txt"))
DataTest <- read.table(unzip(get, "UCI HAR Dataset/test/subject_test.txt"))

dataActivityTrain <- read.table(unzip(get, "UCI HAR Dataset/train/y_train.txt"))
dataSubjectTrain  <- read.table(unzip(get, "UCI HAR Dataset/train/X_train.txt"))
DataTrain <- read.table(unzip(get, "UCI HAR Dataset/train/subject_train.txt"))

Features <- read.table(unzip(get, "UCI HAR Dataset/features.txt"))

colnames(dataSubjectTrain) <- t(Features[2])
colnames(dataSubjectTest) <- t(Features[2])
dataSubjectTrain$activities <- dataActivityTrain[, 1]
dataSubjectTrain$participants <- DataTrain[, 1]
dataSubjectTest$activities <- dataActivityTest[, 1]
dataSubjectTest$participants <- DataTest[, 1]


#1 Merges the training and the test sets to create one data set.
Master <- cbind(dataSubjectTrain, dataActivityTest)
duplicated(colnames(Master))
Master <- Master[, !duplicated(colnames(Master))]

training_data <- cbind(cbind(dataSubjectTrain, DataTrain), dataSubjectTrain)
test_data <- cbind(cbind(dataSubjectTest, DataTest), dataActivityTest)
Merge <- cbind(training_data, test_data)

labels <- rbind(rbind(Features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(merge) <- labels

#2  Extracts only the measurements on the mean and standard deviation for each measurement. 
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

#3 Uses descriptive activity names to name the activities in the data set

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 
##enter name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

#4 Appropriately labels the data set with descriptive variable names. 
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))
#Names before
head(str(dataTable),2)
names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))
# Names after
head(str(dataTable),6)
write.table(dataTable, "TidyData.txt", row.name=FALSE)
