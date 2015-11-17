
if (!getwd() == "./out-of-box-samples") {
    dir.create("./out-of-box-samples")
}
rm(list = ls(all = TRUE))
library(plyr) 
library(data.table) 
get <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",get)
unzip(get, list = TRUE) 
YTest <- read.table(unzip(get, "UCI HAR Dataset/test/y_test.txt"))
XTest <- read.table(unzip(get, "UCI HAR Dataset/test/X_test.txt"))
SubjectTest <- read.table(unzip(get, "UCI HAR Dataset/test/subject_test.txt"))
YTrain <- read.table(unzip(get, "UCI HAR Dataset/train/y_train.txt"))
XTrain <- read.table(unzip(get, "UCI HAR Dataset/train/X_train.txt"))
SubjectTrain <- read.table(unzip(get, "UCI HAR Dataset/train/subject_train.txt"))
Features <- read.table(unzip(get, "UCI HAR Dataset/features.txt"))
unlink(get) 
colnames(XTrain) <- t(Features[2])
colnames(XTest) <- t(Features[2])
XTrain$activities <- YTrain[, 1]
XTrain$participants <- SubjectTrain[, 1]
XTest$activities <- YTest[, 1]
XTest$participants <- SubjectTest[, 1]


# 1. Merges the training and the test sets to create one data set.
all_data <- rbind(XTrain, XTest)
duplicated<-duplicated(colnames(all_data))
all_data <- all_data[, !duplicated(colnames(all_data))]


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
data_mean <- grep("mean()", names(all_data), value = FALSE, fixed = TRUE)
dat_mean <- append(data_mean, 471:477)
MeanMatrix <- all_data[data_mean]
data_std <- grep("std()", names(all_data), value = FALSE)
STDMatrix <- all_data[data_std]


# 3. Uses descriptive activity names to name the activities in the data set
all_data$activities <- as.character(all_data$activities)
all_data$activities[all_data$activities == 1] <- "Walking"
all_data$activities[all_data$activities == 2] <- "Walking Upstairs"
all_data$activities[all_data$activities == 3] <- "Walking Downstairs"
all_data$activities[all_data$activities == 4] <- "Sitting"
all_data$activities[all_data$activities == 5] <- "Standing"
all_data$activities[all_data$activities == 6] <- "Laying"
all_data$activities <- as.factor(all_data$activities)


# 4. Appropriately labels the data set with descriptive names.
names(all_data) <- gsub("Acc", "Accelerator", names(all_data))
names(all_data) <- gsub("Mag", "Magnitude", names(all_data))
names(all_data) <- gsub("Gyro", "Gyroscope", names(all_data))
names(all_data) <- gsub("^t", "time", names(all_data))
names(all_data) <- gsub("^f", "frequency", names(all_data))
names(all_data) <- gsub("-std$", "StdDev", names(all_data))
names(all_data) <- gsub("-mean", "Mean", names(all_data))


# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
all_data_master <- data.table(all_data)
tidy_data <- all_data_master[, lapply(.SD, mean), by = 'participants,activities']
write.table(tidy_data, file = "tidy_data.txt", row.names = FALSE)


