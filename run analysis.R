# Getting-and-Cleaning-data-course-project
require(plyr)

# Directories and loading
data <- "UCI\ HAR\ Dataset"
Feature <- paste(data, "/features.txt", sep = "")
activity_labels_data <- paste(data, "/activity_labels.txt", sep = "")
x_train_file <- paste(data, "/train/X_train.txt", sep = "")
y_train_file <- paste(data, "/train/y_train.txt", sep = "")
subject_train_file <- paste(data, "/train/subject_train.txt", sep = "")
x_test_file  <- paste(data, "/test/X_test.txt", sep = "")
y_test_file  <- paste(data, "/test/y_test.txt", sep = "")
subject_test_file <- paste(data, "/test/subject_test.txt", sep = "")
Features <- read.table(Feature, colClasses = c("character"))
activity_labels <- read.table(activity_labels_file, col.names = c("ActivityId", "Activity"))
x_train <- read.table(x_train_file)
y_train <- read.table(y_train_file)
subject_train <- read.table(subject_train_file)
x_test <- read.table(x_test_file)
y_test <- read.table(y_test_file)
subject_test <- read.table(subject_test_file)

##################################################################
# 1. Merges the training and the test sets to create one data set.
##################################################################
training_data <- cbind(cbind(x_train, subject_train), y_train)
test_data <- cbind(cbind(x_test, subject_test), y_test)
all_data <- rbind(training_data, test_data)
sensor_labels <- rbind(rbind(Features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels

############################################################################################
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
############################################################################################

data_mean_std <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]

###########################################################################
# 3. Uses descriptive activity names to name the activities in the data set
###########################################################################

data_mean_std <- join(data_mean_std, activity_labels, by = "ActivityId", match = "first")
data_mean_std <- data_mean_std[,-1]

##############################################################
# 4. Appropriately labels the data set with descriptive names.
##############################################################

names(data_mean_std) <- make.names(names(data_mean_std))
names(data_mean_std) <- gsub('Acc',"Acceleration",names(data_mean_std))
names(data_mean_std) <- gsub('GyroJerk',"AngularAcceleration",names(data_mean_std))
names(data_mean_std) <- gsub('Gyro',"AngularSpeed",names(data_mean_std))
names(data_mean_std) <- gsub('Mag',"Magnitude",names(data_mean_std))
names(data_mean_std) <- gsub('^t',"TimeDomain.",names(data_mean_std))
names(data_mean_std) <- gsub('^f',"FrequencyDomain.",names(data_mean_std))
names(data_mean_std) <- gsub('\\.mean',".Mean",names(data_mean_std))
names(data_mean_std) <- gsub('\\.std',".StandardDeviation",names(data_mean_std))
names(data_mean_std) <- gsub('Freq\\.',"Frequency.",names(data_mean_std))
names(data_mean_std) <- gsub('Freq$',"Frequency",names(data_mean_std))

######################################################################################################################
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
######################################################################################################################

data_avg_by_act_sub = ddply(data_mean_std, c("Subject","Activity"), numcolwise(mean))
write.table(sensor_avg_by_act_sub, file = "data_avg_by_act_sub.txt")
