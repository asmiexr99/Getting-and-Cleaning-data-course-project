1. Merges the training and the test sets to create one data set.
After setting the source directory for the files, read into tables the data located in

features.txt
activity_labels.txt
subject_train.txt
x_train.txt
y_train.txt
subject_test.txt
x_test.txt
y_test.txt
Assign column names and merge to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
Create a logcal vector that contains TRUE values for the ID, mean and stdev columns and FALSE values for the others. Subset this data to keep only the necessary columns.
3. Uses descriptive activity names to name the activities in the data set
Merge data subset with the activityType table to cinlude the descriptive activity names
4. Appropriately labels the data set with descriptive variable names.
Use gsub function for pattern replacement to clean up the data labels
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
