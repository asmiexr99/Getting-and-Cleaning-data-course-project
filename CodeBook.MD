The source data was collected from the UCI Machine Learning Repository to complete an assignment for a Coursera course named Getting and Cleaning Data instructed by Jeff Leek. The assignment involved working with the data set and producing tidy data representation of the source data. Below is a list of the operations done to achieve the outputs.

Downloaded the data set
Unzipped the data set into my chosen working directory
Loaded test and training data sets into data frames
Loaded source variable names for test and training data sets
Loaded activity labels
Combined test and training data frames using rbind
Paired down the data frames to only include the mean and standard deviation variables
Replaced activity IDs with the activity labels for readability
Combined the data frames to produce one data frame containing the subjects, measurements and activities
Produced "merged_tidy_data.txt" with the combined data frame as the first expected output
Created another data set using the data.table library to easily group the tidy data by subject and activity
Then applied the mean and standard deviation calculations across the groups
Produced "calculated_tidy_data.txt" as the second expected output
