
# Getting and Cleaning Data, week 4, course project

# The R script below does the following: 
# - Merges the training and the test sets to create one data set.
# - Extracts only the measurements on the mean and standard deviation for each measurement.
# - Uses descriptive activity names to name the activities in the data set
# - Appropriately labels the data set with descriptive variable names.
# - From the data set in step 4, creates a second, independent tidy data set with the average of each 
#   variable for each activity and each subject.


# Assumption: dplyr library already available


# Merges the training and the test sets to create one data set.

### first read the various tables in
trainset <- read.table("UCI HAR Dataset/train/X_train.txt")
testset <- read.table("UCI HAR Dataset/test/X_test.txt")

subjecttrain <- read.table("UCI HAR Dataset/train/subject_train.txt")
subjecttest <- read.table("UCI HAR Dataset/test/subject_test.txt")

trainlabels <- read.table("UCI HAR Dataset/train/y_train.txt")
testlabels <- read.table("UCI HAR Dataset/test/y_test.txt")

varnames <- read.table("UCI HAR Dataset/features.txt", as.is = TRUE)
activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt")

### rename the columnnames in the activitylabels set
colnames(activitylabels) <- c("activity_id", "activity_label")

### add the subject and label data frames of train and test to the test- and trainsets
trainset_ext <- bind_cols(trainset, subjecttrain, trainlabels)
testset_ext <- bind_cols(testset, subjecttest, testlabels)

### merge the extended train- and test sets into one data set
mergedset <- merge(testset_ext,trainset_ext, all=TRUE)


# Extracts only the measurements on the mean and standard deviation for each measurement.

### first convert variable names in mergedset to variable names in features.txt and 
colnames(mergedset) <- c(varnames[, 2], "subject", "activity")

### remove duplicates
mergedset <- mergedset[!duplicated(names(mergedset))]

### using select to filter out first the mean columns
submergedset <- select(mergedset, contains("-mean"))

### using bind_cols and cbind andd select to filter out the standard deviation columns and 
### then bind them to the data frame with mean columns
submergedset <- bind_cols(submergedset, select(mergedset, contains("-std")))

submergedset <- cbind(submergedset, mergedset$subject)
submergedset <- cbind(submergedset, mergedset$activity)

### rename the columns for "mergedset$subject" into subject and "mergedset$activity" into "activity"
colnames(submergedset)[80] <- "subject"
colnames(submergedset)[81] <- "activity"


# Uses descriptive activity names to name the activities in the data set

### replace activity numbers with the appropriate labels
submergedset$activity <- factor(submergedset$activity, levels = activitylabels[, 1], labels = activitylabels[, 2])


# Appropriately labels the data set with descriptive variable names.

### first put all the names in one vector
submergedset_columns <- colnames(submergedset)

### in that vector, substitute special characters for nothing to clear them from the columnnames
submergedset_columns <- gsub("[\\(\\)-]", "", submergedset_columns)

### still in that vector, write out short column names to descriptive column names
submergedset_columns <- gsub("^f", "FrequencyDomain", submergedset_columns)
submergedset_columns <- gsub("^t", "TimeDomain", submergedset_columns)
submergedset_columns <- gsub("Acc", "Accelerometer", submergedset_columns)
submergedset_columns <- gsub("Gyro", "Gyroscope", submergedset_columns)
submergedset_columns <- gsub("Mag", "Magnitude", submergedset_columns)
submergedset_columns <- gsub("meanFreq", "MeanFrequency", submergedset_columns)
submergedset_columns <- gsub("mean", "Mean", submergedset_columns)
submergedset_columns <- gsub("std", "StandardDeviation", submergedset_columns)

### use the vector with adjusted names to update the column names in the data set
colnames(submergedset) <- submergedset_columns


# From the data set in step 4, creates a second, independent tidy data set with the average of each
# variable for each activity and each subject.

### compute mean for the grouped data set (grouped by subject and activity)
submergedset_averages <- submergedset %>% 
        group_by(subject, activity) %>%
        summarise_all(funs(mean))

### write the output to a file called "TidyData.txt"
write.table(submergedset_averages, "TidyData.txt", row.names = FALSE)