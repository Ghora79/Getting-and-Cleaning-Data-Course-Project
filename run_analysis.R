library(data.table)
library(dplyr)
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile="UCI HAR Dataset.zip", mode="wb")
#unzip the raw data sets
unzip("UCI HAR Dataset.zip", exdir = "data/projectfiles")
list.files("data/projectfiles/UCI HAR Dataset/train")
#Reading the training tables
train_data <- read.table("data/projectfiles/UCI HAR Dataset/train/X_train.txt")
train_label <- read.table("data/projectfiles/UCI HAR Dataset/train/y_train.txt", col.names = "activity")
subject_train <- read.table("data/projectfiles/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
#Reading the testing tables
test_data <- read.table("data/projectfiles/UCI HAR Dataset/test/X_test.txt")
test_label <- read.table("data/projectfiles/UCI HAR Dataset/test/y_test.txt", col.names = "activity")
subject_test <- read.table("data/projectfiles/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
#Read the features data
features <- read.table("data/projectfiles/UCI HAR Dataset/features.txt")
#Read activity labels data
activityLabels <- read.table("data/projectfiles/UCI HAR Dataset/activity_labels.txt")
colnames(activityLabels) <- c('activityId','activityType')
#combine the respective data in training and test data sets corresponding to subject, label and dataset.
subject <- rbind(subject_train, subject_test)
label <- rbind(train_label, test_label)
dataset <- rbind(train_data, test_data)
#name the columns by transposing the names given in the document features
colnames(dataset) <- transpose(features[2])

#Merges the training and the test sets to create one data set.
MergedData <- cbind(dataset, label, subject)

#Extracts only the measurements on the mean and standard deviation for each measurement. 
column_mean_sd <- grep(".*Mean.*|.*Std.*", names(MergedData), ignore.case=TRUE)
requiredColumns <- c(column_mean_sd, 562, 563)
dim(MergedData)
tidy_data <- MergedData[,requiredColumns]
dim(tidy_data)
str(tidy_data)
#Descriptive data
tidy_data$activity <- as.character(tidy_data$activity)
tidy_data$subject <- as.factor(tidy_data$subject)
for (i in 1:6){
    tidy_data$activity[tidy_data$activity == i] <- as.character(activityLabels[i,2])
}
#descriptive activity names to name the activities in the data set
names(tidy_data)<-gsub("Acc", "Accelerometer", names(tidy_data))
names(tidy_data)<-gsub("Gyro", "Gyroscope", names(tidy_data))
names(tidy_data)<-gsub("BodyBody", "Body", names(tidy_data))
names(tidy_data)<-gsub("Mag", "Magnitude", names(tidy_data))
names(tidy_data)<-gsub("^t", "Time", names(tidy_data))
names(tidy_data)<-gsub("^f", "Frequency", names(tidy_data))
names(tidy_data)<-gsub("tBody", "TimeBody", names(tidy_data))
names(tidy_data)<-gsub("-mean()", "Mean", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("-std()", "STD", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("-freq()", "Frequency", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("angle", "Angle", names(tidy_data))
names(tidy_data)<-gsub("gravity", "Gravity", names(tidy_data))
names(tidy_data)

tidy_data <- data.table(tidy_data)
#independent tidy data set with the average of each variable for each activity and each subject
tidyData <- aggregate(. ~subject + activity, tidy_data, mean)
tidyData <- tidyData[order(tidyData$subject, tidyData$activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

