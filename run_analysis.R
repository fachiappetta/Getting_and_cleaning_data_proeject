library(plyr)
library(dplyr)

#download data set
if(!file.exists("data")){dir.create("data")}
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile = "./data.zip")

#unzip file
unzip(zipfile = "./data.zip", exdir = "./data")

#read features and activities
features <- read.table(file.path("./data/UCI HAR Dataset/features.txt"), col.names = c("n","functions"))
activities <- read.table(file.path("./data/UCI HAR Dataset/activity_labels.txt"), col.names = c("activity_code", "activity"))

#Read activity description datasets (y) for TEST and TRAIN
description_test <- read.table(file.path("./data/UCI HAR Dataset/test/y_test.txt"), header = F, col.names = "activity_code")
description_train <- read.table(file.path("./data/UCI HAR Dataset/train/y_train.txt"), header=F, col.names = "activity_code")

#Read subject dataset (subject) for TEST and TRAIN
subject_test <- read.table(file.path("./data/UCI HAR Dataset/test/subject_test.txt"), header=F, col.names = "subject")
subject_train <- read.table(file.path("./data/UCI HAR Dataset/train/subject_train.txt"), header=F, col.names = "subject")

#read activity dataset (x) for TEST and TRAIN
activity_test <- read.table(file.path("./data/UCI HAR Dataset/test/X_test.txt"), header=F, col.names = features$functions)
activity_train <- read.table(file.path("./data/UCI HAR Dataset/train/X_train.txt"), header=F, col.names = features$functions)

#Merges the training and the test sets to create one data set
description <- rbind(description_train, description_test)
activity <- rbind(activity_train, activity_test)
subject <- rbind(subject_train, subject_test)
merged_df <- cbind(subject, description, activity)

#extracts only the measurements on the mean and standard deviation for each measurement.
TidyData <- merged_df %>% select(subject, activity_code, contains("mean"), contains("std"))
      
#Uses descriptive activity names to name the activities in the data set
TidyData$activity_code <- activities[TidyData$activity_code,2]

#Appropriately labels the data set with descriptive variable names.
names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))

names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))

names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
TidyData_averages <- TidyData %>% group_by(subject,activity) %>% summarize_all(list(mean))

write.table(TidyData_averages, "TidyData_averages.txt", row.names = FALSE, col.names = TRUE)
      