#1 - Download the dataset
library(dplyr)

filename <- "ProjectFinal.zip"

# Checking if archieve already exists.
if (!file.exists(filename)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
    unzip(filename) 
}

#2 - Assigning all data frames
features <- read.table("./UCI HAR Dataset/features.txt")
head(features)
features <- read.table("./UCI HAR Dataset/features.txt", col.names = c("n","functions"))

activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
head(activities)
activities <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
head(subject_test)
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
head(x_test)
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)

y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
head(y_test)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)

y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#3 - Merges the training and the test sets to create one data set
x <- rbind(x_train, x_test)
y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)
data_merged <- cbind(subject, x, y)

#4 - Extracts only the measurements on the mean and standard deviation for each measurement.
tidy_data <- data_merged %>% select(subject, code, contains("mean"), contains("std"))

#5 - Uses descriptive activity names to name the activities in the data set

tidy_data$code <- activities[tidy_data$code, 2]

#6 - Appropriately labels the data set with descriptive variable names
names(tidy_data)[2] = "activity"
names(tidy_data) <- gsub("Acc", "Accelerometer", names(tidy_data))
names(tidy_data) <- gsub("Gyro", "Gyroscope", names(tidy_data))
names(tidy_data) <- gsub("BodyBody", "Body", names(tidy_data))
names(tidy_data) <- gsub("Mag", "Magnitude", names(tidy_data))
names(tidy_data) <- gsub("^t", "Time", names(tidy_data))
names(tidy_data) <- gsub("^f", "Frequency", names(tidy_data))
names(tidy_data) <- gsub("tBody", "TimeBody", names(tidy_data))
names(tidy_data) <- gsub("-mean()", "Mean", names(tidy_data), ignore.case = TRUE)
names(tidy_data) <- gsub("-std()", "STD", names(tidy_data), ignore.case = TRUE)
names(tidy_data) <- gsub("-freq()", "Frequency", names(tidy_data), ignore.case = TRUE)
names(tidy_data) <- gsub("angle", "Angle", names(tidy_data))
names(tidy_data) <- gsub("gravity", "Gravity", names(tidy_data))

#7 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
finaldata <- tidy_data %>%
    group_by(subject, activity) %>%
    summarize_all(funs(mean))
write.table(finaldata, "finaldata.txt", row.name = FALSE)

#8 - Final check
str(finaldata)

