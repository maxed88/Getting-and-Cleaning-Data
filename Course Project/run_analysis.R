library(data.table)

fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists("./UCI HAR Dataset.zip")){
  download.file(fileUrl, destfile = "./UCI HAR Dataset.zip", mode = "wb")
  unzip("UCI HAR Dataset.zip", exdir = getwd())
}
  
## 1. Merges the training and the test sets to create one data set.
## Read data
xTrain <- read.table("./UCI HAR Dataset/train/X_train.txt", sep = "")
yTrain <- read.table("./UCI HAR Dataset/train/y_train.txt", sep = "")
subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", sep = "")

xTest <- read.table("./UCI HAR Dataset/test/X_test.txt", sep = "")
yTest <- read.table("./UCI HAR Dataset/test/y_test.txt", sep = "")
subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt", sep = "")

features <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")

## Merge data
train <- cbind(subjectTrain, yTrain, xTrain)
test <- cbind(subjectTest, yTest, xTest)
wholeData <- rbind(train, test)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
index <- grep(("mean\\(\\)|std\\(\\)"), features[, 2])
##add subject, activity, mean and standard deviation on a single data set 
subData <- wholeData[, c(1, 2, index + 2)]

## 3. Uses descriptive activity names to name the activities in the data set
## Name each variables
featureNames <- features[, 2]
names(subData) <- c("subject", "activity", featureNames[index])
subData$activity <- factor(subData$activity, levels = activityLabels[, 1], labels = activityLabels[, 2])

## 4. Appropriately labels the data set with descriptive variable names.
names(subData) <- gsub("\\()", "", names(subData))
names(subData) <- gsub("^t", "Time", names(subData))
names(subData) <- gsub("^f", "Frequency", names(subData))
names(subData) <- gsub("Acc", "Accelerometer", names(subData))
names(subData) <- gsub("Gyro", "Gyroscope", names(subData))
names(subData) <- gsub("Mag", "Magnitude", names(subData))
names(subData) <- gsub("-mean-", "Mean", names(subData))
names(subData) <- gsub("-std-", "StandardDeviation", names(subData))

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidyData <- aggregate(subData[, 3:ncol(subData)], by = list(activity = subData$activity, subject = subData$subject), FUN = mean)
write.table(tidyData, "./TidyData.txt")

##dat <- read.table("./TidyData.txt")
##View(dat)