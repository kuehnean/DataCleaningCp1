#run_analysis.R
#Data cleaning - course project 1
#This script performs the following tasks on the wearable computing data of Samsung galaxy
#smartphones (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip).
#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each
#variable for each activity and each subject.

#donwload and unzip the dataset (if you have downloaded the dataset and unzipped it allread you can comment those lines)
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "FUCIDataset.zip")
unzip("FUCIDataset.zip")

## Read the labes for activity and features
# read activity labels
activity_labels <- read.table(file="UCI HAR Dataset/activity_labels.txt",sep=" ")
# read feature names
features <- read.table(file="UCI HAR Dataset/features.txt",sep=" ")
## Read the datasets and add directly descriptive feature names (-> Q4)
# read test data
subTrain_test <- read.table(file="UCI HAR Dataset/test/subject_test.txt",sep=" ")
x_test <- read.table(file="UCI HAR Dataset/test/X_test.txt",col.names = features[,2])
y_test <- read.table(file="UCI HAR Dataset/test/y_test.txt",sep=" ")

## read training data
subTrain_train <- read.table(file="UCI HAR Dataset/train/subject_train.txt",sep=" ")
#load x_train and directly add features as column names (->Q4)
x_train <- read.table(file="UCI HAR Dataset/train/X_train.txt",col.names = features[,2])
y_train <- read.table(file="UCI HAR Dataset/train/y_train.txt",sep=" ")

#reduce x_test and x_train to get only mean and std (-> Q2)
idxMeanAndSTD <- grep("mean|std",names(x_test))
x_testReduced <-x_test[,idxMeanAndSTD]
x_trainReduced <-x_train[,idxMeanAndSTD]

# Test data: make vector with appropriate activity labels (->Q3) and 
# merge subject, activityType and the test measurements (->Q1, Q4)
activityType = activity_labels[y_test[[1]],2]
subject <-subTrain_test$V1;
testCombined <- data.frame(cbind(subject,activityType,x_testReduced))

# Training data: make vector with appropriate activity labels (->Q3) and 
# merge subject, activityType and the test measurements (->Q1, Q4)
subject <-subTrain_train$V1;
activityType = activity_labels[y_train[[1]],2]
trainCombined <- data.frame(cbind(subject,activityType,x_trainReduced))

# merge test and training data (->Q1)
completeData <- merge(testCombined,trainCombined,all=TRUE)

# calculate the average per person and activity (->Q5)
avgData <- aggregate( . ~ subject + activityType, completeData, mean )

# export the averaged table to txt file (Addresses question 5)
write.table(avgData, "avgData_perSubjectperActivity.txt", row.names = FALSE)