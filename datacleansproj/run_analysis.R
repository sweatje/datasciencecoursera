#You should create one R script called run_analysis.R that does the following. 
library(data.table)
library(stringr)
library(plyr)


#1 Merges the training and the test sets to create one data set.
# files appear fixed with text
# train folder files all have 7352 rows
# 561 observations in X_train.txt 561 lines in features.txt - assuming features.txt is row lables
# test folder files all have 2947 rows

#read lables
activity_labels <- fread("activity_labels.txt")
setnames(activity_labels,names(activity_labels),c("activity_id","activity_name"))
feature_labels <- fread("features.txt")
setnames(feature_labels,names(feature_labels),c("feature_id","feature_name"))

#read training data
train_subject <- fread("train/subject_train.txt")
setnames(train_subject,names(train_subject),c("subject_id"))
train_activity <- fread("train/y_train.txt")
setnames(train_activity,names(train_activity),c("activity_id"))
#memory crash on work notebook
train.df = read.fwf("train/X_train.txt", rep(16,561), col.names=feature_labels$feature_name)
train <- data.table(train.df)
train$activity_id <- train_activity
train$subject_id <- train_subject
train <- merge(train,activity_labels,by="activity_id")

test_subject <- fread("test/subject_test.txt")
setnames(test_subject,names(test_subject),c("subject_id"))
test_activity <- fread("test/y_test.txt")
setnames(test_activity,names(test_activity),c("activity_id"))
test.df = read.fwf("test/X_test.txt", rep(16,561), col.names=feature_labels$feature_name)
test <- data.table(test.df)
test$activity_id <- test_activity
test$subject_id <- test_subject
test <- merge(test,activity_labels,by="activity_id")
dataall <- rbind(train,test)

#2 Extracts only the measurements on the mean and standard deviation for each measurement. 
str_to_colname <- function(x) { str_replace_all(x, "([-,\\(\\)])", ".")}
feature_labels$colname <- sapply(feature_labels$feature_name,FUN=str_to_colname)
features_to_extract <- feature_labels[grep("mean()|std()",feature_name),colname]
features_to_extract <- append(c("subject_id","activity_name"),features_to_extract)
# method of extraction located on 
# http://stackoverflow.com/questions/13383840/select-multiple-columns-in-data-table-r
data <- dataall[,.SD,.SDcols=features_to_extract]


#3 Uses descriptive activity names to name the activities in the data set
# data$activity_name

#4 Appropriately labels the data set with descriptive variable names. 
# from feature_labels

#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#tidycols <- c("subject_id","activity_name","variable","average")
tidy <- data.table()
for (subject in unique(data$subject_id)) {
	for (activity in unique(data[data$subject_id == subject,activity_name])) {
		avgsubset <- data[data$subject_id == subject & data$activity_name == activity, ]
		for (col in  feature_labels[grep("mean()|std()",feature_name),colname]) {
			avg <- mean(sapply(avgsubset[,.SD,.SDcols=c(col)],as.numeric))
			tidy <- rbind(tidy,as.list(c(subject_id=subject, activity_id=activity, variable=col, average=avg)))

		}
	}
}
#names(tidy) <- c("subject_id","activity_name","variable","average")


