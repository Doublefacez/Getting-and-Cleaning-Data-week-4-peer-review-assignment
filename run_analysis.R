#Week 4 peer assessment

library(tidyr)
library(dplyr)
library()

#extract test data

x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
sub_test <- read.table("./test/subject_test.txt")

#extract train data

x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
sub_train <- read.table ("./train/subject_train.txt")

#extract feature description and act_labels

features <- read.table("features.txt")
act_labels <- read.table("activity_labels.txt")

features
View(x_train)

View(x_train)

#Provide a column name of each set of data
#Step 4 naming the data set with descriptive activity names

colnames(x_test) = features[,2]
colnames(y_test) = "activityId"
colnames(sub_test) = "subject"
colnames(x_train) = features[,2]
colnames(y_train) = "activityId"
colnames(sub_test) = "subject"
colnames(act_labels) = c("activityId", "activityType")



#Merging train and test data

x_tt<-bind_rows(x_test, x_train)
x_tt

y_tt <- bind_rows(y_test, y_train)
y_tt

sub_tt <- bind_rows(sub_test, sub_train)
sub_tt

merge_tt <- bind_cols(x_tt, y_tt, sub_tt)
merge_tt


#Extract the mean and sd of each measurement

coltag <- colnames(merge_tt)

#Getting a subset of all activity and subject means and sd 
mean_sd <- (grepl("activityId", coltag)| grepl("subject", coltag)|grepl("mean..", coltag)|grepl("std.." , coltag))
mean_sd

mean_sd_df <- merge_tt[, mean_sd == TRUE]
mean_sd_df

#Use descriptive activity names to name the activities in the data set

act_names_df <- merge(mean_sd_df, act_labels, by = "activityId", all.x=TRUE)
act_names_df

#creates a second, independent tidy data set with the average of each variable for each activity and each subject.

sec_tidy_df <- aggregate(.~subject + activityId, act_names_df, mean)

#Setting the subject and activityID into logical order

sec_tidy_dataset <- sec_tidy_df[order(sec_tidy_df$subject, sec_tidy_df$activityId),]
sec_tidy_dataset
nrow(sec_tidy_dataset)
#creating a file for the new data set 

write.table(sec_tidy_dataset, "SecondTidyDataset.txt", row.names = FALSE)
