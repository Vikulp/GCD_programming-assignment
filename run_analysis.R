# You should create one R script called run_analysis.R that does the following. 

# we are reading test case files in to test data frames
test_sub <- read.table("UCI HAR Dataset/test/subject_test.txt")
test_feature <- read.table("UCI HAR Dataset/test/X_test.txt")
test_activity <- read.table("UCI HAR Dataset/test/Y_test.txt")
test_body_acc_x <- read.table("UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt")
test_body_acc_y <- read.table("UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt")
test_body_acc_z <- read.table("UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt")
test_body_gyro_x <- read.table("UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt")
test_body_gyro_y <- read.table("UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt")
test_body_gyro_z <- read.table("UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt")
test_total_acc_x <- read.table("UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt")
test_total_acc_y <- read.table("UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt")
test_total_acc_z <- read.table("UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt")

# we are reading train case files in to train data frames
train_sub <- read.table("UCI HAR Dataset/train/subject_train.txt")
train_feature <- read.table("UCI HAR Dataset/train/X_train.txt")
train_activity <- read.table("UCI HAR Dataset/train/Y_train.txt")
train_body_acc_x <- read.table("UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt")
train_body_acc_y <- read.table("UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt")
train_body_acc_z <- read.table("UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt")
train_body_gyro_x <- read.table("UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt")
train_body_gyro_y <- read.table("UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt")
train_body_gyro_z <- read.table("UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt")
train_total_acc_x <- read.table("UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt")
train_total_acc_y <- read.table("UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt")
train_total_acc_z <- read.table("UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt")

# we are combining all test files in to one file by columns
test_final<-cbind(test_activity,test_sub,test_feature,test_body_acc_x,test_body_acc_y,test_body_acc_z,test_body_gyro_x,test_body_gyro_y,test_body_gyro_z,test_total_acc_x,test_total_acc_y,test_total_acc_z)

# we are combining all train files in to one file by columns
train_final<-cbind(train_activity,train_sub,train_feature,train_body_acc_x,train_body_acc_y,train_body_acc_z,train_body_gyro_x,train_body_gyro_y,train_body_gyro_z,train_total_acc_x,train_total_acc_y,train_total_acc_z)
###===============================
### QUESTION 1:#Merges the training and the test sets to create one data set.
###===============================
# we are combining all test and train data by rows. now we have complete activity/subject data set for all 561/128 variables.
final<-rbind(test_final,train_final)

# we are assigning names to first two columns
colnames(final)<-c("activity","subject")

###===============================
### QUESTION 4:#Appropriately labels the data set with descriptive variable names. 
###===============================
# we are sorting the combined data set based on first two columns, We are labelling the activity names as well
final_sort<-final[order(final[,1],final[,2]), ]
final_sort$activity <- factor(final_sort$activity,
                    levels = c(1,2,3,4,5,6),
                    labels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))


# reading all features in to a character data frames
features <- read.table("UCI HAR Dataset/features.txt")
features[, 2] <- sapply(features[, 2], as.character)

###===============================
### QUESTION 3:#Uses descriptive activity names to name the activities in the data set
###===============================
# assigning all 561 features names to final sorted/labled data frame
for(i in 1:561)
{
  names(final_sort)[i+2]<-features[i,2]
}

###===============================
### QUESTION 2:#Extracts only the measurements on the mean and standard deviation for each measurement.
###===============================
# further selecting the columns base on mean and std data values, keeping first two activity and subject columns as it is
d<-features[,2]
c<-c("mean","std")
mean_vec<-d[grepl("mean",d,ignore.case=TRUE)]
std_vec<-d[grepl("std",d,ignore.case=TRUE)]
col_vec<-c("activity","subject",mean_vec,std_vec)
final_sort_sub<-final_sort[,col_vec]

###===============================
### QUESTION 5:#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
###===============================
# averaging the final mean and std data set based on activity and subjects
avg_activity_subject <- aggregate(x = final_sort_sub, by = list(AVG_activity=final_sort_sub$activity, AVG_subject=final_sort_sub$subject), FUN = "mean", na.rm = T)
Final_tidy_data<-avg_activity_subject[,-(3:4)]

# writing the final obtained tidy data set
write.matrix(Final_tidy_data, file="Final_tidy_data.txt", sep="   ")


 





