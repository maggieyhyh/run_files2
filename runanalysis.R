
library(dplyr)

## Load all the data into R memory #####

features <- read.table('/Users/hongmaggie/Downloads/UCI HAR Dataset/features.txt', col.names = c ("n", "feature"))
activities <- read.table('/Users/hongmaggie/Downloads/UCI HAR Dataset/activity_labels.txt', col.names = c("class_label", "activity"))
subject_test <- read.table("/Users/hongmaggie/Downloads/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("/Users/hongmaggie/Downloads/UCI HAR Dataset/test/X_test.txt", col.names = features$feature)
y_test <- read.table("/Users/hongmaggie/Downloads/UCI HAR Dataset/test/y_test.txt", col.names = "class_label")
subject_train <- read.table("/Users/hongmaggie/Downloads/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("/Users/hongmaggie/Downloads/UCI HAR Dataset/train/X_train.txt", col.names = features$feature)
y_train <- read.table("/Users/hongmaggie/Downloads/UCI HAR Dataset/train/y_train.txt", col.names = "class_label")


## Merge the training and the test sets to create one data set. ####

x_merged <- rbind(x_train, x_test)
y_merged <- rbind(y_train, y_test)
subject_merged <- rbind(subject_train, subject_test)
merged_data <- cbind(subject_merged, x_merged, y_merged)


## Extracts only the measurements on the mean and standard deviation for each measurement. ####

mean_sd_merged_data <- select(.data = merged_data, subject, class_label, contains("mean"), contains("sd"))

## Uses descriptive activity names to name the activities in the data set ####
mean_sd_merged_data <- left_join(mean_sd_merged_data, activities, by="class_label")

## Appropriately labels the data set with descriptive variable names. ####
names(mean_sd_merged_data)[2] <- "activity_class_label"
names(mean_sd_merged_data)<-gsub("Acc", "accelerometer", names(mean_sd_merged_data))
names(mean_sd_merged_data)<-gsub("Gyro", "gyroscope", names(mean_sd_merged_data))
names(mean_sd_merged_data)<-gsub("BodyBody", "body", names(mean_sd_merged_data))
names(mean_sd_merged_data)<-gsub("Mag", "magnitude", names(mean_sd_merged_data))
names(mean_sd_merged_data)<-gsub("^t", "time", names(mean_sd_merged_data))
names(mean_sd_merged_data)<-gsub("^f", "frequency", names(mean_sd_merged_data))
names(mean_sd_merged_data)<-gsub("tBody", "timeBody", names(mean_sd_merged_data))
names(mean_sd_merged_data)<-gsub("-mean()", "mean", names(mean_sd_merged_data), ignore.case = TRUE)
names(mean_sd_merged_data)<-gsub("-std()", "std", names(mean_sd_merged_data), ignore.case = TRUE)
names(mean_sd_merged_data)<-gsub("-freq()", "frequency", names(mean_sd_merged_data), ignore.case = TRUE)
names(mean_sd_merged_data)<-gsub("angle", "angle", names(mean_sd_merged_data))
names(mean_sd_merged_data)<-gsub("gravity", "gravity", names(mean_sd_merged_data))


## From the data set in step 4, creates a second, independent tidy data set ###

independented_tidy_data_step5 <- mean_sd_merged_data %>%
  group_by(subject, activity) %>%
  summarise(across(where(is.numeric), mean))


### Please upload the tidy data set created in step 5 of the instructions. 
### Please upload your data set as a txt file created with write.table() using row.name=FALSE 

write.table(x=independented_tidy_data_step5, file = '/Users/hongmaggie/Downloads/UCI HAR Dataset/independented_tidy_data_step5.txt', row.name=FALSE)


## This data set is an independent tidy data set with the average of each variable for each activity and each subject.