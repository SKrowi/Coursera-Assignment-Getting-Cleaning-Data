# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 01 Downloading Data ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)

URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "Coursera_DS3_Final.zip"
download.file(URL, file, method = "curl")
unzip(file)

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")


df_x <- rbind(x_train, x_test)
df_y <- rbind(y_train, y_test)
df_subject <- rbind(subject_train, subject_test)
df_full <- cbind(df_subject, df_y, df_x) # uniting one data frame

rm(features, x_train, y_train, x_test, y_test, subject_train, subject_test, df_x, df_y, df_subject)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 02 Cleaning Data ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
glimpse(df_full)

df_tidy <- df_full %>%
  select(subject, code, contains("mean"), contains("std")) %>% # only  measurements on mean and standard deviation
  left_join(activities) %>% # joining descriptive activity names to name activities in data set
  select(subject, activity, everything()) %>% # results in a diff. variable arrangement
  within(rm(code)) # removal of activity code

glimpse(df_tidy)

new_name <- names(df_tidy) # cleaning variable names
new_name <- gsub("[(][)]", "", new_name)
# new_name <- gsub("\\..", "_", new_name)
new_name <- gsub("^t", "Time_", new_name)
new_name <- gsub("^f", "Frequency_", new_name)
new_name <- gsub("Acc", "Accelerometer", new_name)
new_name <- gsub("Gyro", "Gyroscope", new_name)
new_name <- gsub("Mag", "Magnitude", new_name)
new_name <- gsub("-mean-", "_Mean_", new_name)
new_name <- gsub("-std-", "_StandardDeviation_", new_name)
new_name <- gsub("-", "_", new_name)
new_name <- gsub("angle", "Angle", new_name)
new_name <- gsub("-freq()", "Frequency", new_name, ignore.case = TRUE)
new_name <- gsub("gravity", "Gravity", new_name)
names(df_tidy) <- new_name

colnames(df_tidy)

# independent tidy data set with the average of each variable for each activity and subject 

df_tidy_summarized <- df_tidy %>%
  group_by(subject, activity) %>% 
  summarise_at(.vars = names(.)[3:88],
               .funs = c(GroupMean = "mean"))

glimpse(df_tidy_summarized)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 03 Data Export ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write.table(df_tidy_summarized, "DataTidySummarized.txt", row.names = FALSE)
write.csv2(df_tidy_summarized, file = "DataTidySummarized.csv", row.names = FALSE, na = "") # as csv


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 04 Cleaning Environment ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
