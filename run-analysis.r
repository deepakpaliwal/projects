
library(dplyr)
source('helper.R')

working_dir <- getwd()
data_file <- downloadDataset()
file_list <- unpackDataset(data_file)
train <- list()
train$data <- getFilePath("X_train", file_list)
train$activity <- getFilePath("/y_train.txt", file_list)
train$subjects <- getFilePath("subject_train.txt", file_list)
test <- list()
test$data <- getFilePath("X_test", file_list)
test$activity <- getFilePath("/y_test.txt", file_list)
test$subjects <- getFilePath("subject_test.txt", file_list)
features_path <- getFilePath("features.txt", file_list)
activity_labels_path <- getFilePath("activity_labels.txt", file_list)
train$columns <- features_path
train$labels <- activity_labels_path
test$columns <- features_path
test$labels <- activity_labels_path
training_set <- read_handy(train)
training_set <- select(training_set, Subject, Activity, contains("mean"), contains("std"), -contains("meanFreq"), -contains("gravityMean"), -starts_with("Angle"))
test_set <- read_handy(test)
test_set <- select(test_set, Subject, Activity, contains("mean"), contains("std"), -contains("meanFreq"), -contains("gravityMean"), -starts_with("Angle"))
merged_set <- bind_rows(training_set, test_set)
rm(training_set)
rm(test_set)

tidy_dataset <- merged_set %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))

write.table(tidy_dataset, "tidy_dataset.txt", row.name=FALSE)
