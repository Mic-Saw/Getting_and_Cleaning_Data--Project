run_analysis <- function(folder_path="./Downloads/UCI HAR Dataset/") {
  library(dplyr)
  # Load text files into data frames
  activites <- read.table(paste0(folder_path,"activity_labels.txt"), col.names = c("activity_id", "activity"))
  features <- read.table(paste0(folder_path,"features.txt"), col.names = c("feature_id", "feature"))
  subject_test <- read.table(paste0(folder_path,"test/subject_test.txt"), col.names = "subject")
  x_test <- read.table(paste0(folder_path,"test/X_test.txt"))
  y_test <- read.table(paste0(folder_path,"test/y_test.txt"), col.names = "label")
  subject_train <- read.table(paste0(folder_path,"train/subject_train.txt"), col.names = "subject")
  x_train <- read.table(paste0(folder_path,"train/X_train.txt"))
  y_train <- read.table(paste0(folder_path,"train/y_train.txt"), col.names = "label")
  
  # Rename columns
  colnames(x_test) <- features$feature
  colnames(x_train) <- features$feature
  
 # Select of columns with mean and std values
  x_test <- x_test[, grepl("mean|std",colnames(x_test))]
  x_train <- x_train[, grepl("mean|std",colnames(x_train))]
  
  # Merge data
  test_data <- cbind(subject_test, y_test, x_test)
  train_data <- cbind(subject_train, y_train, x_train)
  all_data <- rbind(test_data,train_data)
  
  # Replace activity number with activity name
  all_data <- all_data %>%
    mutate(label = activites[all_data$label, 2])

  # Rename columns with more descriptive variable names
  names(all_data)[2] <- "activity"
  names(all_data) <- gsub("-mean", "_mean", names(all_data))
  names(all_data) <- gsub("-std", "_std", names(all_data))
  names(all_data) <- gsub( "Acc", "_accelerometer", names(all_data))
  names(all_data) <- gsub("Gyro", "_gyroscope", names(all_data))
  names(all_data) <- gsub("BodyBody", "Body", names(all_data))
  glimpse(all_data)
  
  # Calculate column average by subject and activity
  avg_of_columns <- all_data %>%
    group_by(subject, activity) %>%
    summarise_all(mean)
  
  # Export the merged and tidy data, and the data with averages.
  
  write.table(all_data, file = "tidy_data.txt", row.names = FALSE)
  write.table(avg_of_columns, "averages.txt", row.names = FALSE)
}