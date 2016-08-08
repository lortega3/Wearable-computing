#data_analysis <- function() {
#colSds will require the matrixStats package:
#library(matrixStats)

#join will require the plyr package:
# library(plyr)

#run this once, as a pre-requisite
  test <- read.table("X_test.txt")
  train <- read.table("X_train.txt")

#column names
  features <- read.table("features.txt") 

#1. Merges the training and the test sets to create one data set.

  combined <- rbind(test, train)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.

  means <- colMeans(combined, na.rm = TRUE)
  stdevs <- colSds(as.matrix(combined))

  #4. Appropriately labels the data set with descriptive variable names.
  
  # ***I actually did this before #3, so it assumes the Activity_Label column I added in that step is not present yet***
  
  #replace the data-frame columns with the column descriptions in the feature data-frame
  colnames(combined) <- features[,2]
  
  #3. Uses descriptive activity names to name the activities in the data set

  # *** I actually did step #4 before this step ***
  
  test_labels <- read.table("y_test.txt")
  training_labels <- read.table("y_train.txt")

  #This will produce a labels column for the main "combined" dataset
  combined_labels <- rbind(test_labels, training_labels)

  #combine the translated ACTIVITY LABEL for each observation into one column
  activity_labels <- read.table("activity_labels.txt")
  activity_label_column <- data.frame(label=combined_labels$V1, activity=activity_labels[match(combined_labels$V1, activity_labels$V1), 2])[,2]

  #add the column to the main "combined" dataset
  combined2 <- cbind(activity_label_column[], combined)
  names(combined2)[1] <- "Activity_Label"

  #5. From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.

  #Maybe others added this early, but I'm adding the subject info here:
  subject_train <- read.table("subject_train.txt")
  subject_test <- read.table("subject_test.txt")
  
  #This will merge the subject column for the main "combined" dataset
  combined_subjects <- rbind(subject_test, subject_train)
  
  #add the SUBJECT column to the main "combined" dataset
  combined3 <- cbind(combined_subjects[], combined2)
  #for some reason the column name didn't stick so...
  names(combined3)[1] <- "Subject"
  
  #Finally, we create a tidy new data set, grouped by subject, activity, 
  #and averages for all measurements
  
  final <- aggregate(combined3[, 3:563], list(combined3$Subject, combined3$Activity_Label), mean)
  
  print(final)