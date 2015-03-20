  run_analysis <- function(X_test, X_train, y_test, y_train, features, subject_test, subject_train)
  {
    data <- rbind(X_test, X_train) #Merges the training and the test sets to create one data set
    c <- (grepl("mean", features)) | (grepl("std", features)) 
    data <- data[,c] #Extracts only the measurements on the mean and standard deviation for each measurement. 
    activity_labels <- rbind(y_test, y_train)
    for (i in 1:10299){
      if(activity_labels[i,1]==1){activity_labels[i,1]="WALKING"}
      if(activity_labels[i,1]==2){activity_labels[i,1]="WALKING_UPSTAIRS"}
      if(activity_labels[i,1]==3){activity_labels[i,1]="WALKING_DOWNSTAIRS"}
      if(activity_labels[i,1]==4){activity_labels[i,1]="SITTING"}
      if(activity_labels[i,1]==5){activity_labels[i,1]="STANDING"}
      if(activity_labels[i,1]==6){activity_labels[i,1]="LAYING"}
    } #Uses descriptive activity names to name the activities in the data set
    names(data) <- features[c] #Appropriately labels the data set with descriptive variable names.
   subject <- rbind(subject_test, subject_train)
   names(activity_labels) <- "activity"
   names(subject) <- "subject"
   data <- cbind(subject, activity_labels, data)
   data2 <- group_by(data, subject, activity)
   data3 <- summarise_each(data2, funs(mean))
   data3
   
   
  }