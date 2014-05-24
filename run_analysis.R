run_analysis<-function() {
  ##Merges the training and the test sets to create one data set.
  install.packages("stringr")
  library("stringr", lib.loc="C:/Users/Jim/Documents/R/win-library/3.0")
  install.packages("data.table")
  library("data.table", lib.loc="C:/Users/Jim/Documents/R/win-library/3.0")
  X_train <- read.table("~/Coursera_CleanData-CourseProj/UCI HAR Dataset/train/X_train.txt", quote="\"")
  y_train <- read.table("~/Coursera_CleanData-CourseProj/UCI HAR Dataset/train/y_train.txt", quote="\"")
  subject_train <- read.table("~/Coursera_CleanData-CourseProj/UCI HAR Dataset/train/subject_train.txt", quote="\"")
  X_test <- read.table("~/Coursera_CleanData-CourseProj/UCI HAR Dataset/test/X_test.txt", quote="\"")
  y_test <- read.table("~/Coursera_CleanData-CourseProj/UCI HAR Dataset/test/y_test.txt", quote="\"")
  subject_test <- read.table("~/Coursera_CleanData-CourseProj/UCI HAR Dataset/test/subject_test.txt", quote="\"")
  train<-c(subject_train,y_train,X_train)
  test<-c(subject_test,y_test,X_test)
  combo<-mapply(c, train, test, SIMPLIFY=FALSE)
  features <- read.table("~/Coursera_CleanData-CourseProj/UCI HAR Dataset/features.txt", quote="\"")
  features$V2<-as.character(features$V2)
  myNames<-features[,2]
  myNames<-str_replace_all(myNames, "[()]", "")
  myNames<-str_replace_all(myNames, "[-]", "_")
  myNames<-c("subjectID","activityID",myNames)
  names(combo)[1:563]<-myNames
  
  ##Uses descriptive activity names to name the activities in the data set
  combo$activityID<-gsub(1, "WALKING", combo$activityID)
  combo$activityID<-gsub(2, "WALKING_UPSTAIRS", combo$activityID)
  combo$activityID<-gsub(3, "WALKING_DOWNSTAIRS", combo$activityID)
  combo$activityID<-gsub(4, "SITTING", combo$activityID)
  combo$activityID<-gsub(5, "STANDING", combo$activityID)
  combo$activityID<-gsub(6, "LAYING", combo$activityID)
  names(combo)[2]<-"activity" 
  
  ##Extracts only the measurements on the mean and standard deviation for each measurement.
  keyColumns<-sort(c(1,2,grep("mean()",names(combo)),grep("std()",names(combo))))
  smallCombo<-combo[keyColumns]
  
  ##Appropriately labels the data set with descriptive activity names. 
  myNames<-str_replace_all(myNames, "[-]", ".")
  names(combo)[1:563]<-myNames
  
  ##Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
  install.packages("data.table")
  library("data.table", lib.loc="C:/Users/Jim/Documents/R/win-library/3.0")
  smallComboDT<-data.table(smallCombo)
  write.table(data.frame(tidyData),file="tidyData.txt",row.names=FALSE,sep=",")
}