run_analysis <- function(){
  ## set up file paths
  rootdir<-"UCI HAR Dataset/"
  testdir<-paste(rootdir,"test/", sep="")
  traindir<-paste(rootdir,"train/", sep="")
  
  ## read labels
  
  labels_file<-paste(rootdir, "activity_labels.txt", sep="")
  labels<-read.table(labels_file)
  
  
  print(labels)
  
  ## read features
  
  features_file<-paste(rootdir, "features.txt", sep="")
  print(features_file)
  features<-read.table(features_file)
  
  ##creates a vector indicating which columns contain mean values
  mean_true<-grepl("mean",features[,2])
  ##print(mean_true)
  ##print(features[,2])
  
  ##creates a vector indicating which columns contain standard dev. values
  std_true<-grepl("std",features[,2])
  
  ##combines the previous vectors to indicate the columns I want for the mean and std values.
  my_true<-mean_true | std_true
  print(my_true)
  
  ## read measures
  
  measures_file<-paste(rootdir, "features_info.txt", sep="")
  print(measures_file)
  measures<-read.table(measures_file, skip=12, nrows=17)
  
  ##print(measures)
  
  ##measures_2<-read.table(measures_file, skip=33, nrows=16)
  
  ##print(measures_2)
  ##
  
  ##measures_3<-read.table(measures_file, skip=53, nrows=5)
  
##  print(measures_3)
measures_file<-paste(rootdir, "/test/y_test.txt", sep="")
print(measures_file)
measures<-read.table(measures_file)
print(dim(measures))
##print(measures)


}