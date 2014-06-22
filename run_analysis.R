run_analysis <- function(){
  ## set up file paths
  rootdir<-"UCI HAR Dataset/"
  testdir<-paste(rootdir,"test/", sep="")
  traindir<-paste(rootdir,"train/", sep="")
  ## read activity types and labels
  
  labels_file<-paste(rootdir, "activity_labels.txt", sep="")
  labels<-data.frame(read.table(labels_file))
  labels<-data.frame("act_id" = labels[,1], "Activity"=labels[,2])
  
  
  print(labels)
  
 
  ## read features
  
  features_file<-paste(rootdir, "features.txt", sep="")
  features<-read.table(features_file)
  
  ##creates a vector indicating which columns contain mean values
  mean_true<-grepl("mean",features[,2])
  
  ##creates a vector indicating which columns contain standard dev. values
  std_true<-grepl("std",features[,2])
  
  ##combines the previous vectors to indicate the columns I want for the mean and std values and isolates the labels.
  my_true<-mean_true | std_true
  measure_labels<-features[my_true,2]
  ##measure_labels<-c(measure_labels, "Activity")  
 
  df<-data.frame(measure_labels)
  df<-data.frame(t(df))
  df<-data.frame("act_id",df )
  
  
  
  ##mergers the data sets
  
  output_file<-paste(rootdir, "combo_file.csv")
  
  
  
  ##insert headers
  write.table(df, file=output_file, sep=",",row.names=FALSE, col.names=FALSE)
  
  ## insert test data
  measures_file<-paste(rootdir, "/test/x_test.txt", sep="")
  measures<-read.table(measures_file)
  my_measures<-data.frame(measures[,my_true])
  
  activity_file<-paste(rootdir, "/test/y_test.txt", sep="")
  activities<-read.table(activity_file)
  
  my_set<-data.frame(activities,my_measures)
    
  write.table(my_set, file=output_file, append=TRUE, col.names=FALSE, sep=",",row.names=FALSE)
  
  ## insert training data
  measures_file<-paste(rootdir, "/train/x_train.txt", sep="")
  ##print(measures_file)
  measures<-read.table(measures_file)

  my_measures<-data.frame(measures[,my_true])
  
  activity_file<-paste(rootdir, "/train/y_train.txt", sep="")
  activities<-read.table(activity_file)
  
  my_set<-data.frame(activities,my_measures)
  
  
  write.table(my_set, file=output_file, append=TRUE, col.names=FALSE, sep=",",row.names=FALSE)
  
  my_set2<-read.csv(output_file)
  
  tidy_set<-merge(labels, my_set2, by="act_id")
  
  output_file2<-paste(rootdir, "tidy_file.csv")
  
  
  
  ##
  write.table(tidy_set[,2:80], file=output_file2, sep=",",row.names=FALSE)

}

merge_data <- function(inputfile, outputfile){
  
  measures_file<-paste(rootdir, "/train/x_train.txt", sep="")
  print(measures_file)
  measures<-read.table(measures_file)
  
  my_measures<-data.frame(measures[,my_true])
  
  activity_file<-paste(rootdir, "/train/y_train.txt", sep="")
  activities<-read.table(activity_file)
  
  my_set<-data.frame(activities,my_measures)
  
  
  write.table(my_set, file=output_file, append=TRUE, col.names=FALSE, sep=",",row.names=FALSE)
  
  
}
  