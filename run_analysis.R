run_analysis <- function(){
 ## library (sqldf)
  rootdir<-"UCI HAR Dataset/"
  
  dataset1(rootdir)
  
  dataset2(rootdir)
}
  
dataset1 <- function(rootdir){
    ## set up file paths
  
  
  ## read activity types and labels
  
  labels_file<-paste(rootdir, "activity_labels.txt", sep="")
  labels<-data.frame(read.table(labels_file))
  labels<-data.frame("act_id" = labels[,1], "Activity"=labels[,2])
   
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
  
  df<-data.frame(measure_labels)
  df<-data.frame(t(df))
  
  ## adds a placeholder for the activity ID i will insert
  df<-data.frame("act_id",df )
  
  output_file<-paste(rootdir, "combo_file.csv")
  
  #insert headers
  write.table(df, file=output_file, sep=",",row.names=FALSE, col.names=FALSE)
  
  
  ## mergers the data sets by appending the test and train files to the header row and
  ## inserts the y data for the activity key 
  
  
  ## insert test data by reading it in, isolating it to the mean and std values, and combining the activity keys
  
  measures_file<-paste(rootdir, "/test/x_test.txt", sep="")
  measures<-read.table(measures_file)
  my_measures<-data.frame(measures[,my_true])
  
  activity_file<-paste(rootdir, "/test/y_test.txt", sep="")
  activities<-read.table(activity_file)
  
  my_set<-data.frame(activities,my_measures)
    
  write.table(my_set, file=output_file, append=TRUE, col.names=FALSE, sep=",",row.names=FALSE)
  
  
  ## insert training data
  measures_file<-paste(rootdir, "/train/x_train.txt", sep="")
  measures<-read.table(measures_file)

  my_measures<-data.frame(measures[,my_true])
  
  activity_file<-paste(rootdir, "/train/y_train.txt", sep="")
  activities<-read.table(activity_file)
  
  my_set<-data.frame(activities,my_measures)
  
  
  write.table(my_set, file=output_file, append=TRUE, col.names=FALSE, sep=",",row.names=FALSE)
  
  ## this block tidies up the data by converting the activity ids to the descriptions
    
  my_set2<-read.csv(output_file)

  tidy_set<-merge(labels, my_set2, by="act_id")
  
  output_file2<-paste(rootdir, "tidy_file.csv")
  write.table(tidy_set[,2:80], file=output_file2, sep=",",row.names=FALSE)

}
## This function creates the second tidy data set.

dataset2 <- function(rootdir){
  df1<-dataset2build("test")
  df2<-dataset2build("train")
  
  output_file3<-paste(rootdir, "tidytemp_file.csv")
  write.table(df1, file=output_file3, sep=",",row.names=FALSE)
  write.table(df2, file=output_file3, append=TRUE, col.names=FALSE, sep=",",row.names=FALSE)
  
  
  tidy_set2<-read.csv(file=output_file3)
  
  ## This select groups the means by the activity and subject
  
  consol<-sqldf("select Activity, subject, avg(body_acc_x), avg(body_acc_y), avg(body_acc_z), 
  avg(body_gyro_x), avg(body_gyro_y), avg(body_gyro_z), avg(total_acc_x), avg(total_acc_y),
  avg(total_acc_z)from tidy_set2 group by act_id, subject")
  
 ## This write the data to the file 
  
  output_file4<-paste(rootdir, "tidy_file2.csv")  
  write.table(consol, file=output_file4, sep=",",row.names=FALSE)
  
  
}  

dataset2build <- function(area){
  ## set up file paths
  rootdir<-"UCI HAR Dataset/"
  ## read activity types and labels
  
  
  labels_file<-paste(rootdir, "activity_labels.txt", sep="")
  labels<-data.frame(read.table(labels_file))
  labels<-data.frame("act_id" = labels[,1], "Activity"=labels[,2])
  
  ## read subjects
  
  subjects_file<-paste(rootdir, "/", area, "/subject_", area, ".txt", sep="")
  subjects<-read.table(subjects_file)
  subjects<-data.frame(subject = subjects[,1])
  ##print(subjects[1:10,])
  print(subjects_file)
  
  ## read Activity codes
  activity_file<-paste(rootdir, "/", area, "/y_", area, ".txt", sep="")
  activities<-read.table(activity_file)
  activities<-data.frame(act_id = activities[,1])
  ##print (activities[311:340,]) 
  ##print(dim(activities))
  ##print(activity_file)
  
  combo<-data.frame(subjects,activities)
  ##print (combo[1:10,])
  
  ## read test raw data
  
  factor_name<-"body_acc_x"
  input<-paste(rootdir, "/", area, "/Inertial Signals/", factor_name, "_", area, ".txt", sep="")
  raw_data<-data.frame(read.table(input))
  row_means=data.frame(body_acc_x=rowMeans(raw_data))
  combo<-data.frame(combo,row_means)
  
  factor_name<-"body_acc_y"
  input<-paste(rootdir, "/", area, "/Inertial Signals/", factor_name, "_", area, ".txt", sep="")
  raw_data<-data.frame(read.table(input))
  row_means=data.frame(body_acc_y=rowMeans(raw_data))
  combo<-data.frame(combo,row_means)
  
  factor_name<-"body_acc_z"
  input<-paste(rootdir, "/", area, "/Inertial Signals/", factor_name, "_", area, ".txt", sep="")
  raw_data<-data.frame(read.table(input))
  row_means=data.frame(body_acc_z=rowMeans(raw_data))
  combo<-data.frame(combo,row_means)
  
  
  factor_name<-"body_gyro_x"
  input<-paste(rootdir, "/", area, "/Inertial Signals/", factor_name, "_", area, ".txt", sep="")
  raw_data<-data.frame(read.table(input))
  row_means=data.frame(body_gyro_x=rowMeans(raw_data))
  combo<-data.frame(combo,row_means)
  
  factor_name<-"body_gyro_y"
  input<-paste(rootdir, "/", area, "/Inertial Signals/", factor_name, "_", area, ".txt", sep="")
  raw_data<-data.frame(read.table(input))
  row_means=data.frame(body_gyro_y=rowMeans(raw_data))
  combo<-data.frame(combo,row_means)
  
  factor_name<-"body_gyro_z"
  input<-paste(rootdir, "/", area, "/Inertial Signals/", factor_name, "_", area, ".txt", sep="")
  raw_data<-data.frame(read.table(input))
  row_means=data.frame(body_gyro_z=rowMeans(raw_data))
  combo<-data.frame(combo,row_means)
  
  factor_name<-"total_acc_x"
  input<-paste(rootdir, "/", area, "/Inertial Signals/", factor_name, "_", area, ".txt", sep="")
  raw_data<-data.frame(read.table(input))
  row_means=data.frame(total_acc_x=rowMeans(raw_data))
  combo<-data.frame(combo,row_means)
  
  
  factor_name<-"total_acc_y"
  input<-paste(rootdir, "/", area, "/Inertial Signals/", factor_name, "_", area, ".txt", sep="")
  raw_data<-data.frame(read.table(input))
  row_means=data.frame(total_acc_y=rowMeans(raw_data))
  combo<-data.frame(combo,row_means)
  
  factor_name<-"total_acc_z"
  input<-paste(rootdir, "/", area, "/Inertial Signals/", factor_name, "_", area, ".txt", sep="")
  raw_data<-data.frame(read.table(input))
  row_means=data.frame(total_acc_z=rowMeans(raw_data))
  combo<-data.frame(combo,row_means)
  tidy_set<-merge(labels, combo, by="act_id")
  
}
