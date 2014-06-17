run_analysis <- function(){
  ## set up file paths
  rootdir<-"UCI HAR Dataset/"
  testdir<-paste(rootdir,"test/", sep="")
  traindir<-paste(rootdir,"train/", sep="")
  
  ## read labels
  
  labels_file<-paste(rootdir, "activity_labels.txt", sep="")
  
  print(labels_file)
  
  labels<-read.table(labels_file)
  
  
  labels
}