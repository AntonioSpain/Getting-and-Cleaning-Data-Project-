# Required packages -------------------------------------------------------

if(!is.element("plyr", installed.packages()[,1])){
  print("Installing packages")
  install.packages("plyr")
}

library(plyr)


# Download data -------------------------------------------------

file <- "data.zip"
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
data_path <- "UCI HAR Dataset"
result_folder <- "results"

if(!file.exists(file)){
  
  print("downloading Data")
  download.file(url,file, mode = "wb")
  
}

if(!file.exists(result_folder)){
  print("Creating result folder")
  dir.create(result_folder)
} 

##reads a table from the zip data file and applies cols
getTable <- function (filename,cols = NULL){
  
  print(paste("Getting table:", filename))
  
  f <- unz(file, paste(data_path,filename,sep="/"))
  
  data <- data.frame()
  
  if(is.null(cols)){
    data <- read.table(f,sep="",stringsAsFactors=F)
  } else {
    data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
  }
  
  
  data
  
}

# Read table --------------------------------------------------------------

getData <- function(type, features){
  
  print(paste("Getting data", type))
  
  subject_data <- getTable(paste(type,"/","subject_",type,".txt",sep=""),"id")
  y_data <- getTable(paste(type,"/","y_",type,".txt",sep=""),"activity")    
  x_data <- getTable(paste(type,"/","X_",type,".txt",sep=""),features$V2) 
  
  return (cbind(subject_data,y_data,x_data)) 
}


# Save the data into the result folder ------------------------------------


saveResult <- function (data,name){
  
  print(paste("Saving data", name))
  
  file <- paste(result_folder, "/", name,".txt" ,sep="")
  write.table(data,file, row.name=FALSE)
}


# Get common data tables --------------------------------------------------

features <- getTable("features.txt")
train <- getData("train",features)
test <- getData("test",features)


# Merge and arrange the training and the test sets ------------------------------------

data <- rbind(train, test)
data <- arrange(data, id)


# Use appropiately labels -------------------------------------------------

activity_labels <- getTable("activity_labels.txt")
data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)


# Extract and save indicated measurements ------------------------------------------

dataset1 <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]
saveResult(dataset1,"dataset1")


# Create a second and independent tidy data set ---------------------------

dataset2 <- ddply(dataset1, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(dataset2)[-c(1:2)] <- paste(colnames(dataset2)[-c(1:2)], "_mean", sep="")
saveResult(dataset2,"dataset2")
