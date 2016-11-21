# An helper file to do some stuff
require(dplyr)

# Donwload uci har dataset if needed
downloadDataset <- function(directory=tempdir()) {
  # This is the data url of an archive zip
  data_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  
  # This is the name of the downloaded file
  data_file <- "uci_har_dataset.zip"
  
  # Download data in a temporary directory if needed
  if (! file.exists(data_file)) {
    cat(data_file, "doesn't exists. Downloading it into", tempdir(), "...")
    download.file(data_url, data_file)
    cat("Done!")
  }
  
  #return data_file path
  return(data_file)
}

# Unpack data if needed
unpackDataset <- function(data_file, directory=tempdir()) {
  file_list <- unzip(data_file, list = T)
  
  # Try to determine if data_file is already unpacked
  # (check if the first element in file list exists in temporay directory)
  if (! file.exists(file.path(directory, file_list[1,1]))) {
    cat("Unzipping", data_file, "into", directory, "...")
    unzip(data_file, exdir=directory)
  }
  
  #return the file lista dataframe
  return(file_list)
}

# A function to get file path by givin' file name
getFilePath <- function(name, file_list, directory=tempdir()) {
  idx <- grep(name, file_list$Name)
  file_name <- file_list$Name[idx]
  file_path <- file.path(directory, file_name)
}


#define a function to get a label from a number. Use activity label to get labels
Activity <- function(x = data.frame()) {
  init <- function(activity_file) {
    x <<- read.table(activity_file)
  }
  
  getLabel <- function(number) {
    x[which(x$V1==number),2]
  }
  
  # return the two methods
  list(init = init, getLabel = getLabel)
}

# Read csv and return a data frame
read_handy <-function(set, features_path, labels_path) {
  # Try to handle memory in efficient way. Probe inputfile
  probe <- read.table(set$data, nrows=100);
  
  # probe column classes
  classes <- sapply(probe, class);
  
  # Now load efficiently all data. comment.char is empty cause there are no comments
  data <- read.table(set$data, colClasses = classes, comment.char = "");
  
  # Non set column names from header_file (eg. features.txt)
  colNames <- read.table(set$columns, colClasses = c("NULL", "character"));
  
  # Set header to table. Fix columns names
  tmp <- sub(pattern = "\\(\\)", replacement = "", x=as.vector(colNames[,1]), perl = T)
  tmp <- sub(pattern = "BodyBody", replacement = "Body", x=tmp, perl = T)
  
  # set column names
  colnames(data) <- make.names(tmp, unique=TRUE);
  
  # Now set row activities label
  activity <- read.table(set$activity)
  
  # label this column
  colnames(activity) <- c("Activity")
  
  # Add this column on the left
  data <- bind_cols(activity, data)
  
  # Get a kind of activity class. This is a function that I declared upper in this file
  activity <- Activity()
  activity$init(set$labels)
  
  # Now trasform activity levels in activity names
  data$Activity <- sapply(data$Activity, activity$getLabel)
  
  # Add subject ids
  subjects <- read.table(set$subjects)
  
  # label this column
  colnames(subjects) <- c("Subject")
  
  # Add this column on the left
  data <- bind_cols(subjects, data)
  
  # Now transform data in dplyr table
  data <- tbl_df(data)
  
  # Explicit return data
  return (data);
  
}