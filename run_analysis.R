#
# Script to fulfill the instructions of the term project in "Getting
# and Cleaning Data" (Coursera, January 2015).
#
# Requirements:
#
#   1. Merge the training and the test sets to create one data set.
#   2. Extract only the measurements on the mean and standard deviation
#      for each measurement. 
#   3. Use descriptive activity names to name the activities in the data
#      set.
#   4. Appropriately label the data set with descriptive variable names.
#   5. From the data set in step 4, create a second, independent tidy data
#      set with the average of each variable for each activity and each
#      subject.
#
# Load required libraries.
#
library(dplyr)
library(tidyr)
#
# UPDATE THIS SECTION BEFORE RUNNING THE SCRIPT. This section contains
# file paths and names and other variables that you as a user might
# want or need to change. Be sure to end directories with slashes.
#
parentDataDirectory <- "../UCI HAR Dataset/"
trainingDirectory <- "train/"
testingDirectory <- "test/"
testX <- "X_test.txt"                # testing set features
testY <- "y_test.txt"                # testing set labels
testSubject <- "subject_test.txt"    # testing set subject IDs
trainX <- "X_train.txt"              # training set features
trainY <- "y_train.txt"              # training set labels
trainSubject <- "subject_train.txt"  # training set subject IDs
features <- "features.txt"           # feature names
subjectName <- "Subject"             # name to use for subject ID
labelName <- "Label"                 # name to use for label variable
#
# Read in the feature names (from the parent directory).
#
featureNames <- paste0(parentDataDirectory, features) %>%
                  # source file
                read.table                            %>%
                  # read the feature names
                getElement(2)                         %>%
                  # omit the first (index) column
                as.vector
                  # turn it into a vector
#
# Define a function to load raw data and glue it into one database.
#
# Arguments:
#   directory = path to the data
#   features = name of the feature data file
#   labels   = name of the label date file
#   subjects = name of the subject data file
#   fnames   = names to assign to the features
#              (default: global variable "featureNames")
#
# Value:
#   a data frame tbl containing features, label and subject id for
#   each record
#
loadRawData <- function(directory, features, labels, subjects,
                        fnames = featureNames) {
  x <- read.table(paste0(directory, features), col.names = fnames)
  y <- read.table(paste0(directory, labels), col.names = labelName)
  s <- read.table(paste0(directory, subjects), col.names = subjectName)
  # make sure dimensions match
  if (nrow(x) != nrow(y) || nrow(x) != nrow(s)) {
    stop("Dimension mismatch in training data.")    
  }
  # bind it all into one dataframe and wrap it in a tbl
  tbl_df(cbind(x, y, s))
}
#
# Step 0: Load the data.
#
#
# Load the raw training data.
#
rawTrain <- loadRawData(paste0(parentDataDirectory, trainingDirectory),
                        trainX,
                        trainY,
                        trainSubject)
#
# Load the raw testing data.
#
rawTest <- loadRawData(paste0(parentDataDirectory, testingDirectory),
                       testX,
                       testY,
                       testSubject)
#
# Step 1: Combine the samples into a single tbl_df, adding a variable
# ("Source")to designate whether each observation is training or
# testing data.
#
rawTrain <- rawTrain %>% mutate(Source = "train")
rawTest <- rawTest %>% mutate(Source = "test")
raw <- rbind_list(rawTrain, rawTest)
rm(rawTrain, rawTest)  # free up memory
#
# Step 2: Retain only variables containing means and standard deviations
# of measurements (along with subject id, label and source).
#
raw <- select(raw,
              matches(labelName),       # label variable
              matches(subjectName),     # subject ID
              Source,                   # train or test?
              contains("mean"),        # measurement mean
              contains("std")          # measurement std. dev.
              )
