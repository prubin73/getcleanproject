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
########################################################################
#                                                                      #
# UPDATE THIS SECTION BEFORE RUNNING THE SCRIPT. This section contains #
# file paths and names that you as a user might need to change.        #
# Be sure to end directories with slashes.                             #
#                                                                      #
########################################################################
parentDataDirectory <- "../UCI HAR Dataset/"
trainingDirectory <- paste0(parentDataDirectory, "train/")
testingDirectory <- paste0(parentDataDirectory, "test/")
testX <- paste0(testingDirectory, "X_test.txt")
                                          # testing set features
testY <- paste0(testingDirectory, "y_test.txt")
                                          # testing set labels
testSubject <- paste0(testingDirectory, "subject_test.txt")
                                          # testing set subject IDs
trainX <- paste0(trainingDirectory, "X_train.txt")
                                          # training set features
trainY <- paste0(trainingDirectory, "y_train.txt")
                                          # training set labels
trainSubject <- paste0(trainingDirectory, "subject_train.txt")
                                          # training set subject IDs
features <- paste0(parentDataDirectory, "features.txt")
                                          # feature names
activities <- paste0(parentDataDirectory, "activity_labels.txt")
                                          # activity names
############## DEVELOP/DEBUG ONLY - REMOVE WHEN DONE ###############
trainingDirectory <- paste0(parentDataDirectory, "debug/")
testingDirectory <- paste0(parentDataDirectory, "debug/")
testX <- paste0(testingDirectory, "X_test.txt")
testY <- paste0(testingDirectory, "y_test.txt")
testSubject <- paste0(testingDirectory, "subject_test.txt")
trainX <- paste0(trainingDirectory, "X_train.txt")
trainY <- paste0(trainingDirectory, "y_train.txt")
trainSubject <- paste0(trainingDirectory, "subject_train.txt")
####################################################################
#
# Read in the feature names (from the parent directory) and do some
# minor cleaning of them.
#
featureNames <- features                              %>%
                  # source file
                read.table                            %>%
                  # read the feature names
                getElement(2)                         %>%
                  # omit the first (index) column
                as.vector                             %>%
                  # turn it into a vector
                gsub("()", "", .)                     %>%
                  # eliminate empty parenthesis pairs
                gsub("[(),]", "_", .)                 %>%
                  # convert any remaining parentheses or commas into
                  # underscores
                gsub("__", "_", .)                    %>%
                  # change consecutive underscores to single underscores
                gsub("_$", "", .)
                  # remove terminal underscores
#
# Define a function to load raw data and glue it into one database.
#
# Arguments:
#   features = name/path of the feature data file
#   labels   = name/path of the label date file
#   subjects = name/path of the subject data file
#   fnames   = names to assign to the features
#              (default: global variable "featureNames")
#
# Value:
#   a data frame tbl containing features, label and subject id for
#   each record
#
loadRawData <- function(features, labels, subjects,
                        fnames = featureNames) {
  x <- read.table(features, col.names = fnames)
  y <- read.table(labels, col.names = "Activity")
  s <- read.table(subjects, col.names = "Subject")
  # make sure dimensions match
  if (nrow(x) != nrow(y) || nrow(x) != nrow(s)) {
    stop("Dimension mismatch in training data.")    
  }
  # bind it all into one dataframe and wrap it in a tbl
  tbl_df(cbind(s, y, x))
}
#
# Step 0: Load the data.
#
#
# Load the raw training data.
#
rawTrain <- loadRawData(trainX, trainY, trainSubject)
#
# Load the raw testing data.
#
rawTest <- loadRawData(testX, testY, testSubject)
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
              Subject,                  # subject ID
              Activity,                 # activity number
              Source,                   # train or test?
              contains("mean"),         # measurement mean
              contains("std")           # measurement std. dev.
              )
#
# Step 3: Make the label variable a factor, using the activity names
# from the data set. Also make the subject and source variables factors.
#
raw <- mutate(raw,
              Activity = factor(Activity,
                                     labels = read.table(activities)[, 2]),
              Source = factor(Source),
              Subject = factor(Subject))
