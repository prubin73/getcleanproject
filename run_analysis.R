#
# Script to fulfill the instructions of the term project in "Getting
# and Cleaning Data" (Coursera, January 2015).
#
# Script requirements:
#
#   1. Libraries "dplyr" and "tidyr" must be installed.
#   2. The "parseVariable.R" script must be in the working directory.
#
# Project requirements:
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
# Source the parseVariables.R script.
#
source("parseVariable.R")
#
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
                as.vector
                  # turn it into a vector
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
message("... loading training data ...")
rawTrain <- loadRawData(trainX, trainY, trainSubject)
#
# Load the raw testing data.
#
message("... loading testing data ...")
rawTest <- loadRawData(testX, testY, testSubject)
#
# Step 1: Combine the samples into a single tbl_df. For project purposes,
# we will ignore whether a given subject was in the training or testing
# group.
#
message("... combining ...")
extractedData <- rbind_list(rawTrain, rawTest)
rm(rawTrain, rawTest)  # free up memory
#
# Step 2: Retain only variables containing means and standard deviations
# of measurements (along with subject id, label and source).
# NOTE: Only variables containing "mean()" or "std()" in their names
# (case-sensitive) are kept. This eliminates, for example, 
# "angle(tBodyGyroMean,gravityMean)" and "fBodyBodyAccJerkMag-meanFreq()".
#
message("... selecting means and standard deviations ...")
extractedData <-
  select(extractedData,
         Subject,                                 # subject ID
         Activity,                                # activity number
         contains("mean.", ignore.case = FALSE),  # measurement mean
         contains("std.", ignore.case = FALSE)    # measurement std. dev.
         )
#
# Step 3: Make the label variable a factor named "Activity", using the
# activity names from the data set. Also make the subject variable
# a factor.
#
message("... adding subject and activity labels ...")
extractedData <- 
  mutate(extractedData,
         Activity = factor(Activity,
                           labels = read.table(activities)[, 2]),
         Subject  = factor(Subject)
         )
#
# Step 4: Add descriptive variable names. This is combined with tidying
# the data.
#
# Partially tidy the data by converting all the measurement data to
# two variables (Measure and Value).
#
message("... gathering features into one variable ...")
extractedData <-
  gather(extractedData, Measure, Value, -c(Subject, Activity))
#
#
# Create a lookup table that converts original feature names into
# components (Device, Domain, ...)
#
message("... creating feature name conversion table ...")
featureTable <- parseFeatureList(extractedData$Measure)
#
# Create new variables for each component of the feature names.
#
message("... parsing feature names into multiple factors ...")
extractedData <-
  appendNameComponent(extractedData, "Measure", featureTable, names(featureTable))
#
# Reorder the variables in a somewhat arbitrary but more logical (?)
# manner, while dropping the now redundant "Measure" variable.
#
message("... rearranging variable order ...")
extractedData <-
  select(extractedData,
         Subject, Activity, Component, Device, Domain,
         Direction, Jerk, Magnitude, Statistic, Value)
#
# Step 5: Summarize the data, producing a mean value of each measure
# for each combination of subject and activity. Note: the grouping is
# removed at the end of the command, because it interferes with the
# select() function if the user wants to extract specific entries
# from the table.
#
message("... computing means by subject and activity ...")
groupMeans <-
  extractedData                                            %>%
  group_by(Subject, Activity, Component, Device, Domain,
           Direction, Jerk, Magnitude, Statistic)          %>%
  summarise(Mean = mean(Value))                            %>%
  ungroup
#
# Write the summary table to a text file.
#
message("... writing the group means to a file ...")
write.table(groupMeans, "groupmeans.txt", row.name = FALSE)
