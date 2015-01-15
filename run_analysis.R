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
# Define a function to do pattern matching on a measure name and
# return a factor variable with two levels, based on match/no match.
#
# Arguments:
#   names   = character vector of names to check
#   pattern = regular expression to match
#   match   = desired factor level (character) if a match occurs
#   nomatch = desired factor level (character) if a match does not occur
#
# Value:
#   a factor indicating matches/non-matches
#
factorize <- function(names, pattern, match, nomatch) {
  names %>%
  grepl(pattern, .) %>%
  ifelse(., match, nomatch) %>%
  factor
}
#
# Define a function to do pattern matching on a measure name and
# return a factor variable with levels equalling a matched substring.
#
# Arguments:
#   names   = character vector of names to check
#   pattern = regular expression to match (containing exactly one subgroup)
#
# Value:
#   a factor based on the matched subgroups (NA for no match)
#
factorize2 <- function(names, pattern) {
  # replace names with the matching component of the pattern
  temp <- names %>%
          sub(pattern, "\\1", .)
  # if any name does not match, make it NA
  is.na(temp) <- !grepl(pattern, temp)
  # convert the result to a factor
  factor(temp)
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
rawTrain <- rawTrain %>% mutate(Sample = "train")
rawTest <- rawTest %>% mutate(Sample = "test")
raw <- rbind_list(rawTrain, rawTest)
rm(rawTrain, rawTest)  # free up memory
#
# Step 2: Retain only variables containing means and standard deviations
# of measurements (along with subject id, label and source).
# NOTE: Only variables containing "mean()" or "std()" in their names
# (case-sensitive) are kept. This eliminates, for example, 
# "angle(tBodyGyroMean,gravityMean)" and "fBodyBodyAccJerkMag-meanFreq()".
#
raw <- select(raw,
              Subject,                  # subject ID
              Activity,                 # activity number
              Sample,                   # train or test?
              contains("mean.", ignore.case = FALSE),
                                        # measurement mean
              contains("std.", ignore.case = FALSE)
                                        # measurement std. dev.
              )
#
# Step 3: Make the label variable a factor, using the activity names
# from the data set. Also make the subject and sample variables factors.
#
raw <- mutate(raw,
              Activity = factor(Activity,
                                labels = read.table(activities)[, 2]),
              Sample = factor(Sample),
              Subject  = factor(Subject))
#
# Step 4: Add descriptive variable names. This is combined with tidying
# the data.
#
# Partially tidy the data by converting all the measurement data to
# two variables (Measure and Value).
#
raw <- gather(raw, Measure, Value, -c(Subject, Activity, Sample))
#
# Create a separate variable for domain (time or frequency)
#
raw <- raw$Measure                    %>%
       factorize(., "^t", "time", "frequency") %>%
       mutate(raw, Domain = .)
#
# Create a separate variable to capture the relevant direction (X, Y, Z)
# for each measure. Use NA if no direction is explicit in the name.
#
raw <- raw$Measure                     %>%
       factorize2(., ".*([XYZ])$")     %>%
       mutate(raw, Direction = .)
#
# Create a separate variable (factor) indicating whether the entry in
# the value field is a mean or a standard deviation.
#
raw <- raw$Measure                                            %>%
       factorize(., ".*mean.*", "mean", "standard_deviation") %>%
       mutate(raw, Statistic = .)
#
# Create a variable (factor) indicating the device (accelerometer or
# gyroscope) producing the signal.
#
raw <- raw$Measure                                            %>%
       factorize(., ".*Gyro.*", "gyroscope", "accelerometer") %>%
       mutate(raw, Device = .)
#
# Create a variable (factor) indicating the source of acceleration signals
# (body or gravity).
#
raw <- raw$Measure                                  %>%
       factorize(., ".*Body.*", "body", "gravity")  %>%
       mutate(raw, Source = .)
#
# Create factors indicating whether a signal is a 'Jerk' or 'Mag'
# (magnitude) measurement.
#
raw <- raw$Measure                                  %>%
       factorize(., ".*Jerk.*", TRUE, FALSE)        %>%
       mutate(raw, Jerk = .)
raw <- raw$Measure                                  %>%
       factorize(., ".*Mag.*", TRUE, FALSE)        %>%
       mutate(raw, Magnitude = .)
