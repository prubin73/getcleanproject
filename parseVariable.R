#
# This script provides functions to assist in parsing the (rather clunky)
# variable (feature) names from the UCI source data set.
#
library(dplyr)
#
# Function patternMatch applies a regex pattern to a single string and
# return the matching portion (or NA if no match occurs).
#
# Arguments:
#   pat  = the pattern to apply
#   text = the string to test
#
# Value:
#   A character vector (string) containing the matching text, if any.
#
patternMatch <- function(pat, text) {
  m <- regexpr(pat, text)  %>%
       regmatches(text, .)
  m <- ifelse(length(m) == 0, NA, m)
  m
}
#
# Function parseFeature converts a feature name to a vector of values
# for the factor variables created in the run_analysis.R script.
#
# NOTE: This function is designed to work only with features containing
# 'mean()' or 'std()' in their names.
#
# Argument:
#   feature = the name (character vector) of an original feature variable
#
# Value:
#   a vector of values for the factors (Component, Device, Domain
#   Direction, Jerk, Magnitude, Statistic) used in the tidied data set.
#
parseFeature <- function(feature) {
  factors <- vector(length = 7)
  names(factors) <- c("Component", "Device", "Domain", "Direction",
                      "Jerk", "Magnitude", "Statistic")
  factors["Component"] <- patternMatch("(Body)+|Gravity", feature)
  factors["Device"] <- ifelse(grepl("Acc", feature),
                              "accelerometer",
                              "gyroscope")
  factors["Domain"] <- ifelse(grepl("^t", feature), "time", "frequency")
  factors["Direction"] <- patternMatch("[XYZ]$", feature)
  factors["Jerk"] <- grepl("Jerk", feature)
  factors["Magnitude"] <- grepl("Mag", feature)
  factors["Statistic"] <- ifelse(grepl(".*mean.*", feature),
                                 "mean",
                                 "standard_deviation")
  factors
}
#
# Function parseFeatureList parses a list of feature names and creates
# a lookup table for the various name components.
#
# Argument:
#   features = a list or vector of original feature names
#
# Value:
#   a table with one entry for each original feature name
#
# Example:
#   x <- c("tBodyAcc-mean()-X", "fBodyBodyGyroJerkMag-std()")
#   y <- parseFeatureList(x)
#   y[["tBodyAcc-mean()-X"]]                        # seven name components
#   y[["fBodyBodyGyroJerkMag-std()"]]["Component"]  # "BodyBody"
#   y[["tBodyAcc-mean()-X"]][["Device"]]            # "accelerometer"
#
parseFeatureList <-
  function(features) {
    f <- unique(features)          # weed out duplicate names
    result <-
      f                      %>%
      sapply(parseFeature)   %>%   # parse each feature name
      t                      %>%   # transpose the result
      as.data.frame                # make it a data frame
    rownames(result) <- f          # use the original names as row names
    tbl_df(result)                 # return it as a tbl_df
  }
#
# Function appendNameComponent appends a column to a table containing a
# specified component of the feature names.
#
# Arguments:
#    data      = tbl_df or data.frame to be extended
#    names     = index of the column containing feature names
#    ftable    = feature table (output of parseFeatureList function)
#    component = the name of the component to be added
#
# Value:
#    a copy of 'data' with a new column containing the designated component
#    of the feature names (as a factor)
#
# Example:
#    x <- ...      # data.frame or tbl_df with feature names in column "FN"
#    t <- parseFeatureList(x[, "FN"])
#    y <- appendNameComponent(x, "FN", t, "Domain")
#                  # x with "Domain" column added
#
appendNameComponent <- function(data, names, ftable, component) {
  data <-
    data[, names]                   %>%  # access the feature names
    as.data.frame                   %>%  # strip off the tbl_df wrapper
    getElement(., names)            %>%  # isolate the names column
    ftable[., component]            %>%  # look up those entries in
                                         # the feature table
    cbind(data, .)                       # append the column
}
#
# Function filterFeature extracts from a tbl_df those rows corresponding
# to a particular original feature. The table must have columns for
# the factors composing a feature name ("Component", "Device", "Domain",
# "Direction", "Jerk", "Magnitude" and "Statistic"), consistent with the
# output of parseFeatureList.
#
# Arguments:
#   data    = tbl_df containing the data
#   feature = original feature name (e.g., "tBodyAcc-mean()-X")
#
# Value:
#   the subset of 'data' consisting of records for the target feature
#
filterFeature <- function(data, feature) {
  f <- parseFeature(feature)       # parse the feature name
  filter(data, Component == f["Component"] &
               Device    == f["Device"]    &
               Domain    == f["Domain"]    &
               Direction == f["Direction"] &
               Jerk      == f["Jerk"]      &
               Magnitude == f["Magnitude"] &
               Statistic == f["Statistic"])
}
