#
# This script provides functions to assist in parsing the (rather clunky)
# variable (feature) names from the UCI source data set.
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
