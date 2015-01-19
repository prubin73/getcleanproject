---
title: "Repository Contents"
author: "Paul A. Rubin"
date: "01/19/2015"
output: html_document
---

This repository contains script files and documents for the course project in "Getting and Cleaning Data" (Coursera, section 10, January 2015). The files contained here are described below.

### Documentation
* **README.md** (this file) lists the repository contents.
* **getData.Rmd** (R Markdown) documents how and when the raw data was downloaded and unpacked. *Knit* it to obtain an HTML or PDF version and/or to run the embedded script.
* **codebook.Rmd** (R Markdown) is the "code book" documenting the variables contained in data frames produced by the ```run_analysis.R``` script.
* HTML versions of all three documentation files.

### Code
* **run_analysis.R** (R script) reads and massages the raw data, tidies it up, and extracts a tidy subset containing averages by activity and subject.  
    + There is a block of lines near the top of the script, clearly marked, that sets names and paths for the input files. Be sure to modify those to match your setup before running the script.  
    + Only measurements from the original data containing "mean()" or "std()" in their names are retained. This eliminates, for example, "angle(tBodyGyroMean,gravityMean)" and "fBodyBodyAccJerkMag-meanFreq()". This was an executive decision, given some ambiguity in the instructions.
    + The script writes a file named "groupmeans.txt" to the working directory. This file contains a table of means for each measure, grouped by subject and activity.
* **parseVariable.R** (R script) defines various helper functions, some of which are required by the ``run_analysis.R`` script (which sources ``parseVariable.R``). The functions defined in ``parseVariable.R`` are:
    + ``patternMatch(pat, text)`` applies a pattern (``pat``) to a character string (``text``) and returns the matching characters, or ``NA`` if there is no match.
    + ``parseFeature(feature)`` takes the name of one of the feature variables in the source data (e.g., "tBodyAcc-mean()-X") as its argument, and returns a list of the seven components embedded in the name (e.g., Domain = time, Component = body, Device = accelerometer, Statistic = mean, Direction = X, Jerk = FALSE, Magnitude = FALSE).
    + ``parseFeatureList(features)`` turns a list of feature names (``features``) into a lookup table, with a row for each feature name and columns for the seven components of the name.
    + ``appendNameComponent(data, names, ftable, component)`` takes as inputs a tbl_df data structure (``data``), the index (``names``) of the column of ``data`` containing the original feature names, a name lookup table (``ftable``) as constructed by ``parseFeatureList``, and the name (``component``) of one of the seven components of a feature name. It outputs a copy of ``data`` with an additional column containing the specified component of each feature name.
    + ``filterFeature(data, feature)`` takes a tbl_df data structure (``data``) and an original feature name (``feature``) and outputs those records in the table corresponding to that feature. It can be used to access both the extracted data structure and the group means table (produced by ``run_analysis.R``) using original feature names.
