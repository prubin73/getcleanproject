---
title: "Repository Contents"
author: "Paul A. Rubin"
date: "01/14/2015"
output: html_document
---

This repository contains script files and documents for the course project in "Getting and Cleaning Data" (Coursera, section 10, January 2015). The files contained here are as follows:

1. **getData.Rmd** (R Markdown) documents how and when the raw data was downloaded and unpacked. *Knit* it to obtain an HTML or PDF version and/or to run the embedded script.
2. **run_analysis.R** (R script) reads and massages the raw data, tidies it up, and extracts a tidy subset containing averages by activity and subject.  
    + There is a block of lines near the top of the script, clearly marked, that sets names and paths for the input files. Be sure to modify those to match your setup before running the script.  
    + Only measurements from the original data containing "mean()" or "std()" in their names are retained. This eliminates, for example, "angle(tBodyGyroMean,gravityMean)" and "fBodyBodyAccJerkMag-meanFreq()". This was an executive decision, given some ambiguity in the instructions.  
3. **codebook.Rmd** (R Markdown) is the "code book" documenting the variables contained in data frames produced by the ```run_analysis.R``` script.