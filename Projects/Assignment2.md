# RProgramming Assignment2 Coursera

Introduction
-------------

Download the file ProgAssignment3-data.zip file containing the data for Programming Assignment 3 from
the Coursera web site. Unzip the file in a directory that will serve as your working directory. When you
start up R make sure to change your working directory to the directory where you unzipped the data.

The data for this assignment come from the Hospital Compare web site (http://hospitalcompare.hhs.gov)
run by the U.S. Department of Health and Human Services. The purpose of the web site is to provide data and
information about the quality of care at over 4,000 Medicare-certified hospitals in the U.S. This dataset essentially
covers all major U.S. hospitals. This dataset is used for a variety of purposes, including determining
whether hospitals should be fined for not providing high quality care to patients (see http://goo.gl/jAXFX
for some background on this particular topic).

The Hospital Compare web site contains a lot of data and we will only look at a small subset for this
assignment. The zip file for this assignment contains three files
* outcome-of-care-measures.csv: Contains information about 30-day mortality and readmission rates
for heart attacks, heart failure, and pneumonia for over 4,000 hospitals.
* hospital-data.csv: Contains information about each hospital.
* Hospital_Revised_Flatfiles.pdf: Descriptions of the variables in each file (i.e the code book).

A description of the variables in each of the files is in the included PDF file named Hospital_Revised_Flatfiles.pdf.
This document contains information about many other files that are not included with this programming
assignment. You will want to focus on the variables for Number 19 (“Outcome of Care Measures.csv”) and
Number 11 (“Hospital Data.csv”). You may find it useful to print out this document (at least the pages for
Tables 19 and 11) to have next to you while you work on this assignment. In particular, the numbers of
the variables for each table indicate column indices in each table (i.e. “Hospital Name” is column 2 in the
outcome-of-care-measures.csv file).

Part 1 Best Hospital in the State
---------------------------------

Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
outcome should be excluded from the set of hospitals when deciding the rankings.

**Handling ties**. If there is a tie for the best hospital for a given outcome, then the hospital names should
be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
and “f” are tied for best, then hospital “b” should be returned).

```R
best <- function(state, outcome) {
 
  # Make sure working directory is set correctly
  # Read outcome data
  theData <- read.csv("outcome-of-care-measures.csv",  na.strings = c("Not Available"), stringsAsFactors = FALSE)
  
  # create storage for legal outcomes
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  #create storage for legal states
  states <- unique(thedata[, 7])
  
  # Check that state and outcome are valid
  if(outcome %in% outcomes == FALSE) {
    stop("illegal outcome")
  }
  
  if(state %in% states == FALSE) {
    stop("illegal state")
  }
  
  # Return the hospital name in that state with lowest 30-day death rate
  
  # get column for heart attack
  if(outcome == "heart attack") {
    resulting_column <- 11
  }
  
  # get column for heart failure
  if(outcome == "heart failure") {
    resulting_column <-  17
  }
  
  # get column for pneumonia
  if(outcome == "pneumonia") {
    resulting_column <-  23
  }
  
  # get the new Data Frame with selected state and needed outcome
  stateData <- subset(theData[, c(2,7,resulting_column)], State == state)
  
  colnames(stateData) <- c("Hospital", "State", "Outcome")
  
  # sort the hotels by lowest mortality rate depending on the outcome and hospital
  result <- stateData[order(stateData$Outcome,stateData$Hospital, na.last = TRUE),]
  
  # Get the first value from the Hospital column
  final <- result[1, 1]
  final
}  
```
Part 2 Hospitals Rank by Outcome in a State
-------------------------------------------

Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
of the hospital that has the ranking specified by the num argument. For example, the call

```R
rankhospital("MD", "heart failure", 5) 
```
would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
for heart failure. The num argument can take values “best”, “worst”, or an integer indicating the ranking
(smaller numbers are better). If the number given by num is larger than the number of hospitals in that
state, then the function should return NA. Hospitals that do not have data on a particular outcome should
be excluded from the set of hospitals when deciding the rankings.

**Handling ties**. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
of death. In those cases ties should be broken by using the hospital name. For example, in Texas (“TX”),
the hospitals with lowest 30-day mortality rate for heart failure are shown here.

```R
> head(texas)
Hospital.Name Rate Rank
3935 FORT DUNCAN MEDICAL CENTER 8.1 1
4085 TOMBALL REGIONAL MEDICAL CENTER 8.5 2
4103 CYPRESS FAIRBANKS MEDICAL CENTER 8.7 3
3954 DETAR HOSPITAL NAVARRO 8.7 4
4010 METHODIST HOSPITAL,THE 8.8 5
3962 MISSION REGIONAL MEDICAL CENTER 8.8 6
```

Note that Cypress Fairbanks Medical Center and Detar Hospital Navarro both have the same 30-day rate
(8.7). However, because Cypress comes before Detar alphabetically, Cypress is ranked number 3 in this
scheme and Detar is ranked number 4. One can use the order function to sort multiple vectors in this
manner (i.e. where one vector is used to break ties in another vector).

```R
rankhospital <- function(state, outcome, num = "best") {
  # read outcome data
  theData <- read.csv("outcome-of-care-measures.csv", na.strings = c("Not Available"), stringsAsFactors = FALSE)
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  states <- unique(theData[, 7])
  
  # check that state and outcome are valid
  if(outcome %in% outcomes == FALSE) {
    stop("invalid input")
  }
  
  if(state %in% states == FALSE) {
    stop("invalid input")
  }
  
  # return hospital name in that state with the given rank
  # 30-day death rate
  
  # get column for heart attack
  if(outcome == "heart attack") {
    resulting_column <- 11
  }
  
  # get column for heart failure
  if(outcome == "heart failure") {
    resulting_column <-  17
  }
  
  # get column for pneumonia
  if(outcome == "pneumonia") {
    resulting_column <-  23
  }
  
  # get the new Data Frame with selected state and needed outcome
  stateData <- na.omit(subset(theData[, c(2,7,resulting_column)], State == state))
  
  if(num == "best") {
    num <-1
  }
  
  if(num == "worst") {
    num <- nrow(stateData)
  }
  
  # Name the columns
  colnames(stateData) <- c("Hospital", "State", "Outcome")
  
  # Order the data and get the result
  result <- stateData[order(stateData$Outcome,stateData$Hospital),]
  
  final <- result[num, 1]
  final
}
```

Part 3 Ranking Hospitals in all States
--------------------------------------
Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking
(num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
containing the hospital in each state that has the ranking specified in num. For example the function call
```R
rankall("heart attack", "best") 
```
would return a data frame containing the names of the hospitals that
are the best in their respective states for 30-day heart attack death rates. The function should return a value
for every state (some may be NA). The first column in the data frame is named hospital, which contains
the hospital name, and the second column is named state, which contains the 2-character abbreviation for
the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
hospitals when deciding the rankings.

**Handling ties**. The rankall function should handle ties in the 30-day mortality rates in the same way
that the rankhospital function handles ties.

```R
rankall <- function(outcome, num = "best") {
  ## Read data
  theData <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## check that state and outcome are valid
  if(outcome %in% outcomes == FALSE) {
    stop("invalid input")
  }
  
  # get column for heart attack
  if(outcome == "heart attack") {
    resulting_column <- 11
  }
  
  # get column for heart failure
  if(outcome == "heart failure") {
    resulting_column <-  17
  }
  
  # get column for pneumonia
  if(outcome == "pneumonia") {
    resulting_column <-  23
  }
  
  ## Subset to three columns and remoe NA values
  cleanedData <- na.omit(subset(theData[, c(2,7,resulting_column)]))
  
  # name the columns
  colnames(cleanedData) <- c("Hospital", "State", "Outcome")
  
  ## Order data by state, outcome and hospital
  orderedData <- cleanedData[order(cleanedData$State, cleanedData$Outcome,cleanedData$Hospital),]
  
  ## For each state, find the hospital of the given rank
  #split on state
  splitData <- split(orderedData, orderedData$State) #return a list of data frames ordered by state
  
  #fuction to put hotel names with requested num for each state in a single list
  hospitalNames <- function(data_frame) {
    if(num == "best") {
      num <- 1
    }
    if(num == "worst") {
      num <- nrow(data_frame)
    }
    hospitals_list <- data_frame[num, 1]
  }

  result_of_apply <- lapply(splitData, hospitalNames) #get list of hospital names
  
  values_vector <- unlist(result_of_apply) #unlist lapply to get the character vector instead of a list
  
  states_list <- names(result_of_apply)
  
  #assemble the resulting data frame
  finaldf <- data.frame(hospital = values_vector, state = states_list, row.names = states_list)
  finaldf
}
```







