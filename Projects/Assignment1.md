# RProgramming Coursera

Introduction
For this first programming assignment you will write three functions that are meant to interact 
with dataset that accompanies this assignment. Although this is a programming assignment, you will be assessed using a separate quiz.

Data
The zip file containing the data can be downloaded here:
*	specdata.zip [2.4MB]
The zip file contains 332 comma-separated-value (CSV) files containing pollution monitoring data for fine particulate matter (PM) air pollution at 332 locations in the United States. Each file contains data from a single monitor and the ID number for each monitor is contained in the file name. For example, data for monitor 200 is contained in the file "200.csv". Each file contains three variables:
*	Date: the date of the observation in YYYY-MM-DD format (year-month-day)
*	sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)
*	nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)
For this programming assignment you will need to unzip this file and create the directory 'specdata'. Once you have unzipped the zip file, do not make any modifications to the files in the 'specdata' directory. In each file you'll notice that there are many days where either sulfate or nitrate (or both) are missing (coded as NA). This is common with air pollution monitoring data in the United States.


Part 1
----------
Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a 
specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory 
specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, 
ignoring any missing values coded as NA. 

```R
pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    # first set the working directory to "CourseraR"
    
    # get list of files
    files <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
    
    # store pollutant values from each file in a vector
    values <- numeric()
    
    # go through every file and get the pollutant values
    for(i in id) {
        data <- read.csv(files[i])
        values <- c(values, data[[pollutant]])
    }
    
    # calculate the mean removing NA
    mean(values, na.rm = TRUE)
}
```

Part 2
----------
Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. 
The function should return a data frame where the first column is the name of the file and the second column is the number 
of complete cases.

```R
complete <- function(directory, id = 1:332) {
    
    # get list of files with the provided id
    files <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
    
    # create empty data frame to read stuff to it
    # and calculate complete cases
    final <- data.frame()
    for(i in id) {
        df <- data.frame()
        df <- read.csv(files[i])
        completeCases <- sum(complete.cases(df))
        
        # add complete cases to the final data frame
        final <- rbind(final, c(i, completeCases))
    }
    # name cols in the final data frame
    colnames(final) <- c("ID", "nobs")
    print(final)
}
```

Part 3
----------
Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation 
between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is 
greater than the threshold. The function should return a vector of correlations for the monitors that meet the 
threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric 
vector of length 0.

```R
corr <- function(directory, threshold = 0) {
    
    ## store list of files in a variable
    list <- list.files(path = directory, pattern = ".csv", full.names = TRUE)

    ## create an empty data frame
    df <- NULL
    
    ## create a list to store files that pass threshold
    newList <- character()
    
    ## create vector to store correlations
    correlations <- numeric()
    
    for(i in 1:length(list)) {
        if(sum(complete.cases(read.csv(list[i]))) > threshold) {
            newList <- c(newList, list[i])
        }
    }
    ## if newList is empty, then no monitors meet the treshold requirement
    ## return empty correlations vector
    if(length(newList) == 0) {
        return (correlations)
    }
    
    ## go through the new list and calculate correlations
    for(i in 1:length(newList)) {
        df <- read.csv(newList[i])
        correlation <- cor(df[ ,"sulfate"], df[ ,"nitrate"], use = "complete.obs")
        correlations <- c(correlations, correlation)
    }
    correlations 
}
```
