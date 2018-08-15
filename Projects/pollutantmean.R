pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    ## first set the working directory to "CourseraR"
    
    ## get list of files
    files <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
    
    ## store pollutant values from each file in a vector
    values <- numeric()
    
    ## go through every file and get the pollutant values
    for(i in id) {
        data <- read.csv(files[i])
        values <- c(values, data[[pollutant]])
    }
    
    ## calculate the mean removing NA
    mean(values, na.rm = TRUE)
}


