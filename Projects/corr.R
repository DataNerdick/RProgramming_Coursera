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
