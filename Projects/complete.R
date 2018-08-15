complete <- function(directory, id = 1:332) {
    
    ## get list of files with the provided id
    files <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
    
    ## create empty data frame to read stuff to it
    ## and calculate complete cases
    final <- data.frame()
    for(i in id) {
        df <- data.frame()
        df <- read.csv(files[i])
        completeCases <- sum(complete.cases(df))
        
        ## add complete cases to the final data frame
        final <- rbind(final, c(i, completeCases))
    }
    ## name cols in the final data frame
    colnames(final) <- c("ID", "nobs")
    print(final)
}