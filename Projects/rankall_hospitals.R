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



