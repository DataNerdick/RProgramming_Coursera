best <- function(state, outcome) {
  
  ## Make sure working directory is set correctly
  # setwd("/Users/Annechka/Desktop/UWBothell/Fall\ 2018/498_R_programming/ProgAssignment3_data")
  
  ## Read outcome data
  theData <- read.csv("outcome-of-care-measures.csv",  na.strings = c("Not Available"), stringsAsFactors = FALSE)
  
  # create storage for legal outcomes
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  #create storage for legal states
  states <- unique(thedata[, 7])
  
  ## Check that state and outcome are valid
  if(outcome %in% outcomes == FALSE) {
    stop("illegal outcome")
  }
  
  if(state %in% states == FALSE) {
    stop("illegal state")
  }
  
  ## Return the hospital name in that state with lowest 30-day death rate
  
  ## get column for heart attack
  if(outcome == "heart attack") {
    resulting_column <- 11
  }
  
  ## get column for heart failure
  if(outcome == "heart failure") {
    resulting_column <-  17
  }
  
  ## get column for pneumonia
  if(outcome == "pneumonia") {
    resulting_column <-  23
  }
  
  ## get the new Data Frame with selected state and needed outcome
  stateData <- subset(theData[, c(2,7,resulting_column)], State == state)
  
  colnames(stateData) <- c("Hospital", "State", "Outcome")
  
  ## sort the hotels by lowest mortality rate depending on the outcome and hospital
  result <- stateData[order(stateData$Outcome,stateData$Hospital, na.last = TRUE),]
  
  # Get the first value from the Hospital column
  final <- result[1, 1]
  final
}  