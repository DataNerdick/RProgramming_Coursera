rankhospital <- function(state, outcome, num = "best") {
  ## read outcome data
  theData <- read.csv("outcome-of-care-measures.csv", na.strings = c("Not Available"), stringsAsFactors = FALSE)
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  states <- unique(theData[, 7])
  
  ## check that state and outcome are valid
  if(outcome %in% outcomes == FALSE) {
    stop("invalid input")
  }
  
  if(state %in% states == FALSE) {
    stop("invalid input")
  }
  
  ## return hospital name in that state with the given rank
  ## 30-day death rate
  
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