## This function takes a state and an outcome and returns the hospital in that state with 
## the lowest 30-day death ratre for that outcome.
## "state" is the two-letter code for any US state
## "outcome" can be one of "heart attack", "heart failure", or "pneumonia"

best <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(!(state %in% outcomeData$State)) stop("invalid state")
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death rate. The numbers given
  ## to subsetCol correspond to the column reflecting the outcome in the main data set
  if(outcome == "heart attack") subsetCol <- 11
  if(outcome == "heart failure") subsetCol <- 17 
  if(outcome == "pneumonia") subsetCol <- 23
  
  ## Chops the main data set into the data set for the given state
  stateData <- outcomeData[outcomeData$State == state,]
  
  ## Turns the stateData column of interest into either numbers, or NA if there is
  ## text in the data. Warning suppressed because R warns it is turning text into NAs.
  suppressWarnings(rawStateOutcome <- as.numeric(stateData[, subsetCol]))
  
  ## Subsets the state data set into a data set excluding rows where this is no data for
  ## the outcome of interest
  cleanStateData <- stateData[!is.na(rawStateOutcome), ]

  ## Produces a vector of the column containing the oucome data of interest
  outcomeCol <- as.numeric(cleanStateData[, subsetCol])
  
  ## Retunrs the location of the row containing the minimum value of the outcome
  minRow <- which(outcomeCol == min(outcomeCol))
  
  ## Retieves the name of the hospital/s in the row with the minimum value
  ## of the outcome
  hospName <- cleanStateData[minRow, 2]
 
  ## If more than one hospital has the minimum value for the outcome, this code will
  ## return the name first alphabetically
  sortedHospName <- sort(hospName)
  sortedHospName[1]
}