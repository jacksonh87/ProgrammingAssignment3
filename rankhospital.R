## This function takes a state, an outcome and a numerical ranking and returns the hospital 
## in that state with that ranking for the ouutcome.
## "state" is the two-letter code for any US state
## "outcome" can be one of "heart attack", "heart failure", or "pneumonia"
## "num" is a the state-wide ranking for the hospital for that outcome and can be either an 
## integer or "best" or "worst"

rankhospital <- function(state, outcome, num) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(!(state %in% outcomeData$State)) stop("invalid state")
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")
  
  ## The number given to subsetCol correspond to the column reflecting the outcome 
  ## in the main data set
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
  
  ## Produces a vector of the column containing the outcome data of interest, with the data 
  ## converted to numeric rather than character
  outcomeCol <- as.numeric(cleanStateData[, subsetCol])
  
  ## Assigns a value for num if "best" or "worst" is used
  if (num == "best") num <- 1
  if (num == "worst") num <- length(outcomeCol)
  
  ## Checks if the ranking value passed to the function exceeds the number of hospitals
  ## we have data for. This is not asked for in the assignment sample returns  so I 
  ## commented it out.
  #if (length(outcomeCol) < num) stop("ranking exceeds number of hospitals we have data for")
  
  ## Changes the clean data for the outcome from character to numeric so it can be 
  ## sorted properly
  cleanStateData[,subsetCol] <- outcomeCol
  
  ## Sorts the cleaned data first by the outcome of interest, then by hospital name in order
  ## to separate cases where hospitals are tied 
  cleanStateData <- cleanStateData[ order( cleanStateData[ ,subsetCol], cleanStateData[ ,2]), ]

  ## Retieves the name of the hospital/s in the row with the desired ranking for the outcome
  hospName <- cleanStateData[num, 2]
  
  ## Commented out test code
  ## a <- cbind(cleanStateData$Hospital.Name, cleanStateData[,subsetCol])
  
  ## Final function return
  hospName
  
}