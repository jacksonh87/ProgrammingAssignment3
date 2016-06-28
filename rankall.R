rankall <- function(outcome, num = "best") { 
  
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that the outcome is valid
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")
  
  ## The number given to subsetCol correspond to the column reflecting the outcome 
  ## in the main data set
  if(outcome == "heart attack") subsetCol <- 11
  if(outcome == "heart failure") subsetCol <- 17 
  if(outcome == "pneumonia") subsetCol <- 23
  
  ## Initialise a hospital names vector
  hospname <- c()
  
  ## For each state, find the hospital of the given rank
  statelist <- unique(as.character(outcomeData$State))
  ## Make sure states are in alphabetical order
  statelist <- sort(statelist)

  for (i in 1:length(statelist))
    {
    state <- statelist[i]
    
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
    ## Had to use a new var 'num1' otherwise it was setting length equal to the number of
    ## outcomes in the first state and using that in all wubsequesnt loops
    if (num == "best") num1 <- 1
    else if (num == "worst") num1 <- length(outcomeCol)
    else num1 <- num
    
    ## Changes the clean data for the outcome from character to numeric so it can be
    ## sorted properly
    cleanStateData[,subsetCol] <- outcomeCol

    ## Sorts the cleaned data first by the outcome of interest, then by hospital name in order
    ## to separate cases where hospitals are tied
    cleanStateData <- cleanStateData[ order( cleanStateData[ ,subsetCol], cleanStateData[ ,2]), ]

    ## Retieves the name of the hospital/s in the row with the desired ranking for the outcome
    hospname[i] <- cleanStateData[num1, 2]
  }

  ## Return a data frame with the hospital names and the (abbreviated) state name
  ## Have changed my var names to match those of the Assignment
  state <- statelist
  hospital <- hospname
  as.data.frame( cbind( state, hospital))
  
}
