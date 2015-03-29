############################################################
##
## Programming assignment 3: Part 1
## Coursera course data Science: RProgramming
##
## Preconditions:
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death rate
############################################################
## Instructions:
## put r function best.R, test script rprog_scripts_submitscript3.R 
## and data csv file outcome-of-care-measures.csv in one directory.
best <- function(state, outcome) {
  ## Read outcome data
  outcomeMeasures <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available")
  ##get unique identifier for states
  states <- factor(outcomeMeasures$State)
  ##check if parameter state is available in data
  if (! state %in% states) stop ("invalid state")
  ##define outcome type; I hardly predefine the type ofoutcome
  ##and their column no in tabale data
  outcomeIds <- c('heart attack', 'heart failure', 'pneumonia')
  outcomeColumnId <- c(2,11, 17, 23)
  ##check if we have a valid parameter outcome
  if (! outcome %in% outcomeIds) stop ("invalid outcome")
  ##get the data for the parameter state
  outcomeState <- outcomeMeasures[outcomeMeasures$State == state, ]
  ##convert necessary columns to numeric 
  outcomeState[,c(11,17,23)] <- sapply(outcomeState[,c(11,17,23)],as.numeric)
  outcomeState[,c(2)] <- sapply(outcomeState[,c(2)],as.character)
  ##regarding to outcome type we choose the specific column
  ##and calculate the minimum of that column
  if (outcome == "heart attack") {
    best <- outcomeState[which.min(outcomeState[, 11]),"Hospital.Name"]
  }
  else if (outcome == "heart failure") {
    best <- outcomeState[which.min(outcomeState[, 17]),"Hospital.Name"]
  }
  else {
    best <- outcomeState[which.min(outcomeState[, 23]),"Hospital.Name"]
  }
  ##return the result
  best
}

# test cases
##best("TX", "heart attack")
##best("TX", "heart failure")
##best("MD", "heart attack")
##best("MD", "pneumonia")
##best("BB", "heart attack")
##best("NY", "hert attack")

