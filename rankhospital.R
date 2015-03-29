############################################################
##
## Programming assignment 3: Part 1
## Coursera course data Science: RProgramming
##
## Preconditions:
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with the given rank 30-day death rate
############################################################
## Instructions:
## put r function rankhospital.R, test script rprog_scripts_submitscript3.R 
## and data csv file outcome-of-care-measures.csv in one directory.
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomeMeasures <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available")
  ##get unique identifier for states
  states <- unique(outcomeMeasures$State)
  states <- as.character(states)
  ##check if parameter state is available in data
  if (! state %in% states) stop ("invalid state")
  ##define outcome type; I hardly predefine the type ofoutcome
  ##and their column no in tabale data
  outcomeIds <- c('heart attack', 'heart failure', 'pneumonia')
  outcomeColumnId <- c(2,11, 17, 23)
  ##check if we have a valid parameter outcome
  if (! outcome %in% outcomeIds) stop ("invalid outcome")
  ##get the data for the parameter state
  ##compress the outcome data of the requested state to the outcome 
  outcomeState <- outcomeMeasures[outcomeMeasures$State == state, ]
  ##format the necessary columns 
  outcomeState[,c(2)] <- sapply(outcomeState[,c(2)],as.character)
  outcomeState[,c(11,17,23)] <- sapply(outcomeState[,c(11,17,23)],as.numeric)

  ##order the outcome for the state and death values
  ##and remove all na's
    if (outcome == "heart attack"){
    outcomeState <- outcomeState[order(outcomeState[, 11],outcomeState[, 2]), ]
    outcomeState <- outcomeState[!is.na(outcomeState[,11]),]
  }
  if (outcome == "heart failure"){
    outcomeState <- outcomeState[order(outcomeState[, 17],outcomeState[, 2]), ]
    outcomeState <- outcomeState[!is.na(outcomeState[,17]),]
  }
  if (outcome == "pneumonia"){
    outcomeState <- outcomeState[order(outcomeState[, 23],outcomeState[, 2]), ]
    outcomeState <- outcomeState[!is.na(outcomeState[,23]),]
  }

    ##define the ranking requirements
  if (num == "best"){
    displayRowIndex <- 1 
    rankedHospital <- outcomeState[displayRowIndex,2]
  } else if (num == "worst") {
    displayRowIndex <- nrow(outcomeState)
    rankedHospital <- outcomeState[displayRowIndex,2]
  } else if (num > 0 & num <= nrow(outcomeState)){
    displayRowIndex <- as.numeric(num)
    rankedHospital <- outcomeState[displayRowIndex,2]
  } else {
    ##print("not a valid rank")
    rankedHospital <- "NA"
  }
  ##display the rult
  rankedHospital
}
##test cases
##rankhospital("TX","heart failure", 1)
##rankhospital("TX","heart failure", 4)
##rankhospital("MD","heart attack", "worst")
##rankhospital("MN","heart failure", 5000)

