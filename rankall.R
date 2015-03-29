############################################################
##
## Programming assignment 3: Part 1
## Coursera course data Science: RProgramming
##
## Preconditions:
## Read outcome data
## Check that state and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the (abbreviated) state name
############################################################
## Instructions:
## put r function rankall.R, test script rprog_scripts_submitscript3.R 
## and data csv file outcome-of-care-measures.csv in one directory.
## because we use the r script rankhospital.R this file has to be in 
## directory too.
source("rankhospital.R")
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeMeasures <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available")
  ##check for valid state is not necessary because
  ##get unique identifier for states
  states <- unique(outcomeMeasures$State)
    ##iterate over all states 
  outcomeIds <- c('heart attack', 'heart failure', 'pneumonia')
  ##check if we have a valid parameter outcome
  if (! outcome %in% outcomeIds) stop ("You don't give a valid outcome identifier")
  ##new vector for the calculated rankings
  rankings <- vector()
  ##iterate over all states and calculated the ranking
  ##for each state and outcome
  for( state in levels(states)){
    rankedHospital <- rankhospital(state,outcome,num)
    print(paste0("Rank state:", state, " Ranking: ", rankedHospital))
    
    rankingSet <- c(state, rankedHospital)
    rankings <- rbind(rankings,rankingSet)
  }
  ##convert data to a data frame as requird from submit script
  rankingDataFrame <- data.frame(rankings[,1],rankings[,2])
  ##define columns headers for the data frame
  names(rankingDataFrame) <- c("state","hospital")
  rankingDataFrame
}
##test cases
##rankall("heart attack", 4)
##head(rankall("heart attack",20),10)
##tail(rankall("pneumonia","worst"),3)
##tail(rankall("heart failure"),10)