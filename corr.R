############################################################
##
## Programming assignment 1: Part 3
## Coursera course data Science: RProgramming
##
## Preconditions:
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0
## Return a numeric vector of correlations
############################################################
## Instructions
## collect corr and test script in one directory.
## The specdata should be available in a subdirectory

#Load the r script complete.R
source("complete.R")
corr <- function(directory, threshold = 0) {
  #initialize the vector for the correlation vales
  correlationVector <- NA
  #get all completely edited observations
  allCorrectObservation <- complete(directory)
  #check the observations against the threshold
  nobsPassedThreshold <- allCorrectObservation[allCorrectObservation$nobs > threshold,]
  #iterate over all threshold valid datasets
  for (id in nobsPassedThreshold$id) {
    #create filename for each i 
    fileName <- paste0(directory,"/",sprintf("%03d", id),".csv")
    #read the file data for each fileName
    fileData <- read.csv(file=fileName,header=T, sep=",")
    #caluclate the correlation for each file and put the result to the vector correlationVector
    correlationVector <- c(correlationVector,cor(fileData$nitrate, fileData$sulfate, use = "complete.obs"))
  }
  #delete the initial NA value
  return (na.omit(correlationVector))
}

#Test cases
#data <- corr("specdata", 150)
#head(data)


