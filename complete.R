############################################################
##
##Programming assignment 1: Part 2
##Coursera course data Science: RProgramming
##
## Preconditions:
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
## 'id' is an integer vector indicating the monitor ID numbers
## to be used
## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases
############################################################
## Instructions:
## collect pollutantmean and test script in one directory.
## The specdata should be available in a subdirectory
complete <- function(directory, id = 1:332) {
  #initialize used variables
  idsC <- NA
  nobsC <- NA
  nobs <- NA
  #calculate for each parametrized id the completely edited
  #observations in nobsC and bind id and nobs in vectors named
  #idsC and nobsC
  for (monitorId in id){
    #save each id in the vector idsC
    idsC <- c(idsC,monitorId)
    #create filename for each id 
    fileName <- paste0(directory,"/",sprintf("%03d", monitorId),".csv")
    #read the file data for each fileName
    fileData <- read.csv(file=fileName,header=T, sep=",")
    #count the number of completely edited observations
    nobsC <- nrow(na.omit(fileData))
    #collect the observations in the vector nobs
    nobs <- c(nobs,nobsC)
  }
  #delete initialized NA value from the vectors idsC and nobsC
  idsC <- na.omit(idsC)
  nobs <- na.omit(nobs)
  #combine each vector as a column in a data.frame
  return (data.frame(id = idsC, nobs = nobs))
}

