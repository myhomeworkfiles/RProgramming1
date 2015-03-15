############################################################
##
## Programming assignment 1: Part 1
## Coursera course data Science: RProgramming
##
## Preconditions:
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".
## 'id' is an integer vector indicating the monitor ID numbers
## to be used
## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
############################################################
## Instructions:
## collect pollutantmean and test script in one directory.
## The specdata should be available in a subdirectory
pollutantmean <- function(directory, pollutant, id = 1:332) {
  #initialize dataset for storing from csv files
  data <- NA
  for (i in id){
    #create filename for each i 
    fileName <- paste0(directory,"/",sprintf("%03d", i),".csv")
    #read the file data for each fileName
    fileData <- read.csv(file=fileName,header=T, sep=",")
    #bind all rows of the loaded fileData to data
    data <- rbind(data, fileData)
  }
  #select only the requested column pollutant from data
  pollutants <-data[[pollutant]]
  #calculate the mean for all datasets in data without; NA values should not be considered  
  return (mean(pollutants,na.rm = T))
}
##Test cases
#pollutantmean("specdata", "sulfate", 1:10)
#pollutantmean("specdata", "nitrate", 70:72)
#pollutantmean("specdata", "nitrate", 23)


