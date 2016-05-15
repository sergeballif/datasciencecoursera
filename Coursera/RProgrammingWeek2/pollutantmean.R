
setwd("~/datasciencecoursera/Coursera")

pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {

  # Set working directory
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }

  # Locate files in specdata folder
  myfiles <- as.character( list.files(directory) )
  myfilepaths <- paste(directory, myfiles, sep="")
  
  # Initialize a vector to hold the pollutant data
  alldata <- c()
  
  # Loop through id files and collect the data values that are !na.
  for(index in id) {
    new <- read.csv(myfilepaths[index], header=T, sep=",")
    polcol <- new[,pollutant] # just pollutant column
    clean <- na.omit(polcol) # remove na
    alldata <- c(alldata, clean) # concatenate
  }
  
  # Calculate the mean and round to 3 decimal places.
  round(mean(alldata),digits=3)
}


