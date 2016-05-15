setwd("~/datasciencecoursera/Coursera")

complete <- function(directory, id = 1:332) {
  
  # Set working directory
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  
  # Locate files in specdata folder
  myfiles <- as.character( list.files(directory) )
  myfilepaths <- paste(directory, myfiles, sep="")
  
  # Initialize a vector to hold the count of complete rows
  nobs <- c()
  
  # Loop through id files and count the complete rows.
  for(index in id) {
    new <- read.csv(myfilepaths[index], header=T, sep=",")
    nobs<- c(nobs,nrow(new[complete.cases(new),]))
  }
  
  # Create the data frame
  df=data.frame(id,nobs)
  df
}


