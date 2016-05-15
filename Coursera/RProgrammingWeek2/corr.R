setwd("~/datasciencecoursera/Coursera")

corr <- function(directory, threshold = 0) {
  
  # Set working directory
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  
  # Locate files in specdata folder
  myfiles <- as.character( list.files(directory) )
  myfilepaths <- paste(directory, myfiles, sep="")
  
  # call complete function to see which pass the threshold
  frame<-complete(directory)
  
  # Get data sets that pass the threshold
  pass<-subset(frame, nobs>threshold)
  id=pass[,1]
  
  # Define empty vector to store correlations
  corrvec<-c()
  
  # Loop through id files and collect correlations.
  for(index in id) {
    new <- read.csv(myfilepaths[index], header=T, sep=",")
    #newer <- na.omit(new) # remove na didn't seem to work. Hmm.
    #newcor=cor(new[,1],new[,2])
    newer <- new[complete.cases(new),] #remove na values
    
    # Compute correlation to 5 decimals
    newcor=round(cor(newer[,"sulfate"],newer[,"nitrate"]),5) 
    corrvec<-c(corrvec,newcor)
  }
  
  # Print
  corrvec
}

  
  
  