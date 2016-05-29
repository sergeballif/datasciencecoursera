rankall <- function(outcome, num = "best") {
   ## Read outcome data
   DATA <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
   options(warn=-1) # turns off warnings for NA data
   
   ## Check that state and outcome are valid
   
   conditions <- c("heart attack", "heart failure", "pneumonia")
   OutcomeExists <- outcome %in% conditions
   if (OutcomeExists == FALSE)
      stop('invalid outcome')
   
   ## For each state, find the hospital of the given rank
   
   DATA[, 11] <- suppressWarnings(as.numeric(DATA[, 11])) # convert to numerical data
   DATA[, 17] <- suppressWarnings(as.numeric(DATA[, 17])) # suppress the ugly warning
   DATA[, 23] <- suppressWarnings(as.numeric(DATA[, 23]))
   
   if (outcome=="heart attack") # create indices for columns of smaller data frame
      considercol<-11
   if (outcome=="heart failure")
      considercol<-17
   if (outcome=="pneumonia")
      considercol<-23
   
   # Remove na's, keep only  hospital name, state, outcome columns
   consider <- data.frame(DATA[complete.cases(DATA[,c(2,7,considercol)]),c(2,7,considercol)]) 
   bystate<-split(consider,consider$State)
   ordered <- with(consider,consider[order(consider[2],consider[3],consider[1]),])
   
   # Loop through the states and add a column for Rank of each hospital
   # and a column for the value of num that our function should use.
   
   for (st in unique(ordered$State)) {
      for(j in 1:nrow(ordered[ordered$State == st,])) {
         ordered$Rank[ordered$State == st][j] <- j
         if (num == "best"){
            ordered$Specval[ordered$State == st][j] <- 1
         }
         else if (num == "worst"){
            ordered$Specval[ordered$State == st][j] <- nrow(ordered[ordered$State == st,])
         }
         else {
            ordered$Specval[ordered$State == st][j] <- num
         }
      }
   }
   
   # Remove all unnecessary columns.
   
   keep<-ordered[ordered$Rank==ordered$Specval,1:2]
   
   # Unfortunately this removed all the states that don't match the requirement.
   # Put them back in.
   
   for (st in unique(ordered$State)){
      if (st %in% keep[,2]){}
      else{
         keep[nrow(keep) + 1, ] <- c(NA,st) 
      }
   }

   keep<-keep[order(keep[2]),] # reorder the output
   names(keep)[1]<-"hospital" # rename columns
   names(keep)[2]<-"state"
   
   ## Return a data frame with the hospital names and the
   ## (abbreviated) state name
   
   keep
   
}