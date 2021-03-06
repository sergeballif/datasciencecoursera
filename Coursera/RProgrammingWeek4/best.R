best <- function(state, outcome) {
   ## Read outcome data
   DATA <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
   
   ## Check that state and outcome are valid
   StateExists <- state %in% DATA$State
   
   conditions <- c("heart attack", "heart failure", "pneumonia")
   OutcomeExists <- outcome %in% conditions
   if (StateExists == FALSE)
      stop('invalid state')
   if (OutcomeExists == FALSE)
      stop('invalid outcome')

   ## Return hospital name in that state with lowest 30-day death
   ## rate
   
   DATA[, 11] <- suppressWarnings(as.numeric(DATA[, 11])) # convert to numerical data
   DATA[, 17] <- suppressWarnings(as.numeric(DATA[, 17])) # suppress the ugly warning
   DATA[, 23] <- suppressWarnings(as.numeric(DATA[, 23]))
   
   
   if (outcome=="heart attack") # create indices for columns of smaller data frame
      considercol<-2
   if (outcome=="heart failure")
      considercol<-3
   if (outcome=="pneumonia")
      considercol<-4
   
   consider <- DATA[complete.cases(DATA[,c(2,11,17,23)]),] # remove na's
   consider <- consider[consider$State==state,] # drop the other states
   df <- data.frame(consider[,c(2,11,17,23)]) # Cols are hospital, heart attack, heart failure, pneumonia
   
   df<-df[order(df[considercol]),] # sort by the outcome
   smallest <- rapply(df[considercol],min) # identify smallest mortality rate
   newframe<-df[df[considercol]==smallest,] # discard higher mortality rates
   
   alphabetical <- newframe[order(newframe[1]),] # sort the ties alphabetically
   alphabetical[1,1] # return the name of the hospital
   }