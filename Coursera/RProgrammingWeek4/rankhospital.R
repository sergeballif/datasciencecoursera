rankhospital <- function(state, outcome, num = "best") {
   ## Read outcome data
   DATA <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
   options(warn=-1) # turns off warnings for NA data
   
   ## Check that state and outcome are valid
   StateExists <- state %in% DATA$State
   
   conditions <- c("heart attack", "heart failure", "pneumonia")
   OutcomeExists <- outcome %in% conditions
   if (StateExists == FALSE)
      stop('invalid state')
   if (OutcomeExists == FALSE)
      stop('invalid outcome')
   
   ## Return hospital name in that state with the given rank
   ## 30-day death rate
   
   DATA[, 11] <- suppressWarnings(as.numeric(DATA[, 11])) # convert to numerical data
   DATA[, 17] <- suppressWarnings(as.numeric(DATA[, 17])) # suppress the ugly warning
   DATA[, 23] <- suppressWarnings(as.numeric(DATA[, 23]))
   
   
   if (outcome=="heart attack") # create indices for columns of smaller data frame
      considercol<-11
   if (outcome=="heart failure")
      considercol<-17
   if (outcome=="pneumonia")
      considercol<-23
   
   consider <- DATA[complete.cases(DATA[,c(2,considercol)]),] # remove na's
   consider <- consider[consider$State==state,] # drop the other states
   df <- data.frame(consider[,c(2,considercol)]) # Cols are hospital, heart attack, heart failure, pneumonia
   
   df<-df[order(df[2],df$Hospital.Name),] # sort by the outcome and then alphabetical
   df$rank <- 1:nrow(df) # add a column of ranks
   
   if (num == "best")
      num<-1
   if (num == "worst")
      num <- nrow(df)
      
   df[num,1]
}