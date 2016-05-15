myfunction <- function(x) {
  y <- rnorm(100)
  mean(y)
}

second <- function(x) {
  x + rnorm(length(x))
}

# import Data
D <- read.csv("hw1_data.csv")

# Get vector of 1st column
D[,1]

# Get vector of 1st row
D[1,]

# Get rid of rows where 1st column entry is NA
D[complete.cases(D[,2:5]),]

# Get rid of rows where some entry is NA
D[complete.cases(D),]

