# Writes a function rankhospital() that takes THREE (3) arguments: (a) the TWO(2)-character abbreviated name of a state 
# (state); (b) an outcome (outcome); and © the ranking of a hospital in that state for that outcome (num). The function reads the
# outcome-of-care-measures.csv file and returns a character vector with the name of the hospital that has the ranking specified by 
# the num argument.

# Hospitals that do NOT have data on a particular outcome are excluded from the set of hospitals when deciding the rankings.

# If there is MORE THAN ONE (1) hospital for a given ranking, then the hospital names are sorted in alphabetical order and the
# FIRST (1st) hospital in that set is returned (i.e. if hospitals “b”, “c”, and “f” are tied for a given rank, then hospital 
# “b” is returned).

# The function checks the validity of its arguments. If an invalid state value is passed to rankhospital(), the function
# throws an error via the stop() function with the exact message “invalid state”. If an invalid outcome value is passed to
# rankhospital(), the function throws an error via the stop() function with the exact message “invalid outcome”. The num 
# variable can take values “best”, “worst”, or an integer indicating the ranking (SMALLER numbers are better). If the number 
# given by num is larger than the number of hospitals in that state, then the function returns NA.


rankhospital<-function(state, outcome, num){
  
bigdata<-read.csv("outcome-of-care-measures.csv", colClasses = "character", na.string = "Not Available")
 
findoutcome<-any(c("heart attack", "heart failure", "pneumonia")==outcome)
findstate<-any(bigdata[ , 7]==state)

if (findstate == FALSE){
  stop("invalid state")
}

if (findoutcome == FALSE){
  stop("invalid outcome")
}

if (outcome == "heart attack"){
    outcomecol<- 11
  } else if (outcome == "heart failure") {
    outcomecol <- 17
  } else if (outcome == "pneumonia") {
    outcomecol <-23 
  }

subset1<-bigdata[, c(2,7, outcomecol)]

data<-subset(subset1, State == state)

sortdata<-data[order(as.numeric(as.character(data[,3])), data$Hospital.Name, na.last = NA, decreasing = FALSE), ]

if (num == "best") {
  hnum <- 1
}  else if (num == "worst"){
  hnum <- which.max(sortdata[,3])
} else {
  hnum <- num
}

rowdata<- sortdata[hnum, ]

rowdata[[1]]

}
