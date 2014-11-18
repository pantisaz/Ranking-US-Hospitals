# Writes a function rankhospital() that takes THREE (3) arguments: (a) the TWO(2)-character abbreviated name of a state 
# (state); (b) an outcome (outcome); and © the ranking of a hospital in that state for that outcome (num). The function reads the
# outcome-of-care-measures.csv file and returns a character vector with the name of the hospital that has the ranking specified by 
# the num argument.


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
