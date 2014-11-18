# Writes a function best() that takes TWO (2) arguments: (a) the TWO(2)-character abbreviated name of a state; and (b) an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector with the name of the 
# hospital that has the best (i.e. LOWEST) 30-day mortality for the specified outcome in that state. The hospital name is the name
# provided in the Hospital.Name variable. The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”.


best<-function(state, outcome){

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

rownum<-which.min(data[ , 3])

rowdata<-data[rownum, ]

rowdata[[1]]

}
