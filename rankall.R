# Writes a function called rankall() that takes TWO (2) arguments: (a) an outcome name (outcome); and (b) a hospital ranking (num). 
# The function reads the outcome-of-care-measures.csv file and returns a TWO(2)-column data frame containing the hospital in EACH 
# state that has the ranking specified in num.

rankall<- function(outcome, num){
  
bigdata<-read.csv("outcome-of-care-measures.csv", colClasses = "character", na.string = "Not Available")

findoutcome<-any(c("heart attack", "heart failure", "pneumonia")==outcome)

hospitals<- character()

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

statelist<-sort((unique(bigdata[,7])))

for (i in 1:54){
  data<-subset(subset1, State == statelist[i])
  
  sortdata<-data[order(as.numeric(as.character(data[,3])), data$Hospital.Name, na.last = NA, decreasing = FALSE), ]
  
  if (num == "best") {
    hnum <- 1
  }  else if (num == "worst"){
    hnum <- which.max(sortdata[,3])
  } else {
    hnum <- num
  }
  
  rowdata<- sortdata[hnum, ]
  
  hospitals[i]<-rowdata[[1]] 
}
  
data<-data.frame(hospital=hospitals, state=statelist)

data

}
