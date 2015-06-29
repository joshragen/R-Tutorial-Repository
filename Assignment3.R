#outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#head(outcome)
#run lines 4,5 for histogram
#outcome[, 11] <- as.numeric(outcome[, 11])
#hist(outcome[, 11])
#ignore warning about NAs when these run

best <- function(state,outcome) {
      #three possible outcomes need to be inserted
      #"heart attack"(11), "heart failure"(17), "pneumonia"(23)
      dat <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
      out <- c("heart attack", "heart failure", "pneumonia")
      come <- c(11,17,23)
      if(!state %in% dat$State){
            stop("invalid state")
      } else if(!outcome %in% out){
            stop("invalid outcome")
      }
      #checks if the state and outcome are both possibilities
      
      look <- data.frame(out, come)
      #creates a data.frame in which the names and columns of the
      #outcomes are stored
      #next, go through this data.frame use it to find the columns
      corcol <- look$come[look$out %in% outcome]
      
      corst<-data.frame(split(dat, dat$State)[state])
      #this creates a data frame with all of the data for the state
      
      corrow<-corst[which((corst == min(corst[corcol], na.rm = TRUE)), arr.ind = TRUE)[1],]
      print(corrow[2])
      #this takes that state, checks the column, minimizes it,
      #find the row with that value and puts it in corrow
}

rankhospital <- function(state, outcome, num = "best") {
      
      dat <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
      out <- c("heart attack", "heart failure", "pneumonia")
      come <- c(11,17,23)
      if(!state %in% dat$State){
            stop("invalid state")
      } else if(!outcome %in% out){
            stop("invalid outcome")
      }
      look <- data.frame(out, come)
      corcol <- look$come[look$out %in% outcome]
      corst<-data.frame(split(dat, dat$State)[state])
      
      ord<-corst[order(corst[corcol], corst[2], na.last = NA),]
      #puts the corst frame in order of relevant column
      ranking<-cbind.data.frame(ord[2], ord[corcol], 1:nrow(ord))
      #reduces ord to just the 3 relevent columns while retaining order
      
      #num <- c("best" == 1, "worst" == nrow(ord))
      print(ranking[num, 1])
}


rankall <- function(outcome, num = "best") {
      
      dat <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
      out <- c("heart attack", "heart failure", "pneumonia")
      come <- c(11,17,23)
      if(!outcome %in% out){
            stop("invalid outcome")
      }
      look <- data.frame(out, come)
      corcol <- look$come[look$out %in% outcome]
      
      ord<-dat[order(dat[corcol], dat[2], na.last = NA),]
      #orders the list so it's already sorted next step
      
      corst<-(split(ord, ord$State))
      #puts data into a large list of data frames consisting of the states
      
      ranking<-do.call(rbind, lapply(corst, function(x) x[num,c(2,7)]))
      
      #NOTE: this program doesn't work yet right with states with less than
      #num hospitals with data, and just returns NAs for their name
}