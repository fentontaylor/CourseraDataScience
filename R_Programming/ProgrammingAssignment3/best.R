## The function best takes two arguments: the 2-character abbreviated 
## name of a state and an outcome name. The function reads the 
## outcome-of-care-measures.csv file and returns a character vector 
## with the name of the hospital that has the best (i.e. lowest) 
## 30-day mortality for the specified outcome in that state. 

## The outcomes can be one of "heart attack", "heart failure", or "pneumonia". 
## Hospitals that do not have data on a particular outcome should be excluded 
## from the set of hospitals when deciding the rankings.

## Hospitals are sorted by name, and then by outcome. In the event of a tie, 
## only the first hospital name is returned.

## [,2] Hospital.Name 
## [,7] State
## [,11] 30 day death rates for Heart Attack
## [,17] 30 day death rates for Heart Failure
## [,23] 30 day death rates for Pneumonia

best <- function(state, outcome) {
        ## Read outcome data
        oocdata <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE,
                            na.strings = "Not Available")
        
        ## Create column index with strings that correspond to column of data.
        outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        
        ## Check that state and outcome are valid. If the state or outcome argument
        ## are not in the ooc$State column, or outcomes index vector, 
        ## throw an error with message "invalid state" or "invalid outcome".
        if(!state %in% oocdata[,7]) {           
                stop("invalid state")         
        }
        
        if(!outcome %in% names(outcomes)) {     
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death rate.
        ## thisdata: subset of columns of original data
        ## with hospital name, state, and specified outcome
        ## newdata: reduces data by specified state and removes rows with NA outcomes
        thisdata <- oocdata[,c(2,7,outcomes[outcome])]
        names(thisdata) <- c("Hospital", "State", "Outcome")
        thisdata <- subset(thisdata, thisdata$State == state & !is.na(thisdata$Outcome))
        
        thisdata <- thisdata[order(thisdata$Hospital),]
        thisdata <- thisdata[order(thisdata$Outcome),]
        thisdata[1,1]
}