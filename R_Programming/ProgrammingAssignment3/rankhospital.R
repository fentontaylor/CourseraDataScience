## rankhospital takes three arguments: the 2-character abbreviated name of a
## state (state), an outcome (outcome), and the ranking of a hospital in that state
## for that outcome (num). The function reads the outcome-of-care-measures.csv file 
## and returns a character vector with the name of the hospital that has the 
## ranking specified by the num argument.

rankhospital <- function(state, outcome, num = "best") {
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
        
        ## Return hospital name in that state with the given rank 30-day death rate
        
        ## Create a subset (dat), of only the columns of interest, and subset further by 
        ## state and rows that are complete cases.
        dat <- oocdata[,c(2,7,outcomes[outcome])]                       
        names(dat) <- c("Hospital", "State", "Outcome")
        dat <- subset(dat, dat$State == state & !is.na(dat$Outcome))
        
        ## Rank the data by outcome and then alphabetically. 
        ranked <- dat[order(dat$Outcome, dat$Hospital),]
        
        if(num == "best"){
                ranked[1,1]
        }
        else if(num == "worst") {
                ranked[nrow(ranked),1]
        }
        else {
                ranked[num,1]
        }
}