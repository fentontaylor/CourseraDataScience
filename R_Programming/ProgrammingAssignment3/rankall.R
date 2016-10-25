## rankall takes two arguments: an outcome name (outcome) and a 
## hospital ranking (num). The function reads the outcome-of-care-measures.csv 
## file and returns a 2-column data frame containing the hospital in each state 
## that has the ranking specified in num.

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        oocdata <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE,
                            na.strings = "Not Available")
        
        ## Create column index with strings that correspond to column of data.
        outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        
        ## Check that outcome is valid. If the outcome argument is not in 
        ## the outcomes index vector, throw an error with message "invalid outcome".
        
        if(!outcome %in% names(outcomes)) {     
                stop("invalid outcome")
        }
        
        ## For each state, find the hospital of the given rank
        dat <- oocdata[,c(2,7,outcomes[outcome])]       # subset original data by cols of interest
        names(dat) <- c("Hospital", "State", "Outcome") # name the new columns
        dat <- subset(dat, !is.na(dat$Outcome))         # remove rows with NA values
        
        ranked <- dat[order(dat$State, dat$Outcome, 
                            dat$Hospital),]             # rank by state, outcome, hospital
        splat <- split(ranked, ranked$State)            # split into list by state to use lapply
        result <- lapply(splat, function(x) {           # return hospital and its state of specified num
                if(num == "best"){
                        x[1, 1:2]
                }
                else if(num == "worst") {
                        x[nrow(x), 1:2]
                }
                else {
                        x[num, 1:2]
                }
        })
        
        
        ## Return a data frame with the hospital names and the (abbreviated) state name
        goods <- do.call("rbind", result)
        
        ## This forces in the state names so there are no NA values when num is
        ## greater than the number of hospitals in the state.
        goods$State <- names(result)  
        goods							# Give the goods: the dataframe of desired hospital names
}