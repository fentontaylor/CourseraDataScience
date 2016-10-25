complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the csv files.
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id   nobs
        ## 1    117
        ## 2    1041
        ## ...
        ## where 'id' is the monitor id number and 'nobs' is the number of complete cases
        
        files <- list.files("specdata", full.names = TRUE)
        dat <- data.frame()
        tab <- data.frame()
        
        for(i in id) {
                dat <- rbind(dat, read.csv(files[i]))
        }
        
        for(i in id) {
                obs <- sum(complete.cases(subset(dat, dat[ ,"ID"] == i)))
                tab <- rbind(tab, c(i, obs))
        }
        
        colnames(tab) <- c("id", "nobs")
        rownames(tab) <- c(as.character(1:length(id)))
        tab
        
}