pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the csv files.
        
        ## 'pollutant' is a character vector of length 1 indicating 
        ## the name of the pollutant: "sulfate" or "nitrate"
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant accross all monitors list
        ## in the 'id' vector (ignore NA values)
        
        files <- list.files("specdata", full.names = TRUE)
        dat <- data.frame()
        
        for(i in id) {
                dat <- rbind(dat, read.csv(files[i]))
        }
        mean(dat[, pollutant], na.rm = TRUE)
        
}

