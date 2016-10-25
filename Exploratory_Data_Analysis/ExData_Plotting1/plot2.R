## (1) This function loads the data for only the days Feb. 1-2, 2007.
## (2) It creates a new column 'datetime' which pastes the 'Date' and 'Time'
## data together and converts it to POSIXct. 
## (3) Next, a line plot is constructed to show how the variable 'Global_active_power'
## fluctuates over the 2-day time period. 
## (4) Finally, the function saves the plot as a .png file if it does not already exist.

plot2 <- function() {
        ## (1) ##
        library(lubridate)
        setwd("C:/Users/sec/Desktop/Coursera/projects/ExData_plotting1")
        data <- "C:/Users/sec/Desktop/Coursera/data/household_power_consumption.txt"
        cnames <- names(read.table(data,header= TRUE, sep = ";", nrows = 1))
        power <- read.table(data, sep=";", na.strings="?", skip=66637, 
                            nrows=69517-66637, col.names = cnames)
        ## (2) ##
        power$datetime <- paste(power$Date, power$Time, sep = " ")
        power$datetime <- dmy_hms(power$datetime)
        
        ## (3) ##
	par(mfrow = c(1,1))
        with(power, plot(datetime, Global_active_power, type = "l", xlab="",
                         ylab = "Global Active Power (kilowatts)"))
        
        ## (4) ##
        if(!file.exists("plot2.png")){
                print("Saving plot2.png")
                dev.copy(png, file = "plot2.png", width = 480, height = 480)
                dev.off()
        }
        
        else {print("File plot2.png already exists.")}
}