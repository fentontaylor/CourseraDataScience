## (1) This function loads the data for only the days Feb. 1-2, 2007.
## (2) It plots a histogram of the variable Global_active_power over those 2 days.
## (3) The function saves the plot as a .png file if it does not already exist.


plot1 <- function() {
        ##(1)##
        setwd("C:/Users/sec/Desktop/Coursera/projects/ExData_plotting1")
        data <- "C:/Users/sec/Desktop/Coursera/data/household_power_consumption.txt"
        cnames <- names(read.table(data,header= TRUE, sep = ";", nrows = 1))
        power <- read.table(data, sep=";", na.strings="?", skip=66637, 
                            nrows=69517-66637, col.names = cnames)
        ##(2)##
	par(mfrow = c(1,1))
        with(power, hist(Global_active_power, col = "red", 
                         xlab = "Global Active Power (kilowatts)", 
                         main = "Global Active Power"))
        ##(3)##
        if(!file.exists("plot1.png")){
                print("Saving plot1.png")
                dev.copy(png, file = "plot1.png", width = 480, height = 480)
                dev.off()
        }
        
        else {print("File plot1.png already exists.")}
}