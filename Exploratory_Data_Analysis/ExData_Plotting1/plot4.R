## (1) This function loads the data for only the days Feb. 1-2, 2007.
## (2) It creates a new column 'datetime' which pastes the 'Date' and 'Time'
## data together and converts it to POSIXct. 
## (3) Sets parameters to have 4 plots on one device.
## (4) A line plot is constructed to show how the variable 'Global_active_power'
## fluctuates over the 2-day time period. 
## (5) Each of the three Sub_metering variable line plots are overlayed.
## (6)
## (7)
## (8) Finally, the function saves the plot as a .png file if it does not already exist.

plot4 <- function() {
        #(1)#
        library(lubridate)
        setwd("C:/Users/sec/Desktop/Coursera/projects/ExData_plotting1")
        cnames <- names(read.table("household_power_consumption.txt", 
                                   header= TRUE, sep = ";", nrows = 1))
        power <- read.table("household_power_consumption.txt", sep=";", 
                            na.strings="?", skip=66637, 
                            nrows=69517-66637, col.names = cnames)
        #(2)#
        power$datetime <- paste(power$Date, power$Time, sep = " ")
        power$datetime <- dmy_hms(power$datetime)
        
        #(3)#
        par(mfcol = c(2,2))
        
        #(4)#
        with(power, plot(datetime, Global_active_power, type = "l", xlab="",
                         ylab = "Global Active Power (kilowatts)"))
        #(5)#
        with(power, plot(datetime, Sub_metering_1, 
                         type = "n", 
                         xlab = "", 
                         ylab = "Energy Sub Metering"))
        lines(power$datetime, power$Sub_metering_1)
        lines(power$datetime, power$Sub_metering_2, col = "red")
        lines(power$datetime, power$Sub_metering_3, col = "blue")
        legend("topright", 
               legend = names(power)[7:9],
               col = c("black", "blue", "red"),
               lty = 1,
               bty = "n"
               )
        
        #(6)#
        with(power, plot(datetime, Voltage, type = "l"))
        
        #(7)#
        with(power, plot(datetime, Global_reactive_power, type = "l"))
        
        #(8)#
        if(!file.exists("plot4.png")){
                print("Saving plot4.png")
                dev.copy(png, file = "plot4.png", width = 480, height = 480)
                dev.off()
        }
        
        else {print("File plot4.png already exists.")}
}## (1) This function loads the data for only the days Feb. 1-2, 2007.
## (2) It creates a new column 'datetime' which pastes the 'Date' and 'Time'
## data together and converts it to POSIXct. 
## (3) Sets parameters to have 4 plots on one device.
## (4) A line plot is constructed to show how the variable 'Global_active_power'
## fluctuates over the 2-day time period. 
## (5) Each of the three Sub_metering variable line plots are overlayed.
## (6) Creates a line plot of 'Voltage' over the 2-day period.
## (7) Creates a line plot of 'Global_reactive_power' over the 2-day period.
## (8) Finally, the function saves the plot as a .png file if it does not already exist.

plot4 <- function() {
        #(1)#
        library(lubridate)
        setwd("C:/Users/sec/Desktop/Coursera/projects/ExData_plotting1")
        data <- "C:/Users/sec/Desktop/Coursera/data/household_power_consumption.txt"
        cnames <- names(read.table(data,header= TRUE, sep = ";", nrows = 1))
        power <- read.table(data, sep=";", na.strings="?", skip=66637, 
                            nrows=69517-66637, col.names = cnames)
        #(2)#
        power$datetime <- paste(power$Date, power$Time, sep = " ")
        power$datetime <- dmy_hms(power$datetime)
        
        #(3)#
        par(mfcol = c(2,2))
        
        #(4)#
        with(power, plot(datetime, Global_active_power, type = "l", xlab="",
                         ylab = "Global Active Power (kilowatts)"))
        #(5)#
        with(power, plot(datetime, Sub_metering_1, 
                         type = "n", 
                         xlab = "", 
                         ylab = "Energy Sub Metering"))
        lines(power$datetime, power$Sub_metering_1)
        lines(power$datetime, power$Sub_metering_2, col = "red")
        lines(power$datetime, power$Sub_metering_3, col = "blue")
        legend("topright", 
               legend = names(power)[7:9],
               col = c("black", "blue", "red"),
               lty = 1,
               box.col = "white",
               inset = c(.2,.04)
               )
        
        #(6)#
        with(power, plot(datetime, Voltage, type = "l"))
        
        #(7)#
        with(power, plot(datetime, Global_reactive_power, type = "l"))
        
        #(8)#
        if(!file.exists("plot4.png")){
                print("Saving plot4.png")
                dev.copy(png, file = "plot4.png", width = 480, height = 480)
                dev.off()
        }
        
        else {print("File plot4.png already exists.")}
}