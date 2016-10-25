plot2 <- function() {
        setwd("C:/Users/sec/Desktop/Coursera/projects/ExData_Plotting2")
        
        if(!file.exists("NEI_data.zip")){
                download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                              destfile = "NEI_data.zip")
        }
        
        if(!file.exists("summarySCC_PM25.rds")) {
                unzip("NEI_data.zip", files = "summarySCC_PM25.rds")
        }
        
        nei <- readRDS("summarySCC_PM25.rds")
        
        neisub <- subset(nei, fips == "24510")
        totals <- with(neisub, tapply(Emissions, year, sum))
        
       plot(names(totals), totals,
                type = "b",
                pch = 19,
                lwd = 2,
                xlab = "Year", 
                ylab = "Emissions (tons)", 
                main = "Total Baltimore City Emissions")
        
        if(!file.exists("plot2.png")){
                print("Saving plot2.png")
                dev.copy(png, file = "plot2.png", width = 480, height = 480)
                dev.off()
        }
        else {print("File plot2.png already exists.")}
}