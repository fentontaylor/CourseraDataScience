plot1 <- function() {
        setwd("C:/Users/sec/Desktop/Coursera/projects/ExData_Plotting2")
        
        if(!file.exists("NEI_data.zip")){
                download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                              destfile = "NEI_data.zip")
        }
        
        if(!file.exists("summarySCC_PM25.rds")) {
                unzip("NEI_data.zip", files = "summarySCC_PM25.rds")
        }
        
        nei <- readRDS("summarySCC_PM25.rds")
        
        totals <- with(nei, tapply(Emissions, year, sum))
        totals <- totals/1000000
        
        plot(names(totals), totals, 
             pch = 20, 
             cex = 2, 
             lwd = 2, 
             type = "b",
             xlab = "Year",
             ylab = "Emissions (millions of tons)",
             main = "Total US Emissions of PM2.5 Per Year")
        
        if(!file.exists("plot1.png")){
                print("Saving plot1.png")
                dev.copy(png, file = "plot1.png", width = 480, height = 480)
                dev.off()
        }
        else {print("File plot1.png already exists.")}
}