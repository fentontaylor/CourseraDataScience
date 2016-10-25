plot4 <- function() {
        setwd("C:/Users/sec/Desktop/Coursera/projects/ExData_Plotting2")
        library(ggplot2)
        library(plyr)
        
        if(!file.exists("NEI_data.zip")){
                download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                              destfile = "NEI_data.zip")
        }
        
        if(!file.exists("summarySCC_PM25.rds")) {
                unzip("NEI_data.zip", files = "summarySCC_PM25.rds")
        }
        
        if(!file.exists("Source_Classification_Code.rds")) {
                unzip("NEI_data.zip", files = "Source_Classification_Code.rds")
        }
        
        nei <- readRDS("summarySCC_PM25.rds")
        ssc <- readRDS("Source_Classification_Code.rds")
        
        combustion <- scc[grep("[Cc]ombustion", scc$SCC.Level.One),]
        coalComb <- combustion[grep("[Cc]oal", combustion$EI.Sector),]
        sccList <- coalComb$SCC
        subList <- which(nei$SCC %in% sccList)
        neisub <- nei[subList, ]
        totals <- with(neisub, tapply(Emissions, year, sum))/1000
        
        plot(names(totals), totals,
             type = "b",
             pch = 19,
             lwd = 2,
             xlab = "Year", 
             ylab = "Emissions (x1000 tons)", 
             main = "US Coal Combustion Emissions")
        
        if(!file.exists("plot4.png")){
                print("Saving plot4.png")
                dev.copy(png, file = "plot4.png", width = 480, height = 480)
                dev.off()
        }
        else {print("File plot4.png already exists.")}
}