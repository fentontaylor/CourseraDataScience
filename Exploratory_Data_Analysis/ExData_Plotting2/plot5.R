plot5 <- function() {
        setwd("C:/Users/sec/Desktop/Coursera/projects/ExData_Plotting2")
        
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
        
        neisub <- subset(nei, fips == "24510")
        sccsub <- scc[grep("Mobile", scc$EI.Sector),]
        sccList <- sccsub$SCC
        neisub <- neisub[which(neisub$SCC %in% sccList),]
        
        plot(names(totals), totals,
             type = "b",
             pch = 19,
             lwd = 2,
             xlab = "Year", 
             ylab = "Emissions (tons)", 
             main = "Baltimore Motor Vehicle Emissions")
        
        if(!file.exists("plot5.png")){
                print("Saving plot5.png")
                dev.copy(png, file = "plot5.png", width = 480, height = 480)
                dev.off()
        }
        else {print("File plot5.png already exists.")}
}