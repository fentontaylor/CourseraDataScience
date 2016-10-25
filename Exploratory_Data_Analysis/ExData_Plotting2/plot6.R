plot6 <- function() {
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
        scc <- readRDS("Source_Classification_Code.rds")
        
        neisub <- subset(nei, fips == "24510"  | fips == "06037")
        sccsub <- scc[grep("Mobile", scc$EI.Sector),]
        sccList <- sccsub$SCC
        neisub <- neisub[which(neisub$SCC %in% sccList),]
        neisub <- transform(neisub, fips = as.factor(fips), year = as.factor(as.character(year)))
        levels(neisub$fips) <- c("LA County", "Baltimore")
        
        groupcols <- c("year", "fips")
        df <- ddply(neisub, groupcols, function(x) sum(x["Emissions"]))
        names(df)[3] <- "total"
        
        p <- ggplot(df, aes(year, total, fill=fips)) +
                geom_bar(stat = "identity") +
                facet_grid(.~fips) +
                xlab("Year") +
                ylab("Tons PM2.5") +
                ggtitle("PM2.5 Emissions in LA County and Baltimore") +
                
        print(p)
        
        if(!file.exists("plot6.png")){
                print("Saving plot6.png")
                dev.copy(png, file = "plot6.png", width = 480, height = 300)
                dev.off()
        }
        else {print("File plot6.png already exists.")}
}