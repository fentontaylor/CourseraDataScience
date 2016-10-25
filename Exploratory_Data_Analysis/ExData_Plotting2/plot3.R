plot3 <- function() {
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
        
        nei <- readRDS("summarySCC_PM25.rds")
        neisub <- subset(nei, fips == "24510")
        neisub <- transform(neisub, type=as.factor(type), year = as.factor(as.character(year)))
        
        grcol <- c("year", "type")
        df <- ddply(neisub, grcol, function(x) sum(x["Emissions"]))
        names(df)[3] <- "total"
        
        p <- ggplot(df, aes(year,total, fill = type)) +
                geom_bar(stat = "identity") +
                facet_grid(.~type) +
                ggtitle("PM2.5 Emissions in Baltimore") +
                ylab("PM2.5 Emissions (tons)") +
                xlab("Year")
        print(p)
        
        if(!file.exists("plot3.png")){
                print("Saving plot3.png")
                dev.copy(png, file = "plot3.png", width = 550, height = 220)
                dev.off()
        }
        else {print("File plot3.png already exists.")}
}