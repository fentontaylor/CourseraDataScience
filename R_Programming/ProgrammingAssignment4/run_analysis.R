## This code reads in data from multiple files for testing and training experiments, merges it into a 
## single data frame, subsets based on certain column, then calculates cloumn means grouped by
## subject and activity. 

run_analysis <- function(){
        
        library(plyr)
        
        if(file.exists("C:/Users/sec/Desktop/Coursera/projects/ProgrammingAssignment4/UCI_HAR_Dataset/allData.csv") &
           file.exists("C:/Users/sec/Desktop/Coursera/projects/ProgrammingAssignment4/tidyDataMeans.txt")){
                stop("The data files you want already exist: allData.csv, tidyDataMeans.txt")
        }

        # Merges the training and the test sets to create one data set.
        setwd("C:/Users/sec/Desktop/Coursera/projects/ProgrammingAssignment4/UCI_HAR_Dataset/test")
        fl1 <- dir(pattern = "(.txt)$")
        testList <- lapply(fl1, read.table)
        testData <- do.call("cbind", testList)
        
        setwd("C:/Users/sec/Desktop/Coursera/projects/ProgrammingAssignment4/UCI_HAR_Dataset/train")
        fl2 <- dir(pattern = "(.txt)$")
        trainList <- lapply(fl2, read.table)
        trainData <- do.call("cbind", trainList)
        rm(testList, trainList)
        allData <- rbind(testData,trainData)
        
        # Labels the data set with descriptive variable names from "features.txt"
        setwd("C:/Users/sec/Desktop/Coursera/projects/ProgrammingAssignment4/UCI_HAR_Dataset")
        labels <- read.table("features.txt")
        labels <- as.character(labels$V2)
        names(allData) <- c("subject", labels, "activity")
        
        # Uses descriptive activity names to name the activities in the data set
        actnames <- read.table("activity_labels.txt")
        actnames <- actnames$V2
        allData$activity <- as.factor(allData$activity)
        levels(allData$activity) <- actnames
        
        # Arranges the data by subject and activity and reorders the columns.
        allData <- arrange(allData, subject, activity)
        allData <- allData[,c(1,563,2:562)]
	
        # Write 'allData.csv' if it does not already exist.
        if(!file.exists("allData.csv")){
	        write.csv(allData, file = "allData.csv", row.names = FALSE)
	}

        # Extracts only the measurements on the mean and standard deviation for each measurement. 
        cols <- grep("(mean()|std())",names(allData))
        subData <- allData[,c(1:2,cols)]
        
        # Creates a second, independent tidy data set with the average of each variable 
        # for each activity and each subject
        groupCols <- c("subject", "activity")
        dataCols <- names(subData)[3:length(subData)]
        tidyDataMeans <- ddply(subData, groupCols, function(x) colMeans(x[dataCols]))
        
        # Write 'tidyDataMeans.txt' if it does not already exist.
        if(!file.exists("C:/Users/sec/Desktop/Coursera/projects/ProgrammingAssignment4/tidyDataMeans.txt")){
                write.table(tidyDataMeans, file = "C:/Users/sec/Desktop/Coursera/projects/ProgrammingAssignment4/tidyDataMeans.txt", row.names = FALSE)
        }
}