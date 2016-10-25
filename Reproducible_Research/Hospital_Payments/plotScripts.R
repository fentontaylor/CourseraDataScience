#########################################################################################
# ChargesVsPayments is a function that takes a 2-letter state abbreviation
#       as its only argument. The function will subset the data for that 
#       particular state and create a plot of the log10 of Average.Covered.Charges
#       and the Average.Total.Payments. Other information is added as text to the plot.
#       The plot is saved as a pdf and opened for viewing.
# 
# 
########################################################################################
ChargesVsPayments <- function(state = NULL){
        # Set working directory and download data file if necessary.
        setwd("C:/Users/sec/Desktop/Coursera/projects/RepResearchB")
        
        # Check to see if the pdf file of the plot already exists. If not, run the analysis.
        if(!file.exists(paste(state,"ChargesVsPayments.pdf", sep = ""))) {
                
                if(!file.exists("payments.csv")){
                        url <- "https://d18ky98rnyall9.cloudfront.net/_e143dff6e844c7af8da2a4e71d7c054d_payments.csv?Expires=1469059200&Signature=ZpiU8GV4KxCTAZrKXDOiCUXdKIke2Z1xudxdm9I-wOMC-iDUyUjzMZnRrOQ8TbrePg5Y9RVf3SR7UJuJoNGnxnZ1dl069KTyTJKDOvDKa5OmolgZdTh8NFzKCeCfmfdXqgPoqq4sQyEUhIsqiENedfFV3JGMpkQcSkSUIRFzySk_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"
                        download.file(url, destfile = "payments.csv")
                }
                
                # Read in data from payments.csv
                payments <- read.csv("payments.csv")
                
                # Create character vector of state names and check validity of function argument.
                validStates <- as.character(unique(payments$Provider.State))
                
                if(!(state %in% validStates)) {
                        stop("Invalid state -- Please choose from: CA, FL, IL, NY, PA, TX")
                }
                
                # Subset data for the desired state as specified in the function argument.
                paysub <- subset(payments, Provider.State == state)
                
                # Open pdf device.
                pdf(file=paste0(state,"ChargesVsPayments.pdf"))
                
                # Plot the log of Average.Covered.Charges vs. the log of Average.Total.Payments
                # The log of was taken because the data are heavily skewed toward lower values.
                x <- log10(paysub$Average.Covered.Charges)
                y <- log10(paysub$Average.Total.Payments)
                plot(x, y,
                        pch = 19,
                        col = rgb(0,0.2,1,0.3),
                        main = paste("Correlation of Average Medical Bill Charges \nvs. Average Payments in",state),
                        xlab = expression("Log"[10]* " of Average Covered Charges"),
                        ylab = expression("Log"[10]* " of Average Total Payments"),
                        ylim = c(3.5, 4.6),
                        xlim = c(3.5, 5.5))
                
                # Plot a linear model to show relationship of the data.
                reg1 <- lm(y ~ x)
                abline(reg1, 
                       col = rgb(1,.5,0,.8), 
                       lwd = 2,
                       lty = 2)
                # Make a legend to identify plot information
                legend("bottomright", 
                       legend = c(expression("Log"[10]*" of Data Points"), "Linear Model"),
                       pch = c(19,NA),
                       lty = c(NA,2),
                       lwd = c(NA,2),
                       col = c(rgb(0,0.2,1,0.5),rgb(1,.5,0,.8)))
                # Text information for data before the log is taken.
                text(x = 3.5, y = 4.55,
                     pos = 4,
                     cex = 0.7,
                     labels = paste("Mean of Avg. Covered Charges =", floor(mean(10^x))))
                text(x = 3.5, y = 4.5,
                     pos = 4,
                     cex = 0.7,
                     labels = paste("Mean of Avg. Tot. Payments =", floor(mean(10^y))))
                text(x = 3.5, y = 4.45,
                     pos = 4,
                     cex = 0.7,
                     labels = paste("Ratio of Payments/Charges =",
                                    round(mean(10^y)/mean(10^x),4)))
                text(x = 3.5, y = 4.4,
                     pos = 4,
                     cex = 0.7,
                     labels = paste("Slope of Linear Model =", 
                                    round(reg1$coefficients[[2]],4)))
                
                # Close pdf device.
                print(paste("Saving ", state, "ChargesVsPayments.pdf", sep = ""))
                dev.off()
        }
        # If the file already exists, alert the user.
        else {print("File already exists.")}
        
        # Create a path to the file and open it for viewing pleasure.
        print(paste0("Opening file ", state, "ChargesVsPayments.pdf"))
        path <- paste0(getwd(),"/",state,"ChargesVsPayments.pdf")
        system(paste0('open "', path, '"'))
}

payments_plot2 <- function(){
        # Set working directory and download data file if necessary.
        setwd("C:/Users/sec/Desktop/Coursera/projects/RepResearchB")
        library(RColorBrewer)
        library(scales)
        
        # Check to see if the pdf file of the plot already exists. If not, run the analysis.
        if(!file.exists("payments_plot2.pdf")){
                
                if(!file.exists("payments.csv")){
                        url <- "https://d18ky98rnyall9.cloudfront.net/_e143dff6e844c7af8da2a4e71d7c054d_payments.csv?Expires=1469059200&Signature=ZpiU8GV4KxCTAZrKXDOiCUXdKIke2Z1xudxdm9I-wOMC-iDUyUjzMZnRrOQ8TbrePg5Y9RVf3SR7UJuJoNGnxnZ1dl069KTyTJKDOvDKa5OmolgZdTh8NFzKCeCfmfdXqgPoqq4sQyEUhIsqiENedfFV3JGMpkQcSkSUIRFzySk_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"
                        download.file(url, destfile = "payments.csv")
                }
                
                # Read in data from payments.csv
                payments <- read.csv("payments.csv")
                
                # Open a pdf device.
                pdf(file = "payments_plot2.pdf", height = 7.5, width = 8.614583)
                
                # Choose colors to represent various medical conditions.
                myColors <- c("purple", 
                              "darkgoldenrod1", 
                              "hotpink1", 
                              "limegreen", 
                              "royalblue", 
                              "turquoise")
                
                # Set parameters for 6x6 plot, with large bottom margin to hold the legend.
                par(mfrow = c(6,6),
                    mar = c(0, 0, 0, 0), 
                    oma = c(15, 5, 7, 1),
                    tcl = -0.25)
                
                # Nested for loop plots states by rows and medical conditions by column.
                # Color is assigned to medical condition, and the first plot in each row is
                # labeled with the state.
                for(ps in levels(payments$Provider.State)){
                        for(drg in levels(payments$DRG.Definition)){
                                # subset by unique state and med cond.
                                paysub <- subset(payments, DRG.Definition == drg & Provider.State == ps)
                                # take log of values so that x,y scales normalize
                                x <- log10(paysub$Average.Covered.Charges)
                                y <- log10(paysub$Average.Total.Payments)
                                
                                with(paysub, plot(x,y,
                                                  axes = FALSE,         # so that each plot appears without clutter
                                                  ylim = c(3.5, 4.6),
                                                  xlim = c(3.5, 5.5),
                                                  pch = 19,
                                                  col = alpha(myColors[DRG.Definition], 0.2)))
                                
                                # add linear regression to help visualize relationship.
                                abline(lm(y~x), lty = 2, col = rgb(1,0,0,0.8))
                                
                                # add x-axis ticks and numbering
                                if(ps == levels(payments$Provider.State)[6]){
                                        axis(1, col = "grey40", col.axis = "grey20", 
                                             at = seq(3.5,5.0, 0.5))
                                }
                                
                                # add y-axis ticks and numbering
                                if(drg == levels(payments$DRG.Definition)[1]){
                                        axis(2, col = "grey40", col.axis = "grey20", 
                                             at = seq(3.6,4.6, 0.2))
                                        mtext(ps, side = 3, line = -1, adj = 0.1, 
                                              padj = 0.7, cex = 0.8, col = "grey20")
                                }
                                box(col = "grey60")
                        }
                }
                # Add legend, main title, x-axis label, and y-axis label
                legend(x = -5.0, y = 2.4,
                       title = "Medical Conditions", 
                       inset = c(0.0,0.0),
                       legend = levels(payments$DRG.Definition),
                       col = myColors,
                       pch = 19,
                       xpd = NA)
                mtext("Correlation of Average Medical Charges and Average Payments \nby State and Medical Condition", 
                      side = 3, 
                      outer = TRUE, 
                      padj = -0.5, 
                      cex=1.5)  
                mtext(expression("Log"[10]* " of Average Covered Charges"), 
                      side = 1, 
                      outer = TRUE, 
                      line = 2.2,
                      padj = 0.7,
                      col = "grey20")
                mtext(expression("Log"[10]* " of Average Total Payments"), 
                      side = 2, 
                      outer = TRUE, 
                      line = 2.2, 
                      col = "grey20")
                
                # close pdf device and open file for viewing
                dev.off()
                
        }
        # If the file already exists, alert the user.
        else {print("File already exists.")}
        path <- paste0(getwd(),"/payments_plot2.pdf")
        system(paste0('open "', path, '"'))
}