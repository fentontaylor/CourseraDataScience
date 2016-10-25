payments_cluster_analysis <- function(){
      setwd("C:/Users/sec/Desktop/Coursera/projects/RepResearchB")
      pay <- read.csv("payments.csv")
      paysub <- subset(pay, Provider.State == "NY")
      
      with(pay, plot(Average.Covered.Charges, Average.Total.Payments))
      with(subset(pay, Provider.State == "NY"), 
           plot(Average.Covered.Charges, Average.Total.Payments))
      
      x <- paysub$Average.Covered.Charges
      y <- paysub$Average.Total.Payments
      
      df <- data.frame(x=x,y=y)
      hclustering <- data.frame(x=x,y=y) %>% dist %>% hclust
}