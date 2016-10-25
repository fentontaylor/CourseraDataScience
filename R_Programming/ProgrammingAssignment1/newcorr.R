corr <- function(directory, threshold = 0) {
        ##same as corr function but use lapply instead
        
        # SAMPLE DATA
        files <- list.files("specdata", full.names = TRUE)
        data <- list()
        
        for(i in seq_along(files)){
                data[i] <- list(as.data.frame(read.csv(files[i])))
        }
        
        # RESULTS VECTOR
        CorrList <- numeric()
        start <- end <- 0
        for(i in seq_along(files)) {
                thiscor <- 0
                size <- length(thiscor)
                
                if(sum(complete.cases(data[[i]])) > threshold && size > 0){
                        
                        start <- end + 1
                        end <- start + size - 1
                        
                        thiscor <- cor(x = unlist(data[[i]]$sulfate), 
                                       y = unlist(data[[i]]$nitrate), use = "complete.obs")
                        
                        CorrList[start:end] <- thiscor
                        
                }
        }
        CorrList
}