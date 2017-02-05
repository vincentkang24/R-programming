corr <- function(directory, threshold = 0) {
        dataframe <- complete(directory)
        ids <- df[df["nobs"] > threshold, ]$id
        corrs <- numeric()
        for (i in ids) {
                
                newRead <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                                         ".csv", sep = ""))
                        dff <- newRead[complete.cases(newRead), ]
                corrs <- c(corrs, cor(dff$sulfate, dff$nitrate))
        }
        return(corrs)
}
