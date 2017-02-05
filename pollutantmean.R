pollutantmean <- function(directory, pollutant, id = 1:332) {
        filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
        ## create a list that includes all of the files
        values <- numeric() 
        ## create an empty vector  
        
        for (i in id) {
                dat <- read.csv(filelist[i])
                values <- c(values, dat[[pollutant]])
        }
        mean(values, na.rm = TRUE)
}
