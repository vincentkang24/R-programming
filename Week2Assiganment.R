setwd("C:\\Users\\vince\\Desktop\\Coursera")
unzip("C:\\Users\\vince\\Desktop\\Coursera\\Specdatazip.zip", exdir = "specdata")
setwd("specdata")

part1: 
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)

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
Part2: 

complete <- function (directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
        nobs <- numeric()
        
        for (i in id) {
                data <- read.csv(filelist[i])
                nobs <- c(nobs, sum(complete.cases(data)))
        }
        ## create a file list
        data.frame(id, nobs)
        
}

Part3:

corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
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

