# get share of zero return counts in each month

countifzero <- function(path = ".", sepp = ";") { # count the zero returns in each month for each column
    
    setwd(path) # change working directory
    dir.create(file.path(".", "output"), showWarnings = F) # create output directory
    filelist <- list.files("./input") # get file list under current directory

    for (filn in filelist) { # for1, across csv files
        #data1 <- read.csv(sprintf("./input/%s", filn)) # read data
        data1 <- read.table(sprintf("./input/%s", filn), header = T, sep = sepp) # read data
        output <- gsub(".csv", "-zero.csv", filn) # create output object name 

        averages <- aggregate(data1[,-(1:2)],
                              by = list(data1[,1], data1[,2]),
                              FUN = function(x) length(x[x == 0]) / length(x)) # get averages

        write.csv(averages, file = sprintf("./output/%s", output)) # write to file
    } # close for1

}


covbymonth <- function(path = ".", sepp = ";", lagg = 1) { # count the zero returns in each month for each column
    
    setwd(path) # change working directory
    dir.create(file.path(".", "output.cov"), showWarnings = F) # create output directory
    filelist <- list.files("./input") # get file list under current directory

    for (filn in filelist) { # for1, across csv files
        #data1 <- read.csv(sprintf("./input/%s", filn)) # read data
        data1 <- read.table(sprintf("./input/%s", filn), header = T, sep = sepp) # read data
        output <- gsub(".csv", "-cov.csv", filn) # create output object name 

        averages <- aggregate(data1[,-(1:2)],
                              by = list(data1[,1], data1[,2]),
                              FUN = covna,
                              lagg ) # get lagged covariances

        write.csv(averages, file = sprintf("./output.cov/%s", output)) # write to file
    } # close for1

}

covna <- function(x, lagg = 1) { # lagged covariances
    x <- x[!is.na(x)] # remove NAs
-
    if(!is.numeric(x)) NA else {
        x1 <- x[-(1:lagg)] # trim the beginning
        x2 <- x[-(length(x) - (1:lagg) + 1)] # trim the end
        covx <- cov(x1, x2) # get lagged covariances
        if (!is.na(covx)) {
            if (covx < 0) {
                rollx <- 2 * ((-covx)^0.5)
            } else rollx <- 0
        } else rollx <- NA    
         
        return(rollx)  
    }
}




