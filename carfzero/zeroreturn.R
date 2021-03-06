options(scipen = 999)

# get share of zero return counts in each month
#library("xlsx") #include library xlsx to read data from xls file
library("openxlsx") #include library xlsx to read data from xls file


countifzero <- function(path = ".", sepp = ";") { # count the zero returns in each month for each column
    
    setwd(path) # change working directory
    dir.create(file.path(".", "output.xlsx"), showWarnings = F) # create output directory
    #filelist <- list.files("./input") # get file list under current directory
    filelist <- list.files("./input.xls") # get file list under current directory

    for (filn in filelist) { # for1, across csv files
        #data1 <- read.csv(sprintf("./input/%s", filn)) # read data
        #data1 <- read.table(sprintf("./input/%s", filn), header = T, sep = sepp) # read data
        data1 <- openxlsx::read.xlsx(sprintf("./input.xls/%s", filn)) # read sudoku data from file into a matrix names sudoku01
        #output <- gsub(".csv", "-zero.csv", filn) # create output object name 
        output <- gsub(".xlsx", "-zero.xlsx", filn) # create output object name 
        NTfn <- gsub(".xlsx", "-NT.xlsx", filn) # create NT object name

        NT <- apply(data1, 1, function(x) length(x[x == 0 & !is.na(x)]) / length(x[!is.na(x)]) > 0.9)

        data1 <- data1[!NT,]

        averages <- aggregate(data1[,-(1:2)],
                              by = list(data1[,1], data1[,2]),
                              FUN = function(x) length(x[x == 0 & !is.na(x)]) / length(x[!is.na(x)])) # get averages

        #write.csv(NT, file = sprintf("./NT/%s", NTfn)) # write NT to file
        write.xlsx(NT, file = sprintf("./NT/%s", NTfn)) # write NT to file
        #write.csv(averages, file = sprintf("./output/%s", output)) # write to file
        write.xlsx(averages, file = sprintf("./output.xlsx/%s", output)) # write to file
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

covbymonth.xls <- function(path = ".", sepp = ";", lagg = 1) { # count the zero returns in each month for each column
    
    setwd(path) # change working directory
    dir.create(file.path(".", "output.cov.xls"), showWarnings = F) # create output directory
    filelist <- list.files("./input.xls") # get file list under current directory

    for (filn in filelist) { # for1, across csv files
        #data1 <- read.csv(sprintf("./input/%s", filn)) # read data
        data1 <- openxlsx::read.xlsx(sprintf("./input.xls/%s", filn)) # read sudoku data from file into a matrix names sudoku01
        #data1 <- read.table(sprintf("./input/%s", filn), header = T, sep = sepp) # read data
        output <- gsub(".xlsx", "-cov.csv", filn) # create output object name 

        averages <- aggregate(data1[,-(1:2)],
                              by = list(data1[,1], data1[,2]),
                              FUN = covna,
                              lagg ) # get lagged covariances

        write.csv(averages, file = sprintf("./output.cov.xls/%s", output)) # write to file
    } # close for1

}

covna <- function(x, lagg = 1) { # lagged covariances
    x <- x[!is.na(x)] # remove NAs

    if(!is.numeric(x)) NA else { # if1 the column is not numeric (all NA column, return NA else
        x1 <- x[-(1:lagg)] # trim the beginning
        x2 <- x[-(length(x) - (1:lagg) + 1)] # trim the end
        covx <- cov(x1, x2) # get lagged covariances
        if (!is.na(covx)) { # if2 covariance is not NA
            if (covx < 0) { # if3 covariance is negative
                rollx <- 2 * ((-covx)^0.5) # apply the roll measure
            } else rollx <- 0 # else3 0
        } else rollx <- NA # else2 NA
         
        return(rollx)  
    } # close if
}




