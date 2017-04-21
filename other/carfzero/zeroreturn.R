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
