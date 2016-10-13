# Code to manipulate reuters data for dimension reduction
# By Serhat Cevikel
#
# Data should be saved as a csv file
# "reshape2" for casting/melting data
# stringr for string manipulation using regexp

for (package in c("reshape2", "stringr")) { # check both packages
    if(!require(package, character.only = T, quietly = T)) { # if1 the package is not installed
       install.packages(package) # install the package
       require(package, character.only = T) # and load it
    } # close if1
} # close for1

#library(reshape2) # for melting data
#library(stringr) # for string manipulation with regexp


reuters_data <- function(filename = "Serhat-Data.csv") { # get the data and reshape it into 3 columns
    dat <- read.csv(filename, header = F, stringsAsFactors = F) # read the file in csv, headers as first row so that special characters are not lost
    longnames <- dat[1,-1] # long names from the first row excluding the first one
    dat <- dat[-1,] # delete first row
    dat[,1] <- as.Date(dat[,1], "%d/%m/%Y") # convert the first column to date format
    namdat <- names(dat)[-1] # get the varnames but the V1
    datlong <- reshape2::melt(dat, id.vars = "V1") # melt the wide matrix into long matrix using the date as the id variable
    colnames(datlong)[1] <- "date" # change the name of first column to date
    compname <- str_extract(longnames, "(?:(?! - ).)*") # extract the part before " - " (if any) with regexp
    varname <- str_extract(longnames, "(?<= - )(.*)") # extract the part after " - " (if any) with regexp
    varname[is.na(varname)] <- "PRICE" # replace empty varnames with "PRICE"
    varsmat <- data.frame(namdat, compname, varname, stringsAsFactors = F) # combine column names (V2 - V31), company names and variable names into one matrix
    datlong <- base::merge(x = varsmat, y = datlong, by.x = "namdat", by.y = "variable", all.x = T) # merge data and variable names
    datlong <- datlong[,c(4,2,3,5)] # reorder and filter columns
    return(datlong)
} # close function
