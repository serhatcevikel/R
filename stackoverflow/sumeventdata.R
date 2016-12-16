# Sum Event Data in R
# For http://stackoverflow.com/questions/41191386/sum-event-data-in-r

data1 <- structure(list(Time = structure(c(1353398400, 1353484800, 1353571200, 
    1353657600, 1353744000, 1353830400, 1353916800, 1354003200, 1354089600, 
    1354176000, 1354262400, 1354348800, 1354435200, 1354521600, 1354608000, 
    1354694400, 1354780800, 1354867200, 1354953600, 1355040000, 1355126400, 
    1355212800, 1355299200, 1355385600, 1355472000, 1355558400, 1355644800, 
    1355731200, 1355817600, 1355904000, 1355990400, 1356076800, 1356163200, 
    1356249600, 1356336000, 1356422400, 1356508800, 1356595200, 1356681600, 
    1356768000, 1356854400, 1356940800, 1357027200, 1357113600, 1357200000, 
    1357286400, 1357372800, 1357459200, 1357545600, 1357632000, 1357718400
    ), class = c("POSIXct", "POSIXt"), tzone = ""), inc = c(NA, NA, 
    NA, NA, NA, NA, NA, 0.11, NA, 0.62, 0.0899999999999999, 0.39, 
    NA, NA, 0.03, NA, NA, NA, NA, NA, NA, 0.34, NA, NA, NA, NA, 0.0600000000000001, 
    0.02, NA, NA, NA, 0.29, 0.35, 0.02, 0.27, 0.17, 0.0600000000000001, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.47, NA, NA, NA, 0.0300000000000002
    )), .Names = c("Time", "inc"), row.names = 50:100, class = "data.frame")

rainruns <- function(datas = data1) {
    incs <- c(NA, datas$inc) # last column
    event <- cumsum(is.na(incs[-length(incs)]) & !is.na(incs[-1])) # counter for rain events
    datas <- cbind(datas, event) # add events column
    datas2 <- datas[!is.na(datas$inc),] # delete na's
    summarydata1 <- aggregate(datas2$inc, by = list(datas2$event), # summarize rain data by event
                              FUN = function(x) c(length(x), sum(x), mean(x)))[[2]]
    summarydata2 <- aggregate(as.Date(datas2$Time), by = list(datas2$event), # summarize dates by event
                              FUN = function(x) c(min(x), max(x)))[[2]]
    summarydata <- data.frame(format(as.Date(summarydata2[,1], # combine both, correcting date formats
                                             origin = "1970-01-01"), "%m/%d/%Y"),
                              format(as.Date(summarydata2[,2],
                                             origin = "1970-01-01"), "%m/%d/%Y"), summarydata1)
    names(summarydata) <- c("Begin", "End", "Days", "Total", "Intensity") # update column names
    return(summarydata)
}
