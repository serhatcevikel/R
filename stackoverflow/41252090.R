# http://stackoverflow.com/questions/41252090/is-there-a-way-to-do-this-in-sql-using-a-loop-or-easier-in-r/41252672#41252672



dailypath <- function(datap = "./data/41252090") {
    data1 <- read.csv(datap, header = T, stringsAsFactors = F)
    data1[,2] <- as.POSIXct(strptime(data1[,2], "%d/%m/%y %H:%M:%S"))
    ids <- unique(data1$ID)
    ID <- NULL
    Datetime <- NULL
    Routestage <- NULL

    for (i in ids) {
        dataid <- data1[data1$ID == i,]
        datesi <- unique(as.Date(dataid$Datetime))
        for (j in datesi) {
            dataiddate <- dataid[as.Date(dataid$Datetime) == j,]
            datetimes <- dataiddate$Datetime
            mint <- as.numeric(datetimes[-1]) - as.numeric(datetimes[-length(datetimes)]) > 180
            journeys <- c(1, cumsum(mint) + 1)

            for (z in unique(journeys)) {
                journey <- dataiddate[journeys == z,]
                datetime <- min(journey$Datetime)
                routes <- paste(journey$Routestage, sep = "-", collapse = "-")
                ID <- c(ID, i)
                Datetime <- c(Datetime, datetime)
                Routestage <- c(Routestage, routes)       
            }
        }
    }
    summarydf <- data.frame(ID = ID, Datetime = as.POSIXct(Datetime, origin="1970-01-01"), Journey = Routestage)
    return(summarydf)
}
