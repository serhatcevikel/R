status_codes1 <- c("Date", "Type", "Operator", "Registration")
status_codes2 <- paste(status_codes1, ":", sep = "")

table1 <- data.frame(Status = status_codes2, Description = 1:4, stringsAsFactors = F)
table1

individual_status <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), sample(status_codes1))

table2 <- table1[sample(1:4),]

append_to_is <- function()
{
    table2 <- table1[sample(1:4),]
    n_row <- nrow(individual_status)
    cols <- gsub(":", "", table2$Status)
    individual_status[n_row + 1, cols] <<- table2$Description
    return(list(table2, individual_status))
}

