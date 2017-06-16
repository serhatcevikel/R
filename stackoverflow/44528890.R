library(pryr)

timesheet <- data.frame(matrix(nrow = 0, ncol = 3))
varn <- c("date", "project", "status")
colnames(timesheet) <- varn

timesheet[1,] <- c(date(), "test", "test") 
#The above line exists because rbind was renaming the columns otherwise

print(environment())
print(ls(environment()))
sprintf("address of timesheet object is %s", pryr::address(timesheet))
envg <- environment()

new.task <- function(project, env = envg){
    # timesheet[nrow(timesheet)+1,] <<- c(date(), project, "started")
    env$timesheet[nrow(env$timesheet)+1,] <- c(date(), project, "started")
    #env$timesheet <- append(time, c(date(), paste(project, "_global", sep = ""), "started"))
    
    print(environment())
    print(ls(environment()))
    sprintf("address of timesheet object is %s", pryr::address(timesheet))
}

new.task("Name of task")
