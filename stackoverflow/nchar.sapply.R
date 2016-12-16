# R script - nchar(x) function is not working in sapply
# http://stackoverflow.com/questions/41193288/r-script-ncharx-function-is-not-working-in-sapply

ncharsap <- function() {
    df1 <-data.frame(valor1=c("R$ 27.144,22"," 30.035,07 "," 30.761,40 "),valor2=c("17.935,85","13.741,63","19.090,80"),valor3=c("0,00","0,00","1"))
    df1 <- as.data.frame(sapply(df1,as.character)) 
    df2 <- t(as.data.frame(apply(df1,1, function(x) nchar(x))))
    return(df2)
}
