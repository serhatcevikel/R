# using code at:
# https://www.r-bloggers.com/a-gentle-introduction-to-finance-using-r-efficient-frontier-and-capm-part-1/
# https://datashenanigan.wordpress.com/2016/05/24/a-gentle-introduction-to-finance-using-r-efficient-frontier-and-capm-part-1/

library(data.table)

calcEFParams <- function(rets)
{
     
    retbar <- colMeans(rets, na.rm = T)
    covs <- var(rets, na.rm = T) # calculates the covariance of the returns
    invS <- solve(covs)
    i <- matrix(1, nrow = length(retbar))
     
    alpha <- t(i) %*% invS %*% i
    beta <- t(i) %*% invS %*% retbar
    gamma <- t(retbar) %*% invS %*% retbar
    delta <- alpha * gamma - beta * beta
         
    retlist <- list(alpha = as.numeric(alpha),
                    beta = as.numeric(beta),
                    gamma = as.numeric(gamma),
                    delta = as.numeric(delta))
         
    return(retlist)
}
 
# load data
link <- "https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/mult_assets.csv"
 
df <- data.table(read.csv(link))
df2 <- df[,lapply(.SD, sample),]
df3 <- cbind(df, df2)
df4 <- df3[,lapply(.SD, sample),]
df5 <- cbind(df3, df4)

abcds <- calcEFParams(df5)
abcds

