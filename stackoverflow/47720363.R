iter1 <- list(item1 = 1, item2 = "a")
iter2 <- list(item1 = 1, item2 = "b")
All <- list(iter1 = iter1, iter2 = iter2)

extract <- function(x, listx) sapply(listx, "[[", x)

df <- lapply(1:2, extract, All)
df <- as.data.frame(df, col.names = names(All), stringsAsFactors = F)
df



df <- as.data.frame(lapply(1:2, function(x, listx) sapply(listx, "[[", x), All), col.names = names(All), stringsAsFactors = F)
df

