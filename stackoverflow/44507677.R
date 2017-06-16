id <- c(1,1,1,1,1,1,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4)
start <- c(NA, NA, NA, 1, NA, NA, NA, NA, 1, NA, NA, NA, 1, NA, NA, NA, NA, NA, 1, NA, NA, NA)
e <- as.data.frame(cbind(id, start))

e$target <- NA
for (i in 2:length(e$id)){
      if (e$id[i]!=e$id[i-1]){
              e$target[i] <- NA
  } else {
          e$target[i] <- e$target[i-1]+1
      if (!is.na(e$start[i]==1)){
                e$target[i] <- 0
          }
        }
}
