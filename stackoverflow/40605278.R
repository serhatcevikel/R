v1 <- c(A4 = 0, A2 = 0, A5 = 0, A1 = 3, A6 = 5)
v2 <- c(A5 = 3, A7 = 1, A2 = 4)

repvecs <- function(a = v1, b = v2) {
    commons <- names(a)[names(a) %in% names(b)]
    a[commons] <- b[commons]
    return(a)
}
