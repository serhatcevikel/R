# answer to
# http://puzzling.stackexchange.com/questions/47219/little-bit-of-a-code-in-binary

puz <- read.csv("47219", header = F)
puz <- puz[,-ncol(puz)]

colxor <- function(keys = puz[-(15:16),1:2], mat = puz[-(15:16),-(1:2)]) {
    keys <- as.matrix(keys)
    mat <- as.matrix(mat)
    colkey <- ncol(keys)
    colmat <- ncol(mat)
    keymat <- keys[,rep(1:colkey, 7)] # replicate columns
    matxor <- t(sapply(1:colmat, function(x) bitwXor(mat[,x], keymat[,x])))
    dec1 <<- apply(matxor[1:7,], 2, function(x) sum(2^(6:0) * x))
    asc1 <<- sapply(dec1, function(x) rawToChar(as.raw(x)))
    dec2 <<- apply(matxor[8:14,], 2, function(x) sum(2^(6:0) * x))
    asc2 <<- sapply(dec2, function(x) rawToChar(as.raw(x)))
    return(list(asc1, asc2, matxor))
}

rowxor <- function(keys = puz[15:16,-(1:2)], mat = matxor[-(15:16),]) {
    keys <- as.matrix(keys)
    mat <- as.matrix(mat)
    rowkey <- nrow(keys)
    rowmat <- nrow(mat)
    keymat <- keys[rep(1:rowkey, 7),] # replicate columns
    matxor <- t(sapply(1:rowmat, function(x) bitwXor(mat[x,], keymat[x,])))
    dec1 <<- apply(matxor[1:7,], 2, function(x) sum(2^(0:6) * x))
    asc1 <<- sapply(dec1, function(x) rawToChar(as.raw(x)))
    dec2 <<- apply(matxor[8:14,], 2, function(x) sum(2^(0:6) * x))
    asc2 <<- sapply(dec2, function(x) rawToChar(as.raw(x)))
    return(list(asc1, asc2, matxor))
}


