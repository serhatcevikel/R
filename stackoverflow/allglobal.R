allglobal <- function() {
    lss <- ls(envir = parent.frame())
    for (i in lss) {
        assign(lss[1], get(lss[1], envir = parent.frame()), envir = .GlobalEnv)
    }
}

testallglob <- function() {
    test1 <- 1
    allglobal()
}
