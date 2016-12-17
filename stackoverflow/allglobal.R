allglobal <- function() {
    lss <- ls(envir = parent.frame())
    for (i in lss) {
        assign(i, get(i, envir = parent.frame()), envir = .GlobalEnv)
    }
}

testallglob <- function() {
    test1 <- 1
    test2 <- function() 2
    allglobal()
}

# eval(parse(text=sprintf("class(%s)", lss[1])))

