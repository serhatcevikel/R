# https://stackoverflow.com/questions/28035831/how-to-build-a-crossword-like-plot-for-a-boolean-matrix


iterate_conway <- function(dens = 0.1, dimm = 30, waitt = 0.5, maxit = 50)
{
    sizee <- dimm^2
    samp <- sample(c(rep(1, sizee * dens), rep(0, sizee * (1 - dens))))
    mm <- matrix(samp, nrow = dimm)
    mm <- rbind(F, mm, F)
    mm <- cbind(F, mm, F)
    
    neigh_count <- function(x,y) sum(mm[((-1):1)+x, ((-1):1) + y ]) - mm[x,y]
    neigh_countv <- Vectorize(neigh_count)
    
    for (i in 1:maxit)
    {
        mm2 <- outer(2:(dimm + 1), 2:(dimm + 1), neigh_countv)
        mm[-c(1, nrow(mm)), -c(1, ncol(mm))] <- mm2 %in% 2:3
        plot(rep(1:(dimm + 2), each = (dimm + 2)),
             rep(-(1:(dimm + 2)), (dimm + 2)),
             pch = ifelse(mm, 15, 0),
             cex = 2,
             asp = 1,
             xpd = NA,
             axes = FALSE,
             ann = FALSE)
        Sys.sleep(waitt)
    }
}


