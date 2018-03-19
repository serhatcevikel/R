# https://stackoverflow.com/questions/28035831/how-to-build-a-crossword-like-plot-for-a-boolean-matrix


iterate_conway <- function(dens = 0.3, dimm = 30, waitt = 0.01, maxit = 1e6, torus = T)
{
    sizee <- dimm^2
    samp <- sample(c(rep(1, sizee * dens), rep(0, sizee * (1 - dens))))
    mm <- matrix(samp, nrow = dimm)
    mm <- edit(mm)

    neigh_count <- function(x,y)
    {
        if(torus)
        {
            sum(mm[((-2):0 + x) %% dimm + 1 , ((-2):0 + y) %% dimm + 1 ]) - mm[x,y]
        }
        else
        {
            sum(mm[setdiff((-1):1 + x, c(0,dimm + 1)) , setdiff((-1):1 + y, c(0,dimm + 1))]) - mm[x,y]
        }
    }

    neigh_countv <- Vectorize(neigh_count)
   
    plotobj1 <- rep(1:dimm, each = dimm)
    plotobj2 <- rep(-(1:dimm), dimm)

    plot(plotobj1,
         plotobj2,
         pch = ifelse(mm, 15, 0),
         cex = 2,
         asp = 1,
         xpd = NA,
         axes = FALSE,
         ann = FALSE)

    for (i in 1:maxit)
    {
        mm2 <- outer(1:dimm, 1:dimm, neigh_countv)

        mm[] <- mm * (mm2 %in% 2:3) + (!mm) * (mm2 == 3)
        
        plot(plotobj1,
             plotobj2,
             pch = ifelse(mm, 15, 0),
             cex = 2,
             asp = 1,
             xpd = NA,
             axes = FALSE,
             ann = FALSE)

        Sys.sleep(waitt)
    }
}


