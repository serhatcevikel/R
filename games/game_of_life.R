# https://stackoverflow.com/questions/28035831/how-to-build-a-crossword-like-plot-for-a-boolean-matrix

editsavepattern <- function(sizee, namee)
{
    starter <- matrix(0, nrow = sizee[1], ncol = sizee[2])
    starter <- edit(starter)
    save(starter, file = sprintf("%s.RData", namee))
}


iterate_conway <- function(dens = 0.3, dimm = 30, waitt = 0.01, maxit = 1e6, torus = T, pattern = NULL)
{
    if(!is.null(pattern))
    {
        load(sprintf("%s.RData", pattern))
        dims <- dim(starter)
        dimm <- max(dimm, max(dims))
        mm <- matrix(0, nrow = dimm, ncol = dimm)
        topleftr <- floor((dimm - dims[1]) / 2)
        topleftc <- floor((dimm - dims[2]) / 2)
        mm[topleftr + (1:dims[1]), topleftc + (1:dims[2])] <- starter
    }
    else
    {
        sizee <- dimm^2
        samp <- sample(c(rep(1, sizee * dens), rep(0, sizee * (1 - dens))))
        mm <- matrix(samp, nrow = dimm)
        mm <- edit(mm)
    }

    neigh_count <- function(x,y)
    {
        if(torus)
        {
            sum(mm[((-2):0 + x) %% dimm + 1 , ((-2):0 + y) %% dimm + 1 ]) - mm[x,y]
        }
        else
        {
            xvals <- (-1):1 + x
            yvals <- (-1):1 + y
            exc <- c(0, dimm + 1)
            #sum(mm[setdiff((-1):1 + x, c(0,dimm + 1)) , setdiff((-1):1 + y, c(0,dimm + 1))]) - mm[x,y]
            sum(mm[xvals[!(xvals %in% exc)], yvals[!(yvals %in% exc)]]) - mm[x,y]
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


