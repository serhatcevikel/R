df<-  structure(list(p1 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                NA, NA, NA, NA, NA, NA, NA), p2 = structure(c(NA, NA, 5L, 6L, 
                                                                                  NA, 2L, 7L, NA, NA, 4L, NA, 3L, NA, 1L, 1L, 1L, 1L), .Label = c("", 
                                                                                      "R16", "R29", "R3", "R36", "R40", "R56"), class = "factor"), 
                             p3 = structure(c(NA, 1L, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                      NA, NA, NA, NA, NA, NA, NA), .Label = "R33", class = "factor")), .Names = c("p1", 
                         "p2", "p3"), class = "data.frame", row.names = c(NA, -17L))

dfl <- as.list(df)

dfn <- lapply(dfl, function(x) x[!is.na(x)])
