# HDD, SSD, RAM read/write speeds

library(microbenchmark)

SSDpath <- "/home/s"
HDDpath <- "/media/s/files"
RAMpath <- "/dev/shm"

paths <- c(SSDpath, HDDpath, RAMpath)

sourcefile <- "OI.vdi"

readandcopy <- function(input = 1, output = 1) {
    destfile <- sprintf("OI_%s.vdi", gsub(" ", "_", Sys.time()))
    sourcepath <- paste(paths[input], sourcefile, sep = "/")
    destpath <- paste(paths[output], destfile, sep = "/")
    system_call <- sprintf("cp %s %s; ls -l %s", sourcepath, destpath, destpath)
    system(system_call, intern = T, ignore.stderr = F, ignore.stdout = F)
}

benchmarkrac <- function() {
    microbenchmark(
                   readandcopy(1,1),
                   readandcopy(1,2),
                   readandcopy(1,3),
                   readandcopy(2,1),
                   readandcopy(2,2),
                   readandcopy(2,3),
                   readandcopy(3,1),
                   readandcopy(3,2),
                   readandcopy(3,3),
                   times = 1
                   )
}
