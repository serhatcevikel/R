#!/usr/bin/env Rscript

# piping stdin to R
# http://stackoverflow.com/questions/9370609/piping-stdin-to-r

f <- file("stdin")
open(f)
cat("input:")
write(readLines(f, n=1), stdout())

#while(length(line <- readLines(f, n=1)) > 0) {
#      write(line, stderr())
#  # process line
#}
