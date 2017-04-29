# Get xm data, parse and convert to tab

library(XML)

books <- "http://rigaux.org/language-study/syntax-across-languages.html"
startafter <- "/html/body/hr[1]"


library(plyr)

xml2 <- function() {
    ldply(xmlToList(books), function(x) { data.frame(x) } )
}

