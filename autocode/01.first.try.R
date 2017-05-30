# a code which writes itself
# deneme

autocode.main <<- function() {
    continue <- T

    while(continue) {
        source("01.first.try.R")
        continue <- readline(prompt = "do you want to continue? T/F (default T)\n") # move input by you
        if(continue == "") continue <- T else continue <- as.logical(continue)
        if (continue) {
            deb <- readline(prompt = "do you want to debug? T/F (default F)\n") # move input by you
            if(deb == "") deb <- F else deb <- as.logical(deb)
            if (deb) debug(autocode.expand)
            append.text <- autocode.expand()
            if (deb) undebug(autocode.expand)
            catt <- readline(prompt = "do you want to see the new code? T/F (default F)\n") # move input by you
            if(catt == "") catt <- F else catt <- as.logical(catt)
            if (catt) system("cat 01.first.try.R") 

            linen <- as.integer(system("sed -n '/# check.1/=' 01.first.try.R", intern = T)[2]) - 1
            #system(sprintf("sed -i '%s i \\\t\\\t%s' 01.first.try.R", linen, append.text))
            system(sprintf("sed -i '%s c \\\t\\\t\\\t%s' 01.first.try.R", linen, append.text))
            "[["(body(autocode.main), c(3,3,5,3,14)) <<- substitute(append.text)
            trace(what = autocode.main, tracer = quote(parse(text = append.text)), at = list(c(3,3,5,3,14)), where = .GlobalEnv)

			aswpalosyo()
            # check.1

            }
    }
}


autocode.expand <<- function() {
    char.count <- sample(3:10, 1)
    function.name <- paste(sample(letters, char.count, replace = T), collapse = "")
    var.name <- function.name
    #var.name <- paste(sample(letters, char.count, replace = T), collapse = "")
    function.value <- sample(1:1e4, 1)
    new.function.text <- sprintf("%s <<- function() {%s <- %s; print(paste(\"%s\", \"is\", %s, sep = \" \"))}",
                                 function.name,
                                 var.name, 
                                 function.value,
                                 var.name,
                                 function.value)
    eval(parse(text = new.function.text))
    do.call(function.name, list())
    system(sprintf("sed -i '$ a %s' 01.first.try.R", new.function.text))
    linen <- as.integer(system("sed -n '/# check.2/=' 01.first.try.R", intern = T)[2])
    append.text <- sprintf("%s()", function.name)
    system(sprintf("sed -i '%s i \\\t%s' 01.first.try.R", linen, append.text))
    del <- sample(c(T,T,T,F), 1)
    if (del) {
        func.st <- as.integer(system("sed -n '/# func.st/=' 01.first.try.R", intern = T)[2]) + 1
        func.end <- linen
        del.line <- sample(func.st:func.end, 1)
        linee <- system(sprintf("sed -n %sp 01.first.try.R", del.line), intern = T)
        system(sprintf("sed -i '%sd' 01.first.try.R", del.line))
        print(sprintf("Sorry I deleted line %s: \"%s\"", del.line, linee))
    } 

    # func.st    
	orxpqkfdaj()
	iddcv()
	rewcfpftet()
	bgimfg()
	kwqhmr()
	lxn()
	mewupi()
	rxagav()
	neiwj()

	xdnxn()
	pvian()
	spxrsloiv()
	prde()
	hwrqp()
	xsrhq()
	bwpimuutn()
	jcj()
	jzgkzko()
	altdtuidil()
	ztgnmyy()
	aswpalosyo()
    # check.2
    return(append.text)
}


ggiwhlwa <<- function() {ggiwhlwa <- 3350; print(paste("ggiwhlwa", "is", 3350, sep = " "))}
orxpqkfdaj <<- function() {orxpqkfdaj <- 6719; print(paste("orxpqkfdaj", "is", 6719, sep = " "))}
aubsvvhj <<- function() {aubsvvhj <- 7048; print(paste("aubsvvhj", "is", 7048, sep = " "))}
zha <<- function() {zha <- 6089; print(paste("zha", "is", 6089, sep = " "))}
kappx <<- function() {kappx <- 2624; print(paste("kappx", "is", 2624, sep = " "))}
sqzynp <<- function() {sqzynp <- 5274; print(paste("sqzynp", "is", 5274, sep = " "))}
iddcv <<- function() {iddcv <- 4114; print(paste("iddcv", "is", 4114, sep = " "))}
qwlqju <<- function() {qwlqju <- 3559; print(paste("qwlqju", "is", 3559, sep = " "))}
itmk <<- function() {itmk <- 1783; print(paste("itmk", "is", 1783, sep = " "))}
eaoulq <<- function() {eaoulq <- 4480; print(paste("eaoulq", "is", 4480, sep = " "))}
ibomtjitao <<- function() {ibomtjitao <- 2226; print(paste("ibomtjitao", "is", 2226, sep = " "))}
rewcfpftet <<- function() {rewcfpftet <- 7646; print(paste("rewcfpftet", "is", 7646, sep = " "))}
tytmpiv <<- function() {tytmpiv <- 3270; print(paste("tytmpiv", "is", 3270, sep = " "))}
bgimfg <<- function() {bgimfg <- 8647; print(paste("bgimfg", "is", 8647, sep = " "))}
kwqhmr <<- function() {kwqhmr <- 7366; print(paste("kwqhmr", "is", 7366, sep = " "))}
lxn <<- function() {lxn <- 7872; print(paste("lxn", "is", 7872, sep = " "))}
rymtzcjtn <<- function() {rymtzcjtn <- 5820; print(paste("rymtzcjtn", "is", 5820, sep = " "))}
nkx <<- function() {nkx <- 3204; print(paste("nkx", "is", 3204, sep = " "))}
mewupi <<- function() {mewupi <- 8404; print(paste("mewupi", "is", 8404, sep = " "))}
jljbqv <<- function() {jljbqv <- 3957; print(paste("jljbqv", "is", 3957, sep = " "))}
rxagav <<- function() {rxagav <- 8650; print(paste("rxagav", "is", 8650, sep = " "))}
neiwj <<- function() {neiwj <- 6629; print(paste("neiwj", "is", 6629, sep = " "))}
vihkqc <<- function() {vihkqc <- 3495; print(paste("vihkqc", "is", 3495, sep = " "))}
aakydoh <<- function() {aakydoh <- 3528; print(paste("aakydoh", "is", 3528, sep = " "))}
yqqnnwwo <<- function() {yqqnnwwo <- 1478; print(paste("yqqnnwwo", "is", 1478, sep = " "))}
xdnxn <<- function() {xdnxn <- 2827; print(paste("xdnxn", "is", 2827, sep = " "))}
jpgsghwmqo <<- function() {jpgsghwmqo <- 3725; print(paste("jpgsghwmqo", "is", 3725, sep = " "))}
pvian <<- function() {pvian <- 842; print(paste("pvian", "is", 842, sep = " "))}
spxrsloiv <<- function() {spxrsloiv <- 6733; print(paste("spxrsloiv", "is", 6733, sep = " "))}
sebaag <<- function() {sebaag <- 6836; print(paste("sebaag", "is", 6836, sep = " "))}
zunmfhe <<- function() {zunmfhe <- 3123; print(paste("zunmfhe", "is", 3123, sep = " "))}
prde <<- function() {prde <- 7511; print(paste("prde", "is", 7511, sep = " "))}
mxqvzjslh <<- function() {mxqvzjslh <- 5935; print(paste("mxqvzjslh", "is", 5935, sep = " "))}
kpi <<- function() {kpi <- 5899; print(paste("kpi", "is", 5899, sep = " "))}
hwrqp <<- function() {hwrqp <- 5539; print(paste("hwrqp", "is", 5539, sep = " "))}
xsrhq <<- function() {xsrhq <- 8916; print(paste("xsrhq", "is", 8916, sep = " "))}
bwpimuutn <<- function() {bwpimuutn <- 4969; print(paste("bwpimuutn", "is", 4969, sep = " "))}
jcj <<- function() {jcj <- 601; print(paste("jcj", "is", 601, sep = " "))}
jzgkzko <<- function() {jzgkzko <- 3388; print(paste("jzgkzko", "is", 3388, sep = " "))}
altdtuidil <<- function() {altdtuidil <- 7105; print(paste("altdtuidil", "is", 7105, sep = " "))}
ztgnmyy <<- function() {ztgnmyy <- 5861; print(paste("ztgnmyy", "is", 5861, sep = " "))}
aswpalosyo <<- function() {aswpalosyo <- 7197; print(paste("aswpalosyo", "is", 7197, sep = " "))}
