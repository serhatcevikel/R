# dimensions

omer01 <- function() {

    upperlimit <- 120 # row
    shufflecount <- 1000 # column

    x <- runif(568) # for excel vector 

    y <- replicate(upperlimit,0) # empty vector of 0's

    rep1 <- replicate(shufflecount, sample(x)) # shuffle vector with sample and replicate with shufflecount

    rep.var <- replicate(shufflecount,y) # 2nd matrix empty

    rep.rat1 <- rep.var # make a copy of y
    rep.rat2 <- rep.var # make a copy of y
    adj.rep.rat1 <- rep.var # make a copy of y
    adj.rep.rat2 <- rep.var # make a copy of y

    exp.rat1 <- y
    exp.rat2 <- y

    lx <- length(x)

    for(i in 1:shufflecount) # across columns
    {
        for(j in 1:upperlimit) # across rows
        {
            temp <- j:lx # sequence from
            temp2 <- rep(0, length(temp))
            
            for(k in temp)
            {
                temp1[k-j+1] <- sum(rep1[(k-j+1):k,i])
            }

          rep.var[j,i] <- var(temp2) 
        }
    }

    for(i in 1:shufflecount)
    {
    for(j in 1:upperlimit)
    {
      rep.rat1[,i][j]=rep.var[,i][j]/(j*rep.var[,i][1])
    }
    }

    for(i in 1:shufflecount)
    {
    for(j in 1:upperlimit)
    {
      rep.rat2[,i][j]=(12*rep.var[,i][j])/(j*rep.var[,i][12])
    }
    }

    for(i in 1:upperlimit)
    {
      temp=c(1:(i-1))
      for(j in 1:(i-1))
      {
       temp[j]=(j-i)/(i*(length(x)-j)) 
      }
      exp.rat1[i]=1+2*(sum(temp))
    }  

    for(i in 1:upperlimit)
    {
    temp=c(1:3)
    tempp=c(1:(i-1))
    temppp=c(1:11)
    temp[1]=(12+5*i)/(6*i)
    for(j in 1:(i-1))
    {
    tempp[j]=(length(x)-i)/(length(x)-j)
    }
    if(i==1)
    {temp[2]=0}
    else
    {temp[2]=(2/i)*(sum(tempp))}
    for(k in 1:11)
    {
    temppp[k]=(length(x)-12)/(length(x)-k)
    }
    temp[3]=-(1/6)*(sum(temppp))
    exp.rat2[i]=sum(temp)
    } 

    for(i in 1:shufflecount)
    {
    adj.rep.rat1[,i]=rep.rat1[,i]/exp.rat1
    }

    for(i in 1:shufflecount)
    {
    adj.rep.rat2[,i]=rep.rat2[,i]/exp.rat2
    }

    write.table(vr1, "C:/Users/omere/Desktop/Dersler/Tez/DATA/randomization/vr1.txt")
    write.table(vr2, "C:/Users/omere/Desktop/Dersler/Tez/DATA/randomization/vr2.txt")
    write.table(adj.vr1, "C:/Users/omere/Desktop/Dersler/Tez/DATA/randomization/adj.vr1.txt")
    write.table(adj.vr2, "C:/Users/omere/Desktop/Dersler/Tez/DATA/randomization/adj.vr2.txt")

}
