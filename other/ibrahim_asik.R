
savings <- function(initial_usd = 3.5, monthss = 20, growth = 0.03)
{
    dollar_vec <- rep(NA, monthss)
    val_usd <- initial_usd

    for (i in 1:monthss)
    {
        val_usd <- val_usd * (1 + growth)
        dollar_vec[i] <- val_usd
    }

    return(dollar_vec)
}

dollar_vec2 <- 1.03^(1:20) * 3.5

