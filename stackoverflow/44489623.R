valid_times <- c(219.934, 229.996, 239.975, 249.935, 259.974)

actual_times <- c(200, 210, 215, 220.5, 260)
strain <- c("green", "green", "green", "green", "green")
valid_or_not <- c(rep(NA, 5))

valid_or_not[sapply(actual_times, function(x) any(abs(x - valid_times) <= 1))] <- "valid"

