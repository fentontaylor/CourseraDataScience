###################
##  Forecasting  ##
###################

library(quantmod)

invisible(Sys.setlocale("LC_MESSAGES", "C"))
invisible(Sys.setlocale("LC_TIME", "C"))

from.dat <- as.Date("04/01/14", format="%m/%d/%y")
to.dat <- as.Date("10/31/16", format="%m/%d/%y")
getSymbols("GOOG", src="google", from = from.dat, to = to.dat)
head(GOOG)

mGOOG <- to.monthly(GOOG)
googOpen <- Op(mGOOG)
ts1 <- ts(googOpen, frequency=12)
plot(ts1, xlab="Years + 1", ylab="GOOG")
plot(decompose(ts1), xlab="Years + 1")

ts1Train <- window(ts1, start=1, end=5)
ts1Test <- window(ts1, start=5, end=(7-0.01))
