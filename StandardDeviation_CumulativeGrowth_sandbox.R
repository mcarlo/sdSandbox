library(shiny)

set.seed(123)
simZs <- matrix(rnorm(360*101), nrow = 360, ncol = 101)

arithMu = .06
arithSigma = .20

logSigma2 <- function(arithSigma = arithSigma, arithMu = arithMu){
  log(1+arithSigma^2/(1+arithMu)^2)
}

logMean <- function(arithSigma = arithSigma, arithMu = arithMu){
  logS2 <- logSigma2(arithSigma, arithMu)
  log((1+arithMu)^2-logS2)/2
}

### reactive starts here

wealthFactors <- matrix(rep(1, 361*101), nrow = 361, ncol = 101)

wealth <- matrix(rep(0, 361*101), nrow = 361, ncol = 101)
wealth[1, ] <- rep(1, 101)

wealth[1:5, 1:5]

logMeanMonthly <- logMean(arithSigma, arithMu)/12
logSigma2Monthly <- logSigma2(arithSigma, arithMu)/12

for (i in 2:361){
  for (j in 1:101){
    wealthFactors[i, j] <- exp(logMeanMonthly + (logSigma2Monthly)^0.5 * simZs[i - 1, j])

  }
}

wealthFactors[361, 1:10]
hist(apply(wealthFactors, 2, sd))

cumWealthFactors <- matrix(rep(1.0, 361 * 101), nrow = 361, ncol = 101)

for (j in 1:101){
  cumWealthFactors[, j] <- cumprod(wealthFactors[ ,j])
}

years <- (0:360)/12

pctiles <- apply(cumWealthFactors, 1, quantile, c(.05, .25, .5, .75, .95))

pctiles2 <- which(cumWealthFactors[361, ] %in% quantile(cumWealthFactors[361, ], 
                                                        c(.05, .25, .5, .75, .95)))


plot(years, cumWealthFactors[, 1], log = "y", ylim = c(0.25, 64), type = "n",
     xlab = "Years", ylab = "Cumulative Wealth")

for (j in 1:101){
  lines(years, cumWealthFactors[, j], col = 16)
}

for (i in 1:5){
  lines(years, t(cumWealthFactors[, pctiles2[i]]), col = "blue")
}

# let the user choose the year for selecting the percentile threads