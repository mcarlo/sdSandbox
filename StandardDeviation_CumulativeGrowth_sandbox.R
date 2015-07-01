rm(list = ls())

formatBucks <- function(dollars){
  if (dollars < 1000){
    paste0("$ ", formatC(dollars, digits = 0, format = "f"))
  } else {
    paste0("$ ", formatC(as.integer(dollars/1000), big.mark = ",", digits = 0,
                         format = "d"), " K")
  }
}

logSigma2 <- function(sigma, mu){
  log(1+sigma^2/(1+mu)^2)
}

logMean <- function(sigma, mu){
  logS2 <- logSigma2(sigma, mu)
  log((1+mu)^2-logS2)/2
}

quantileColor <- cbind(c(2:6), c(2, 6, 1, 4, 3))

### reactive starts here

set.seed(123)
simZs <- matrix(rnorm((360)*101), nrow = (360), ncol = 101)

genPlot <- function(inputSD = 25, inputHorizon = 10, inputReturn = 6){
  wealthFactors <- matrix(rep(1, (12 * inputHorizon + 1)*101), nrow = (12 * inputHorizon + 1), ncol = 101)

  logMeanMonthly <- logMean(inputSD/100.0, inputReturn/100.0)/12
  logSigma2Monthly <- logSigma2(inputSD/100.0, inputReturn/100.0)/12

  for (i in 2:(12 * inputHorizon + 1)){
    for (j in 1:101){
      wealthFactors[i, j] <- exp(logMeanMonthly + (logSigma2Monthly)^0.5 * simZs[i - 1, j])

    }
  }

  cumWealthFactors <- matrix(rep(1.0, (12 * inputHorizon + 1) * 101), nrow = (12 * inputHorizon + 1), ncol = 101)

  for (j in 1:101){
    cumWealthFactors[, j] <- cumprod(wealthFactors[ ,j])*10000
  }

  findQuantiles <- function(year = 30){
    qcolumns <- which(cumWealthFactors[inputHorizon*12+1,] %in% quantile(cumWealthFactors[inputHorizon*12+1,], c(0.05, 0.33, 0.5, 0.67, 0.95)))
    outVecs <- cumWealthFactors[,qcolumns[order(cumWealthFactors[(12 * inputHorizon + 1),qcolumns])]]

    colnames(outVecs) <- c("Pct05", "Pct33", "Med", "Pct67", "Pct95")
    outVecs
  }

  years <- (0:(12*inputHorizon))/12 #tail(years)
  quantilePoints <- data.frame(cbind(years, findQuantiles()))

  minY <- min(apply(quantilePoints[, 2:6], 2, min))
  maxY <- max(apply(quantilePoints[, 2:6], 2, max))

  suppressWarnings(plot(years, log(quantilePoints[,2]), type = "n",
                        ylim = range(c(log(minY), log(maxY))),
                        xlab = "Years", ylab = "Cumulative Wealth (log scale)",
                        xlim = c(0, 40), yaxt = "n"))

  for (j in 1:101){
    lines(years, log(cumWealthFactors[,j]), lty = 3, col = 8)
  }

  for (j in 2:6){# j = 3
    lines(years, log(quantilePoints[,j]), lty = 1, col = quantileColor[quantileColor[,1] == j, 2], lwd = 2)
  }

  text(0, log(quantilePoints$Pct95[1]), paste0("\n\n",formatBucks(quantilePoints$Pct95[1])), cex = 1.5)
  text(34, log(quantilePoints$Pct95[(12 * inputHorizon + 1)]), paste0("95th %ile: ",formatBucks(quantilePoints$Pct95[(12 * inputHorizon + 1)])), cex = 1.5)
  text(34, log(quantilePoints$Pct67[(12 * inputHorizon + 1)]), paste0("67th %ile: ",formatBucks(quantilePoints$Pct67[(12 * inputHorizon + 1)])), cex = 1.5)
  text(34, log(quantilePoints$Med[(12 * inputHorizon + 1)]), paste0("50th %ile: ",formatBucks(quantilePoints$Med[(12 * inputHorizon + 1)])), cex = 1.5)
  text(34, log(quantilePoints$Pct33[(12 * inputHorizon + 1)]), paste0("33rd %ile: ",formatBucks(quantilePoints$Pct33[(12 * inputHorizon + 1)])), cex = 1.5)
  text(34, log(quantilePoints$Pct05[(12 * inputHorizon + 1)]), paste0("5th %ile: ",formatBucks(quantilePoints$Pct05[(12 * inputHorizon + 1)])), cex = 1.5)
}
