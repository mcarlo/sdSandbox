# fansims2_prototype

# Set directory and read in files
#
setup <- function(weekFilename){
#setupComplete = FALSE

  #setwd("D:/WTP")

  ### set up weekFilename = "2014week17.csv"
  weekFile <- read.csv(weekFilename, stringsAsFactors = F)
  weekFile <<- weekFile[order(-weekFile$Confidence),]
  winProb <<- weekFile[, 2]
  if (max(winProb) > 1) {winProb <<- winProb/100.0}

  favorites <<- weekFile$Victor

  # simulate whether fans pick the favorite
  fanProb <- weekFile$FanProb

  # simulate favorite confidence and underdog confidence
  favConf <- weekFile$FavConf
  dogConf <- weekFile$DogConf

  games <<- length(winProb)
  premium <- 16 - games
  prem <- FALSE

  oppLabel <- function(c){paste0(c, "'s opponent")}
  dogs <<- sapply(favorites, oppLabel)
  if(dim(weekFile)[2] == 8) {dogs <<- weekFile$Underdog}

  playerCols = 2000

  # simulate placeholder
  simRaw <- matrix(rep(0,playerCols*games), nrow = games, ncol = playerCols)

  premiumPts <- 0 + prem * premium

  upsetDiagMatrix <- matrix(rep(0, games * games), nrow = games,
                            ncol = games)
  diag(upsetDiagMatrix) <- rep(games + premiumPts, games)

  upsetMatrix <- matrix(rep((games:1) + premiumPts, games, times = games),
                        nrow = games, ncol = games)

  diag(upsetMatrix) <- rep(0, games)

  for (j in 2:games){
    upsetMatrix[1:(j-1), j] <- (games + premiumPts - 1):
      (games + premiumPts - j + 1)
  }

  strategies <<- matrix(rep(favorites, 14), ncol = 14)
  for (j in 2:14){ # j = 2
    strategies[j - 1, j] <- dogs[j-1] #weekFile[1:3, ]; favorites[j]
    strategies[, j] <<- strategies[order(-(upsetMatrix + upsetDiagMatrix)[, j - 1]), j]
  }

  set.seed(123) #as.numeric(Sys.time()))

  simplayerCols <- matrix(runif(games*playerCols), nrow = games, ncol = playerCols)

  simPicks <- matrix((simplayerCols < fanProb)*1, nrow = games, ncol = playerCols)
  # simPicks[1:10, 1:10]

  simRand <- matrix(runif(games*playerCols), nrow = games, ncol = playerCols)
  # simRanks <- matrix(rep(0, games*playerCols), nrow = games, ncol = playerCols)
  # simRanks[1:10, 1:10]

  simFavs <- matrix(qbinom(simRand, games, (favConf - .5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
  # simFavs[1:10, 1:10]

  simDogs <- matrix(qbinom(simRand, games, (dogConf - .5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
  simPrior <- matrix(qbinom(simRand, games, 0.5, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
  rm(simplayerCols); rm(simRand)

  simRaw <- (simPrior + simFavs *simPicks + simDogs *(1 - simPicks))/2

  simRanks <- apply(simRaw, 2, rank) + premiumPts # max(apply(simRanks, 2, max))
  rm(simRaw); rm(simPrior); rm(simFavs); rm(simDogs)

  simOutcomes2 <- matrix(1*(runif(games * 2000) <= winProb), nrow = games, ncol = 2000)

  myRanks <- rank(winProb)+premiumPts

  myPoints <<- as.vector(t(myRanks) %*% simOutcomes2) # * myRanks


  totalPoints <<- t(t(simPicks * simRanks) %*% simOutcomes2 + t((1 - simPicks) * simRanks) %*% (1 - simOutcomes2))

  upsetPoints <<- t(t(upsetMatrix) %*% simOutcomes2 + t(upsetDiagMatrix) %*% (1 - simOutcomes2))

  fanIndex <- sample(1:2000, 100, replace = T)

  setupComplete = TRUE

}

simulatePool <- function(maxIter = 2000, numFans = 90,
                         payouts = c(100, 0, 0), totalPointsMatrix = totalPoints,
                         myPointsVector = myPoints, upsetPointsMatrix = upsetPoints){

  stratWins <- rep(0, 14)
  stratPlace <- rep(0, 14)
  stratShow <- rep(0, 14)

  temp <- rep(0, 14)
  tempPlace <- rep(0, 14)
  tempShow <- rep(0, 14)

  set.seed(as.numeric(Sys.time()))

  for (i in 1:maxIter){ # i = 1; numFans = 1
    #numFans = 90; i = 2

    resultIndex <<- sample(1:2000, 1) # resultIndex = 9947
    fanIndex <<- sample(1:2000, numFans, replace = T)

    #totalPointsMatrix <- totalPoints
    totalPointsIter <<- totalPointsMatrix[resultIndex, fanIndex]

    #myPointsVector <- myPoints; upsetPointsMatrix <- upsetPoints
    WTP <- 1*(myPointsVector[resultIndex] > max(totalPointsIter))
    opp1Win <- 1*(upsetPointsMatrix[resultIndex, 1] > max(totalPointsIter))
    opp2Win <- 1*(upsetPointsMatrix[resultIndex, 2] > max(totalPointsIter))
    opp3Win <- 1*(upsetPointsMatrix[resultIndex, 3] > max(totalPointsIter))
    opp4Win <- 1*(upsetPointsMatrix[resultIndex, 4] > max(totalPointsIter))
    opp5Win <- 1*(upsetPointsMatrix[resultIndex, 5] > max(totalPointsIter))

    opp6Win <- 1*(upsetPointsMatrix[resultIndex, 6] > max(totalPointsIter))
    opp7Win <- 1*(upsetPointsMatrix[resultIndex, 7] > max(totalPointsIter))
    opp8Win <- 1*(upsetPointsMatrix[resultIndex, 8] > max(totalPointsIter))
    opp9Win <- 1*(upsetPointsMatrix[resultIndex, 9] > max(totalPointsIter))
    opp10Win <- 1*(upsetPointsMatrix[resultIndex, 10] > max(totalPointsIter))

    opp11Win <- 1*(upsetPointsMatrix[resultIndex, 11] > max(totalPointsIter))
    opp12Win <- 1*(upsetPointsMatrix[resultIndex, 12] > max(totalPointsIter))
    opp13Win <- 1*(upsetPointsMatrix[resultIndex, 13] > max(totalPointsIter))

    WTPplace <- 1*(sum(myPointsVector[resultIndex] < totalPointsIter) == 1)
    opp1Place <- 1*(sum(upsetPointsMatrix[resultIndex, 1] < totalPointsIter) == 1)
    opp2Place <- 1*(sum(upsetPointsMatrix[resultIndex, 2] < totalPointsIter) == 1)
    opp3Place <- 1*(sum(upsetPointsMatrix[resultIndex, 3] < totalPointsIter) == 1)
    opp4Place <- 1*(sum(upsetPointsMatrix[resultIndex, 4] < totalPointsIter) == 1)
    opp5Place <- 1*(sum(upsetPointsMatrix[resultIndex, 5] < totalPointsIter) == 1)

    opp6Place <- 1*(sum(upsetPointsMatrix[resultIndex, 6] < totalPointsIter) == 1)
    opp7Place <- 1*(sum(upsetPointsMatrix[resultIndex, 7] < totalPointsIter) == 1)
    opp8Place <- 1*(sum(upsetPointsMatrix[resultIndex, 8] < totalPointsIter) == 1)
    opp9Place <- 1*(sum(upsetPointsMatrix[resultIndex, 9] < totalPointsIter) == 1)
    opp10Place <- 1*(sum(upsetPointsMatrix[resultIndex, 10] < totalPointsIter) == 1)

    opp11Place <- 1*(sum(upsetPointsMatrix[resultIndex, 11] < totalPointsIter) == 1)
    opp12Place <- 1*(sum(upsetPointsMatrix[resultIndex, 12] < totalPointsIter) == 1)
    opp13Place <- 1*(sum(upsetPointsMatrix[resultIndex, 13] < totalPointsIter) == 1)

    WTPShow <- 1*(sum(myPointsVector[resultIndex] < totalPointsIter) == 2)
    opp1Show <- 1*(sum(upsetPointsMatrix[resultIndex, 1] < totalPointsIter) == 2)
    opp2Show <- 1*(sum(upsetPointsMatrix[resultIndex, 2] < totalPointsIter) == 2)
    opp3Show <- 1*(sum(upsetPointsMatrix[resultIndex, 3] < totalPointsIter) == 2)
    opp4Show <- 1*(sum(upsetPointsMatrix[resultIndex, 4] < totalPointsIter) == 2)
    opp5Show <- 1*(sum(upsetPointsMatrix[resultIndex, 5] < totalPointsIter) == 2)

    opp6Show <- 1*(sum(upsetPointsMatrix[resultIndex, 6] < totalPointsIter) == 2)
    opp7Show <- 1*(sum(upsetPointsMatrix[resultIndex, 7] < totalPointsIter) == 2)
    opp8Show <- 1*(sum(upsetPointsMatrix[resultIndex, 8] < totalPointsIter) == 2)
    opp9Show <- 1*(sum(upsetPointsMatrix[resultIndex, 9] < totalPointsIter) == 2)
    opp10Show <- 1*(sum(upsetPointsMatrix[resultIndex, 10] < totalPointsIter) == 2)

    opp11Show <- 1*(sum(upsetPointsMatrix[resultIndex, 11] < totalPointsIter) == 2)
    opp12Show <- 1*(sum(upsetPointsMatrix[resultIndex, 12] < totalPointsIter) == 2)
    opp13Show <- 1*(sum(upsetPointsMatrix[resultIndex, 13] < totalPointsIter) == 2)

#    myPointsVector = myPoints; upsetPointsMatrix = upsetPoints
    temp <- temp + c(WTP, opp1Win, opp2Win, opp3Win, opp4Win, opp5Win, opp6Win,
                     opp7Win, opp8Win, opp9Win, opp10Win, opp11Win, opp12Win, opp13Win)

    tempPlace <- tempPlace + c(WTPplace, opp1Place, opp2Place, opp3Place,
                               opp4Place, opp5Place, opp6Place, opp7Place,
                               opp8Place, opp9Place, opp10Place, opp11Place,
                               opp12Place, opp13Place)

    tempShow <- tempShow + c(WTPShow, opp1Show, opp2Show, opp3Show, opp4Show,
                             opp5Show, opp6Show, opp7Show, opp8Show, opp9Show,
                             opp10Show, opp11Show, opp12Show, opp13Show)

    stratWins <- stratWins + temp
    stratPlace <- stratPlace + tempPlace
    stratShow <- stratShow + tempShow

    temp <- rep(0, 14)
    tempPlace <- temp
    tempShow <- temp
  }
  #rm(list = ls())
  #save.image("fsims2results.RData")

  #displayResults

  #  load("fsims2results.RData")

  resultsMatrix <<- as.matrix(cbind(stratWins, stratPlace, stratShow), nrow = 6, ncol = 3) * 17.0 / maxIter
  winnings <<- round(as.data.frame(t((resultsMatrix %*% payouts))), 1)
  inTheMoney <<- round(apply(resultsMatrix %*% (1*(payouts > 0)), 1, sum), 2)

  colnames(winnings) <<- c("WTP", "Fav", "Fav-1", "Fav-2", "Fav-3", "Fav-4",
                          "Fav-5", "Fav-6", "Fav-7", "Fav-8", "Fav-9",
                          "Fav-10", "Fav-11", "Fav-12")
  rownames(resultsMatrix) <<- colnames(winnings)
  #print(resultsMatrix)
  print(rbind(round(winnings, 2), round(apply(resultsMatrix, 1, sum), 1)))
  cat(paste0("maxIterations = ", maxIter))

}

top3Money <- function(){
  inTheMoney[which(rank(inTheMoney) == 14) ]
  inTheMoney[which(rank(inTheMoney) == 13) ]
  inTheMoney[which(rank(inTheMoney) == 12) ]
}

top3Dollars <- function(){
  winnings[which(rank(winnings) == 14)]
  winnings[which(rank(winnings) == 13)]
  winnings[which(rank(winnings) == 12)]
}



save.image("fsims2.RData")

###
# rm(list = ls())
load("fsims2.RData")

setup("2014week11.csv") # 2000 sufficient
setup("2014week12.csv") # 2000 sufficient
setup("2014week13.csv") # 2000 sufficient
setup("2014week14.csv") # 2000 sufficient
setup("2014week15.csv") # 2000 sufficient
setup("2014week16.csv") # 20000 sufficient
setup("2014week17.csv") # 20000 sufficient

system.time(simulatePool(maxIter = 2000, numFans = 10, payouts = c(100, 0, 0)))

topWin <- which(winnings == max(winnings))
topMoney <- which(inTheMoney == max(inTheMoney))
strategies[, topWin]
strategies[, topMoney]
