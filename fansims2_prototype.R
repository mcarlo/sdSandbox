# fansims2_prototype

# Set directory and read in files
#
setup <- function(weekFilename){
#setupComplete = FALSE

  setwd("D:/WTP")

  ### set up
  weekFile <- read.csv(weekFilename, stringsAsFactors = F)
  weekFile <- weekFile[order(-weekFile$Confidence),]
  winProb <<- weekFile[, 2]
  if (max(winProb) > 1) {winProb <<- winProb/100.0}
  
  # simulate whether fans pick the favorite
  fanProb <- weekFile$FanProb
  
  # simulate favorite confidence and underdog confidence
  favConf <- weekFile$FavConf
  dogConf <- weekFile$DogConf
  
  games <- length(winProb)
  premium <- 16 - games
  prem <- FALSE
  
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

  save.image("fsims2.RData")
}

simulatePool <- function(maxIter = 2000, numFans = 90, 
                         payouts = c(100, 0, 0), totalPointsMatrix = totalPoints,
                         myPointsVector = myPoints, upsetPointsMatrix = upsetPoints){
  
  stratWins <- rep(0, 11)
  stratPlace <- rep(0, 11)
  stratShow <- rep(0, 11)
  
  temp <- rep(0, 11)
  tempPlace <- rep(0, 11)
  tempShow <- rep(0, 11)
  

  for (i in 1:maxIter){ # i = 121; numFans = 1
    #numFans = 2; i = 2
    set.seed(i)
    
    resultIndex <<- sample(1:2000, 1) # resultIndex = 9947
    fanIndex <<- sample(1:2000, numFans, replace = T)
    
    totalPointsIter <<- totalPointsMatrix[resultIndex, fanIndex]
    
    WTP <- 1*(myPointsVector[resultIndex] >= max(totalPointsIter))
    opp1Win <- 1*(upsetPointsMatrix[resultIndex, 1] >= max(totalPointsIter))
    opp2Win <- 1*(upsetPointsMatrix[resultIndex, 2] >= max(totalPointsIter))
    opp3Win <- 1*(upsetPointsMatrix[resultIndex, 3] >= max(totalPointsIter))
    opp4Win <- 1*(upsetPointsMatrix[resultIndex, 4] >= max(totalPointsIter))
    opp5Win <- 1*(upsetPointsMatrix[resultIndex, 5] >= max(totalPointsIter))
    
    opp6Win <- 1*(upsetPointsMatrix[resultIndex, 6] >= max(totalPointsIter))
    opp7Win <- 1*(upsetPointsMatrix[resultIndex, 7] >= max(totalPointsIter))
    opp8Win <- 1*(upsetPointsMatrix[resultIndex, 8] >= max(totalPointsIter))
    opp9Win <- 1*(upsetPointsMatrix[resultIndex, 9] >= max(totalPointsIter))
    opp10Win <- 1*(upsetPointsMatrix[resultIndex, 10] >= max(totalPointsIter))
    
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
    
    temp <- temp + c(WTP, opp1Win, opp2Win, opp3Win, opp4Win, opp5Win, opp6Win,
                     opp7Win, opp8Win, opp9Win, opp10Win)
    
    tempPlace <- tempPlace + c(WTPplace, opp1Place, opp2Place, opp3Place,
                               opp4Place, opp5Place, opp6Place, opp7Place,
                               opp8Place, opp9Place, opp10Place)
    
    tempShow <- tempShow + c(WTPShow, opp1Show, opp2Show, opp3Show, opp4Show,
                             opp5Show, opp6Show, opp7Show, opp8Show, opp9Show,
                             opp10Show)
    
    stratWins <- stratWins + temp
    stratPlace <- stratPlace + tempPlace
    stratShow <- stratShow + tempShow
    
    temp <- rep(0, 11)
    tempPlace <- temp
    tempShow <- temp
  }
  
  #save.image("fsims2results.RData")
  
  #displayResults
  
  #  load("fsims2results.RData")
  
  resultsMatrix <<- as.matrix(cbind(stratWins, stratPlace, stratShow), nrow = 6, ncol = 3) * 17.0 / maxIter
  winnings <<- as.data.frame(t((resultsMatrix %*% payouts)))
  colnames(winnings) <<- c("WTP", "Fav", "Fav-1", "Fav-2", "Fav-3", "Fav-4",
                          "Fav-5", "Fav-6", "Fav-7", "Fav-8", "Fav-9")
  rownames(resultsMatrix) <<- colnames(winnings)
  print(resultsMatrix)
  print(round(winnings, 2))
  print(apply(resultsMatrix, 1, sum))
  cat(paste0("maxIterations = ", maxIter))
  
}
###

setup("2014week11.csv")
simulatePool(numFans = 90, payouts = c(100, 50, 25))

setup("2014week12.csv")
simulatePool(numFans = 90, payouts = c(100, 50, 25))


setup("2014week13.csv")
simulatePool(numFans = 90, payouts = c(100, 50, 25))

setup("2014week14.csv")
simulatePool(numFans = 90, payouts = c(100, 50, 25))

setup("2014week15.csv")
simulatePool(numFans = 90, payouts = c(100, 50, 25))

setup("2014week16.csv")
simulatePool(numFans = 90, payouts = c(100, 50, 25))

