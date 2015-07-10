# fansims2_prototype

# Set directory and read in files
#
rm(list = ls())
#setupComplete = FALSE

weekFilename = "2014week16.csv"
group = "Jack"

if (group == "Ted"){
  payouts <<- c(100, 75, 50)
} else if (group == "Jack"){
  payouts <<- c(220, 125, 55)
  prem <<- TRUE
} else {
  payouts <<- c(100, 0, 0)
}


### set up
weekFile <- read.csv(weekFilename, stringsAsFactors = F)
weekFile <- weekFile[order(-weekFile$Confidence),]
winProb <- weekFile[, 2]
if (max(winProb) > 1) {winProb <<- winProb/100.0}

# simulate whether fans pick the favorite
fanProb <- weekFile$FanProb

# simulate favorite confidence and underdog confidence
favConf <- weekFile$FavConf
dogConf <- weekFile$DogConf

games <- length(winProb)
premium <- 16 - games
prem <- FALSE

playerCols = 10000

# simulate placeholder
simRaw <- matrix(rep(0,playerCols*games), nrow = games, ncol = playerCols)

premiumPts <- 0 + prem * premium

oneupsetmatrix <- matrix(rep(1, games**2), nrow = games, ncol = games)
diag(oneupsetmatrix) <- rep(0, games)

oneupsetConfidence <- oneupsetmatrix
for (j in 1:games){
  for (i in 1:games){
    if (i < j) {
      oneupsetConfidence[i, j] <- games + premiumPts - i
    } else {
      oneupsetConfidence[i, j] <- games + premiumPts - i + 1
    }
  }
}
diag(oneupsetConfidence) <- rep(games + premiumPts, games)

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

#simOutcomes <- 1*(runif(games) <= winProb); # simOutcomes[5] <- 0
simOutcomes2 <- matrix(1*(runif(games * 10000) <= winProb), nrow = games, ncol = 10000)

myRanks <- rank(winProb)+premiumPts

myPoints <- t(myRanks) %*% simOutcomes2 # * myRanks


totalPoints <- t(t(simPicks * simRanks) %*% simOutcomes2 + t((1 - simPicks) * simRanks) %*% (1 - simOutcomes2))
rm(simPicks); rm(simRanks)

upsetPoints <- t(t(oneupsetmatrix * oneupsetConfidence) %*% simOutcomes2 + t((1 - oneupsetmatrix) * oneupsetConfidence) %*% (simOutcomes2 - 1))
# upsetPoints[1:10]

fanIndex <- sample(1:10000, 100, replace = T)

setupComplete = TRUE

# save.image("fsims2.RData")

# simulate

# load("fsims2.RData")
maxIter = 2000
numFans = 90

temp <- rep(0, 11)
tempPlace <- rep(0.0, 11)
tempShow <- rep(0.0, 11)

stratWins <- rep(0.0, 11)
stratPlace <- rep(0.0, 11)
stratShow <- rep(0.0, 11)
resultIndex <- 1


for (i in 1:maxIter){ # i = 121; numFans = 1
  #numFans = 2; i = 2
  set.seed(i)

  resultIndex <<- sample(1:10000, 1) # resultIndex = 9947
  fanIndex <<- sample(1:10000, numFans, replace = T)

  # max(upsetPoints[resultIndex, ]); simOutcomes2[, resultIndex]
  # fanIndex <- which(totalPoints[resultIndex, ] < max(upsetPoints[resultIndex, ]))[1:numFans]
  totalPointsIter <<- totalPoints[resultIndex, fanIndex]

  WTP <- 1*(myPoints[, resultIndex] >= max(totalPointsIter))
  opp1Win <- 1*(upsetPoints[resultIndex, 1] >= max(totalPointsIter))
  opp2Win <- 1*(upsetPoints[resultIndex, 2] >= max(totalPointsIter))
  opp3Win <- 1*(upsetPoints[resultIndex, 3] >= max(totalPointsIter))
  opp4Win <- 1*(upsetPoints[resultIndex, 4] >= max(totalPointsIter))
  opp5Win <- 1*(upsetPoints[resultIndex, 5] >= max(totalPointsIter))

  opp6Win <- 1*(upsetPoints[resultIndex, 6] >= max(totalPointsIter))
  opp7Win <- 1*(upsetPoints[resultIndex, 7] >= max(totalPointsIter))
  opp8Win <- 1*(upsetPoints[resultIndex, 8] >= max(totalPointsIter))
  opp9Win <- 1*(upsetPoints[resultIndex, 9] >= max(totalPointsIter))
  opp10Win <- 1*(upsetPoints[resultIndex, 10] >= max(totalPointsIter))

  WTPplace <- 1*(sum(myPoints[, resultIndex] < totalPointsIter) == 1)
  opp1Place <- 1*(sum(upsetPoints[resultIndex, 1] < totalPointsIter) == 1)
  opp2Place <- 1*(sum(upsetPoints[resultIndex, 2] < totalPointsIter) == 1)
  opp3Place <- 1*(sum(upsetPoints[resultIndex, 3] < totalPointsIter) == 1)
  opp4Place <- 1*(sum(upsetPoints[resultIndex, 4] < totalPointsIter) == 1)
  opp5Place <- 1*(sum(upsetPoints[resultIndex, 5] < totalPointsIter) == 1)

  opp6Place <- 1*(sum(upsetPoints[resultIndex, 6] < totalPointsIter) == 1)
  opp7Place <- 1*(sum(upsetPoints[resultIndex, 7] < totalPointsIter) == 1)
  opp8Place <- 1*(sum(upsetPoints[resultIndex, 8] < totalPointsIter) == 1)
  opp9Place <- 1*(sum(upsetPoints[resultIndex, 9] < totalPointsIter) == 1)
  opp10Place <- 1*(sum(upsetPoints[resultIndex, 10] < totalPointsIter) == 1)

  WTPShow <- 1*(sum(myPoints[, resultIndex] < totalPointsIter) == 2)
  opp1Show <- 1*(sum(upsetPoints[resultIndex, 1] < totalPointsIter) == 2)
  opp2Show <- 1*(sum(upsetPoints[resultIndex, 2] < totalPointsIter) == 2)
  opp3Show <- 1*(sum(upsetPoints[resultIndex, 3] < totalPointsIter) == 2)
  opp4Show <- 1*(sum(upsetPoints[resultIndex, 4] < totalPointsIter) == 2)
  opp5Show <- 1*(sum(upsetPoints[resultIndex, 5] < totalPointsIter) == 2)

  opp6Show <- 1*(sum(upsetPoints[resultIndex, 6] < totalPointsIter) == 2)
  opp7Show <- 1*(sum(upsetPoints[resultIndex, 7] < totalPointsIter) == 2)
  opp8Show <- 1*(sum(upsetPoints[resultIndex, 8] < totalPointsIter) == 2)
  opp9Show <- 1*(sum(upsetPoints[resultIndex, 9] < totalPointsIter) == 2)
  opp10Show <- 1*(sum(upsetPoints[resultIndex, 10] < totalPointsIter) == 2)

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
resultsMatrix <- as.matrix(cbind(stratWins, stratPlace, stratShow), nrow = 6, ncol = 3) * 17.0 / maxIter
winnings <- as.data.frame(t((resultsMatrix %*% payouts)))
colnames(winnings) <- c("WTP", "Fav", "Fav-1", "Fav-2", "Fav-3", "Fav-4",
                        "Fav-5", "Fav-6", "Fav-7", "Fav-8", "Fav-9")
rownames(resultsMatrix) <- colnames(winnings)
resultsMatrix
round(winnings, 2)
apply(resultsMatrix, 1, sum)


# system.time(simWeeklyPayouts(weekFilename = "2014week10.csv", numFans = 3))
# system.time(simWeeklyPayouts(weekFilename = "2014week11.csv", numFans = 2))
# system.time(simWeeklyPayouts(weekFilename = "2014week12.csv", numFans = 2))
# system.time(simWeeklyPayouts(weekFilename = "2014week14.csv", numFans = 2))
# system.time(simWeeklyPayouts(weekFilename = "2014week15.csv", numFans = 2))
# system.time(simWeeklyPayouts(weekFilename = "2014week16.csv", numFans = 2))
# system.time(simWeeklyPayouts(weekFilename = "2014week17.csv", numFans = 2))
#
# system.time(simWeeklyPayouts(weekFilename = "2014week11.csv", group = "Ted", numFans = 100))
# system.time(simWeeklyPayouts(weekFilename = "2014week11.csv", group = "Jack", numFans = 190))
