# fansims2_prototype

# Set directory and read in files
#
rm(list = ls())

set_up <- function(weekFileName){
  setwd("C:/Users/Anichini/Documents") # setwd("D:/WTP")
  weekFile <- read.csv(weekFileName, stringsAsFactors = F)
  weekFile <<- weekFile[order(-weekFile$Confidence),]
  winProb <<- weekFile[, 2]
  if (max(winProb) > 1) {winProb <<- winProb/100.0}

  games <<- length(winProb)
  premium <<- 16 - games
  prem <<- FALSE
}

define_payouts <- function(group){
  if (group == "Ted"){
    payouts <<- c(100, 75, 50)
    players <<- 97
  } else if (group == "Jack"){
    payouts <<- c(220, 125, 55)
    players <<- 162
    prem <<- TRUE
  } else {
    payouts <<- c(100, 0, 0)
    players <<- 82
  }
}

initialize_matrices <- function(){
  # simulate whether fans pick the favorite
  fanProb <<- weekFile$FanProb

  # simulate favorite confidence and underdog confidence
  favConf <<- weekFile$FavConf
  dogConf <<- weekFile$DogConf

  playerCols = 10000

  # simulate placeholder
  simRaw <<- matrix(rep(0,playerCols*games), nrow = games, ncol = playerCols)

  premiumPts <<- 0 + prem * premium

  oneupsetmatrix <<- matrix(rep(1, games**2), nrow = games, ncol = games)
  diag(oneupsetmatrix) <<- rep(0, games)

  oneupsetConfidence <<- oneupsetmatrix
  for (j in 1:games){
    for (i in 1:games){
      if (i < j) {
        oneupsetConfidence[i, j] <<- games + premiumPts - i
      } else {
        oneupsetConfidence[i, j] <<- games + premiumPts - i + 1
      }
    }
  }
  diag(oneupsetConfidence) <<- rep(games + premiumPts, games)

  set.seed(123) #as.numeric(Sys.time()))

  simplayerCols <- matrix(runif(games*playerCols), nrow = games, ncol = playerCols)

  simPicks <- matrix((simplayerCols < fanProb)*1, nrow = games, ncol = playerCols)
  # simPicks[1:10, 1:10]

  simRand <- matrix(runif(games*playerCols), nrow = games, ncol = playerCols)
  simRanks <- matrix(rep(0, games*playerCols), nrow = games, ncol = playerCols)
  # simRanks[1:10, 1:10]

  simFavs <- matrix(qbinom(simRand, games, (favConf - .5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
  # simFavs[1:10, 1:10]

  simDogs <- matrix(qbinom(simRand, games, (dogConf - .5)/games, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)
  simPrior <- matrix(qbinom(simRand, games, 0.5, lower.tail = T), nrow = games, ncol = playerCols) + (runif(playerCols * games) - .5)

  simRaw <- (simPrior + simFavs *simPicks + simDogs *(1 - simPicks))/2

  simRanks <<- apply(simRaw, 2, rank) + premiumPts # max(apply(simRanks, 2, max))

  simOutcomes <<- 1*(runif(games) <= winProb); # simOutcomes[5] <- 0
  myRanks <<- rank(winProb)+premiumPts

  myCorrect <<- simOutcomes * myRanks
  myPoints <<- sum(myCorrect)
  simCorrect <<- simOutcomes * simPicks + (1 - simOutcomes) * (1 - simPicks)

  totalPoints <<- apply(simCorrect * simRanks, 2, sum)
  # totalPoints[1:10]

  upsetPoints <<- apply(((simOutcomes * oneupsetmatrix) + (simOutcomes - 1) * (oneupsetmatrix - 1)) * oneupsetConfidence, 2, sum)
  # upsetPoints[1:10]

  }

runIterations <- function(maxIter, numFans = 100){

  stratWins <<- rep(0, 11)
  stratPlace <<- stratWins
  stratShow <<- stratWins

  temp <- rep(0, 11)
  tempPlace <- temp
  tempShow <- temp

  set.seed(123)

  for (i in 1:1){ # i = 1
    for (j in 1:1){ # j = 1
      fanIndex <- sample(1:10000, numFans, replace = T)

      totalPointsIter <- totalPoints[fanIndex]
      WTP <- 1*(myPoints >= max(totalPointsIter))
      opp1Win <- 1*(upsetPoints[1] > max(totalPointsIter))
      opp2Win <- 1*(upsetPoints[2] > max(totalPointsIter))
      opp3Win <- 1*(upsetPoints[3] > max(totalPointsIter))
      opp4Win <- 1*(upsetPoints[4] > max(totalPointsIter))
      opp5Win <- 1*(upsetPoints[5] > max(totalPointsIter))

      opp6Win <- 1*(upsetPoints[6] > max(totalPointsIter))
      opp7Win <- 1*(upsetPoints[7] > max(totalPointsIter))
      opp8Win <- 1*(upsetPoints[8] > max(totalPointsIter))
      opp9Win <- 1*(upsetPoints[9] > max(totalPointsIter))
      opp10Win <- 1*(upsetPoints[10] > max(totalPointsIter))

      WTPplace <- 1*(sum(myPoints < totalPointsIter) == 1)
      opp1Place <- 1*(sum(upsetPoints[1] < totalPointsIter) == 1)
      opp2Place <- 1*(sum(upsetPoints[2] < totalPointsIter) == 1)
      opp3Place <- 1*(sum(upsetPoints[3] < totalPointsIter) == 1)
      opp4Place <- 1*(sum(upsetPoints[4] < totalPointsIter) == 1)
      opp5Place <- 1*(sum(upsetPoints[5] < totalPointsIter) == 1)

      opp6Place <- 1*(sum(upsetPoints[6] < totalPointsIter) == 1)
      opp7Place <- 1*(sum(upsetPoints[7] < totalPointsIter) == 1)
      opp8Place <- 1*(sum(upsetPoints[8] < totalPointsIter) == 1)
      opp9Place <- 1*(sum(upsetPoints[9] < totalPointsIter) == 1)
      opp10Place <- 1*(sum(upsetPoints[10] < totalPointsIter) == 1)

      WTPShow <- 1*(sum(myPoints < totalPointsIter) == 2)
      opp1Show <- 1*(sum(upsetPoints[1] < totalPointsIter) == 2)
      opp2Show <- 1*(sum(upsetPoints[2] < totalPointsIter) == 2)
      opp3Show <- 1*(sum(upsetPoints[3] < totalPointsIter) == 2)
      opp4Show <- 1*(sum(upsetPoints[4] < totalPointsIter) == 2)
      opp5Show <- 1*(sum(upsetPoints[5] < totalPointsIter) == 2)

      opp6Show <- 1*(sum(upsetPoints[6] < totalPointsIter) == 2)
      opp7Show <- 1*(sum(upsetPoints[7] < totalPointsIter) == 2)
      opp8Show <- 1*(sum(upsetPoints[8] < totalPointsIter) == 2)
      opp9Show <- 1*(sum(upsetPoints[9] < totalPointsIter) == 2)
      opp10Show <- 1*(sum(upsetPoints[10] < totalPointsIter) == 2)

      temp <- temp + c(WTP, opp1Win, opp2Win, opp3Win, opp4Win, opp5Win, opp6Win, opp7Win, opp8Win, opp9Win, opp10Win)

      tempPlace <- tempPlace + c(WTPplace, opp1Place, opp2Place, opp3Place, opp4Place,
                     opp5Place, opp6Place, opp7Place, opp8Place, opp9Place,
                     opp10Place)

      tempShow <- tempShow + c(WTPShow, opp1Show, opp2Show, opp3Show, opp4Show, opp5Show,
                    opp6Show, opp7Show, opp8Show, opp9Show, opp10Show)

    }
    stratWins <<- stratWins + temp
    stratPlace <<- stratPlace + tempPlace
    stratShow <<- stratShow + tempShow

    temp <- rep(0, 11)
    tempPlace <- temp
    tempShow <- temp

  }
}

displayResults <- function(group, maxIter){
  resultsMatrix <- as.matrix(cbind(stratWins, stratPlace, stratShow), nrow = 6, ncol = 3)
  winnings <- as.data.frame(t((resultsMatrix %*% payouts))/(1.0 * maxIter * maxIter))
  colnames(winnings) <- c("WTP", "Fav", "Fav-1", "Fav-2", "Fav-3", "Fav-4",
                          "Fav-5", "Fav-6", "Fav-7", "Fav-8", "Fav-9")
  rownames(resultsMatrix) <- colnames(winnings)
  print(resultsMatrix)
  print(round(winnings, 2))
  print(apply(resultsMatrix, 1, sum))
}

simWeeklyPayouts <- function(weekFilename = "2014week11.csv", maxIter = 25, group = "Jack", numFans = 190){

  set_up(weekFilename)
  define_payouts(group)
  initialize_matrices()
  runIterations(maxIter, numFans = numFans)
  displayResults(group, maxIter)
}

system.time(simWeeklyPayouts())
system.time(simWeeklyPayouts(group = "Ted"))
system.time(simWeeklyPayouts(group = "Jack"))
