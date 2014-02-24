loadData <- function(x, source=c('steamer14', 'zips14', 'oliver14')) {
  src <- match.arg(source)
  res <- list(batting=list(main=NULL, pt=NULL, extra=NULL), pitching=list(main=NULL, pt=NULL, extra=NULL))
  # load defaults
  if(src == 'steamer14') {
    data(Steamer2014)
    data(PlayingTime2014)
    res$batting$main <- BattingSteamer2014
    res$pitching$main <- PitchingSteamer2014
    res$batting$pt <- BattingPlayingTime2014
    res$pitching$pt <- PitchingPlayingTime2014
  } else if(src == 'zips14') {
    data(Zips2014)
    data(PlayingTime2014)
    res$batting$main <- BattingZips2014
    res$pitching$main <- PitchingZips2014
    res$batting$pt <- BattingPlayingTime2014
    res$pitching$pt <- PitchingPlayingTime2014
  } else if(src == 'oliver14') {
    data(Oliver2014)
    data(PlayingTime2014)
    res$batting$main <- BattingOliver2014
    res$pitching$main <- PitchingOliver2014
    res$batting$pt <- BattingPlayingTime2014
    res$pitching$pt <- PitchingPlayingTime2014
  }
  if(is.list(x)) {
    if('batting' %in% names(x)) {
      if('main' %in% names(x$batting)) res$batting$main <- x$batting$main
      if('pt' %in% names(x$batting)) res$batting$main <- x$batting$pt
      if('extra' %in% names(x$batting)) res$batting$main <- x$batting$extra
    }
    if('pitching' %in% names(x)) {
      if('main' %in% names(x$pitching)) res$pitching$main <- x$pitching$main
      if('pt' %in% names(x$pitching)) res$pitching$main <- x$pitching$pt
      if('extra' %in% names(x$pitching)) res$pitching$main <- x$pitching$extra
    }
  }
  res
}

loadPlayerIds <- function(mlbamID, file=NULL) {
  if(is.null(file)) file <- system.file("extdata", "IdConversion.csv", package="lastplayerpicked")
  ids <- read.csv(file, stringsAsFactors=FALSE)
  ids[match(mlbamID, ids$mlbamID), 4:6]
}

createQualityStartsProjections <- function(x) {
  qs <- rep(NA, nrow(x))
  if(all(c('IP', 'ER') %in% names(x))) {
    qs <- ifelse(x$IP > 120, round(6.35 + 0.095 * x$IP - 1.6 * x$ER / x$IP * 9), 0)
  }
  qs
}

adjustPT <- function(players, league, playerType=c('Hitters', 'Pitchers')) {
  players[,'adjustPercent'] <- 0
  type <- match.arg(playerType)
#   pt <- loadData(league, type, 'pt')
  pt <- switch(playerType, Hitters = league@info$data$batting$pt, Pitchers = league@info$data$pitching$pt)
  if(is.null(pt)) return(players)
  playingTime <- pt[match(players$mlbamID, pt[,1]),]
  # for hitters, PA should be second column
  # for pitchers, IP should be second column, saves fourth column
  if(type == 'Hitters') {
    denom <- rowSums(players[,intersect(c('AB','BB','SH','HBP'), names(players))])
    players$adjustPercent <- ifelse(!is.na(playingTime[,2]) & denom > 0, playingTime[,2] / denom, 0)
    cats <- intersect(c("AB", "H", "2B", "3B", "HR", "R", "RBI", "SB", "CS", "BB", "SO", "SH", "SF", "HBP"), names(players))
    # force an adjustment ceiling
    players[players$adjustPercent > 20, 'adjustPercent'] <- 20
    players[,cats] <- round(players[,cats] * players$adjustPercent)
  } else {
    denom <- players$IP
    players$adjustPercent <- ifelse(!is.na(playingTime[,2]) & denom > 0, playingTime[,2] / denom, 0)
    cats <- intersect(c('IP','W','L','G','H','ER','HR','BB','K'), names(players))
    # force an adjustment ceiling
    players[players$adjustPercent > 20, 'adjustPercent'] <- 20
    players[,cats] <- round(players[,cats] * players$adjustPercent)
    saves <- playingTime[,4]
    players$S <- ifelse(is.na(saves), 0, saves)
    players$qs <- createQualityStartsProjections(players)
  }
  players[players$adjustPercent > 0,]
}

buildRateStat <- function(n, d) {
  ifelse(d > 0, n/d, NaN)
}

getAverageOfDraftedPlayers <- function(n, d, players, nPlayers) {
  ix <- seq(nPlayers)
  numer <- sum(players[ix,n])
  denom <- sum(players[ix,d])
  if(denom == 0) return(0)
  numer/denom
}

getAverageOPSOfDraftedPlayers <- function(players, nPlayers) {
  ix <- seq(nPlayers)
  n1 <- sum(players[ix,c('H','BB','HBP')])
  d1 <- sum(players[ix,'PA'])
  n2 <- sum(players[ix,'TB'])
  d2 <- sum(players[ix,'AB'])
  if(d2 == 0) return(0)
  n1/d1 + n2/d2
}

buildXStat <- function(n, d, players, nPlayers, smallIsGood=FALSE) {
  overallAverage <- getAverageOfDraftedPlayers(n, d, players, nPlayers)
  signConstant <- 1
  if(smallIsGood) signConstant <- -1
  stat <- numeric(nrow(players))
  for(i in seq_along(stat)) {
    stat[i] <- sum(players[i,n]) - (sum(players[i,d]) * overallAverage)
  }
  list(stat * signConstant, overallAverage)
}

buildXOPS <- function(players, nPlayers) {
  overallAverage <- getAverageOPSOfDraftedPlayers(players, nPlayers)
  stat <- (players[,'OPS'] - overallAverage) * players[,'PA']
  list(stat, overallAverage)
}

buildAveStandardDeviations <- function(players, nPlayers, categories) {
  ix <- seq(nPlayers)
  sd <- apply(players[ix, categories], MARGIN=2, sd)
  mean <- apply(players[ix, categories], MARGIN=2, mean)
  list(sd=sd, ave=mean)
}

isNegativeCategory <- function(category, ip=FALSE) {
  category %in% c('E','GIDP','SO','L') || (category %in% c('H','BB','HR') && ip)
}

buildStandardScores <- function(players, categories, stdave) {
  mvals <- t(apply(players[,categories], MARGIN=1, FUN=function(i) (i - stdave[[2]]) / stdave[[1]]))
  nostd <- which(stdave[[1]] == 0)
  mvals[,nostd] <- 0
  negate <- which(sapply(categories, isNegativeCategory, 'IP' %in% names(players)))
  mvals[,negate] <- mvals[,negate] * -1
  colnames(mvals) <- paste0('m', colnames(mvals))
  cbind(mvals, total=rowSums(mvals))
}

posMap <- function(position) {
  pos <- position
  c(position, switch(position,
    CI = c('1B','3B'),
    MI = c('2B','SS'),
    IF = c('1B','2B','3B','SS','CI','MI'),
    OF = c('LF','CF','RF'),
    Util = c('C','1B','2B','3B','SS','CI','MI','LF','CF','RF','OF'),
    P = c('SP','RP'),
    c()
  ))
}

posMatch <- function(position='') {
  if(is.numeric(position)) return(NA)
  switch(position,
    C = c('C','Util'),
    '1B' = c('1B','CI','IF','Util'),
    '2B' = c('2B','MI','IF','Util'),
    '3B' = c('3B','CI','IF','Util'),
    SS = c('SS','MI','IF','Util'),
    LF = c('LF','OF','Util'),
    CF = c('CF','OF','Util'),
    RF = c('RF','OF','Util'),
    OF = c('OF','Util'),
    Util = c('Util'),
    SP = c('SP','P'),
    RP = c('RP','P'),
    P = c('P'),
    NA
  )
}

playerMatchesPos <- function(player, position) {
  quals <- lapply(strsplit(player$pos, '[|]'), FUN=function(i) {
    unlist(lapply(i, posMatch))
  })
  sapply(quals, FUN=function(i) { position %in% i })
}

buildReplacementLevels <- function(players, positions) {
  pos <- names(which(positions > 0))
  isAboveReplacement <- logical(nrow(players))
#   replacementLevels <- numeric(length(pos))
#   names(replacementLevels) <- pos
  for(curpos in pos) {
    numPlayersAtPosition <- positions[curpos]
    matched <- which(playerMatchesPos(players, curpos) & !isAboveReplacement)
    isAboveReplacement[matched[seq(numPlayersAtPosition)]] <- TRUE
#     replacementLevels[curpos] <- players[matched[numPlayersAtPosition],'total']
  }
  tapply(players[isAboveReplacement,'total'], players[isAboveReplacement,'pos'], min)
}

adjustForReplacementLevel <- function(players, positions) {
  replacementLevels <- buildReplacementLevels(players, positions)
  replacementLevels[match(players[,'pos'], names(replacementLevels))]
}

buildTotalValue <- function(players, numberOfPlayersDrafted) {
  sum(players[seq(numberOfPlayersDrafted), 'adjTotal'])
}

mergeFromX <- function(ids, x) {
  ix <- match(ids, x[,1])
  x[ix,-1,drop=FALSE]
}

buildPositions <- function() {
  h <- c('C','1B','2B','3B','SS','LF','CF','RF','OF')
  p <- c('SP','RP')
  file <- system.file("extdata", "PlayingTime2013.csv", package="lastplayerpicked")
  dat <- read.csv(file, stringsAsFactors=FALSE, check.names=FALSE)
  dat[dat$Position == 'Catcher','Position'] <- 'C'
  pitPos <- dat$Position %in% c('SP','RP')
  pitID <- unique(dat[pitPos,'MLBAMID'])
  hitID <- unique(setdiff(dat$MLBAMID, pitID))
  nHit <- length(hitID)
  hitters <- data.frame(matrix(0, nrow=nHit, ncol=length(h)+2))
  names(hitters) <- c('mlbamID', 'playerName', sprintf("G_%s", h))
  for(i in seq(nHit)) {
    row <- dat[dat$MLBAMID == hitID[i] & dat$Position %in% h,]
    hitters[i,1] <- hitID[i]
    hitters[i,2] <- dat[match(hitID[i], dat$MLBAMID), 1]
    hitters[i,sprintf("G_%s", row$Position)] <- row$Games
  }
  nPit <- length(pitID)
  pitchers <- data.frame(matrix(0, nrow=nPit, ncol=length(p)+2))
  names(pitchers) <- c('mlbamID', 'playerName', sprintf("G_%s", p))
  for(i in seq(nPit)) {
    row <- dat[dat$MLBAMID == pitID[i] & dat$Position %in% p,]
    pitchers[i,1] <- pitID[i]
    pitchers[i,2] <- dat[match(pitID[i], dat$MLBAMID), 1]
    pitchers[i,sprintf("G_%s", row$Position)] <- row$Games
  }
  list(hitters, pitchers)
}

grabIdFromName <- function(x) {
  file <- system.file("extdata", "playerid_list.csv", package="lastplayerpicked")
  dat <- read.csv(file, stringsAsFactors=FALSE, check.names=FALSE)
  posData <- buildPositions()
  hitPos <- posData[[1]]
  pitPos <- posData[[2]]
  res <- rep(NA, length(x))
  for(i in seq_along(res)) {
    curname <- x[i]
    hit.ix <- match(curname, hitPos$playerName)
    pit.ix <- match(curname, pitPos$playerName)
    if(!is.na(hit.ix)) {
      res[i] <- hitPos[hit.ix, 'mlbamID']
      hitPos[hit.ix, 'mlbamID'] <- NA
    } else if(!is.na(pit.ix)) {
      res[i] <- pitPos[pit.ix, 'mlbamID']
      pitPos[pit.ix, 'mlbamID'] <- NA
    }
  }
  if(any(is.na(res))) {
    dat <- dat[dat$MLBCODE != 'NULL',]
    name <- apply(dat[,2:1], MARGIN=1, paste, collapse=' ')
    res[is.na(res)] <- dat[match(x[is.na(res)], name), 'MLBCODE']
  }
#   name <- apply(dat[,2:1], MARGIN=1, paste, collapse=' ')
#   res <- dat[match(x, name), 'MLBCODE']
#   bad.ix <- which(res == 'NULL')
#   if(length(bad.ix)) {
#     for(i in bad.ix) {
#       curname <- x[i]
#       use.ix <- which(name == curname & dat$MLBCODE != 'NULL')[1]
#       if(!is.na(use.ix)) {
#         res[i] <- dat[use.ix,'MLBCODE']
#       }
#     }
#   }
  res
}

fix2014Data <- function() {
  # load position information
  posData <- buildPositions()
  hitPos <- posData[[1]]
  pitPos <- posData[[2]]

  file <- system.file("extdata", "2014BattingOliver.csv", package="lastplayerpicked")
  a <- read.csv(file, stringsAsFactors=FALSE, check.names=FALSE)
  a$mlbamID <- grabIdFromName(a$Name)
  a <- a[,c(24,1:23)]
  names(a)[match('Name', names(a))] <- 'playerName'
  BattingOliver2014 <- cbind(a, hitPos[match(a$mlbamID, hitPos$mlbamID),-c(1:2)])

  file <- system.file("extdata", "2014PitchingOliver.csv", package="lastplayerpicked")
  a <- read.csv(file, stringsAsFactors=FALSE, check.names=FALSE)
  a$mlbamID <- grabIdFromName(a$Name)
  a <- a[,c(17,1:16)]
  names(a)[match('Name', names(a))] <- 'playerName'
  names(a)[match('SO', names(a))] <- 'K'
  PitchingOliver2014 <- cbind(a, pitPos[match(a$mlbamID, pitPos$mlbamID),-c(1:2)])

  file <- system.file("extdata", "2014BattingSteamer.csv", package="lastplayerpicked")
  a <- read.csv(file, stringsAsFactors=FALSE, check.names=FALSE)
  a$mlbamID <- grabIdFromName(a$Name)
  a <- a[,c(24,1:23)]
  names(a)[match('Name', names(a))] <- 'playerName'
  BattingSteamer2014 <- cbind(a, hitPos[match(a$mlbamID, hitPos$mlbamID),-c(1:2)])

  file <- system.file("extdata", "2014PitchingSteamer.csv", package="lastplayerpicked")
  a <- read.csv(file, stringsAsFactors=FALSE, check.names=FALSE)
  a$mlbamID <- grabIdFromName(a$Name)
  a <- a[,c(20,1:19)]
  names(a)[match('Name', names(a))] <- 'playerName'
  names(a)[match('SO', names(a))] <- 'K'
  PitchingSteamer2014 <- cbind(a, pitPos[match(a$mlbamID, pitPos$mlbamID),-c(1:2)])

  file <- system.file("extdata", "2014BattingZips.csv", package="lastplayerpicked")
  a <- read.csv(file, stringsAsFactors=FALSE, check.names=FALSE)
  a$mlbamID <- grabIdFromName(a$Name)
  a <- a[,c(25,1:24)]
  names(a)[match('Name', names(a))] <- 'playerName'
  BattingZips2014 <- cbind(a, hitPos[match(a$mlbamID, hitPos$mlbamID),-c(1:2)])

  file <- system.file("extdata", "2014PitchingZips.csv", package="lastplayerpicked")
  a <- read.csv(file, stringsAsFactors=FALSE, check.names=FALSE)
  a$mlbamID <- grabIdFromName(a$Name)
  a <- a[,c(19,1:18)]
  names(a)[match('Name', names(a))] <- 'playerName'
  names(a)[match('SO', names(a))] <- 'K'
  PitchingZips2014 <- cbind(a, pitPos[match(a$mlbamID, pitPos$mlbamID),-c(1:2)])

  save(BattingOliver2014, PitchingOliver2014, file='Oliver2014.rda')
  save(BattingSteamer2014, PitchingSteamer2014, file='Steamer2014.rda')
  save(BattingZips2014, PitchingZips2014, file='Zips2014.rda')
  NULL
}

fix2014pt <- function() {
# playing time projections
# http://www.fantasypros.com/mlb/projections/hitters.php?filters=231:11
  # for hitters, PA should be second column
  file <- system.file("extdata", "BattingPlayingTime2014.csv", package="lastplayerpicked")
  a <- read.csv(file, stringsAsFactors=FALSE, check.names=FALSE)
  a$playerName <- sprintf("%s %s", a$Player, a$Name)
  a$mlbamID <- grabIdFromName(a$playerName)
  a$pa <- a$ab + a$bb
  BattingPlayingTime2014 <- a[,c('mlbamID','pa','playerName')]
#   write.csv(a[,c('mlbamID','pa','playerName')], file='stats/BattingPlayingTime2014m.csv', row.names=FALSE, quote=FALSE)

  # for pitchers, IP should be second column, saves fourth column
  file <- system.file("extdata", "PitchingPlayingTime2014.csv", package="lastplayerpicked")
  a <- read.csv(file, stringsAsFactors=FALSE, check.names=FALSE)
  a$playerName <- sprintf("%s %s", a$Player, a$Name)
  a$mlbamID <- grabIdFromName(a$playerName)
  PitchingPlayingTime2014 <- a[,c('mlbamID','pa','playerName','sv')]
#   write.csv(a[,c('mlbamID','ip','playerName','sv')], file='stats/PitchingPlayingTime2014m.csv', row.names=FALSE, quote=FALSE)
  save(BattingPlayingTime2014, PitchingPlayingTime2014, file='PlayingTime2014.rda')
  NULL
}
