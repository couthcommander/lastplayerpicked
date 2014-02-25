setClass("league", representation(options = "list", hitters = "data.frame", pitchers = "data.frame", info = "list"))

setMethod("initialize", "league", function(.Object, ...) {
  glob <- list(...)
  dotNames <- names(glob)
  opts <- list()
  opts$batting <- validateBattingCategories(glob$batting)
  opts$pitching <- validatePitchingCategories(glob$pitching)
  opts$hitreq <- validateHittingPositions(glob$hitreq)
  opts$pitreq <- validatePitchingPositions(glob$pitreq)
  opts$posqual <- validatePosQualify(glob$posqual)
  opts$numberOfTeams <- validateNTeams(glob$numberOfTeams)
  opts$adjustPlayingTime <- validatePTadj(glob$adjustPlayingTime)
  opts$preset <- validatePreset(glob$preset)
  opts$league <- validateLeague(glob$league)
  opts$minimumBid <- validateMinBid(glob$minimumBid)
  playersPerTeam <- sum(opts$hitreq) + sum(opts$pitreq)
  opts$moneyPerTeam <- validateTeamMoney(glob$moneyPerTeam, playersPerTeam * opts$minimumBid)
  opts$nplayers <- validateNPlayers(glob$nplayers, playersPerTeam * opts$numberOfTeams)
  opts$hittersSplit <- validateSplit(glob$hittersSplit)
  # set unknown options
  for(i in setdiff(names(glob), names(opts))) {
    opts[[i]] <- glob[[i]]
  }
  .Object@options <- opts
  # info will hold stuff like inflationRate
  .Object@info <- list(data=loadData(glob$data, source=glob$source))
#   bf <- fileData(.Object, 'Batting', files=glob$files$batting)
#   pf <- fileData(.Object, 'Pitching', files=glob$files$pitching)
#   .Object@info <- list(battingFiles=bf, pitchingFiles=pf)
  cat("Loading player data...\n")
  .Object@hitters <- loadPlayers(.Object, 'Hitters')
  .Object@pitchers <- loadPlayers(.Object, 'Pitchers')
  .Object <- updateValuesDollars(.Object)
  nPitPos <- opts$pitreq * opts$numberOfTeams
  nPit <- sum(nPitPos)
  totalIP <- sum(.Object@pitchers[seq(nPit), 'IP']) / opts$numberOfTeams
  .Object
})

setGeneric("config", function(x) { standardGeneric("config") })
setMethod("config", "league", function(x) { print(x@options) })

setMethod("show", "league", function(object) {
  config(object)
})

league <- function(...) new('league', ...)

# setGeneric("fileData", function(x, ...) { standardGeneric("fileData") })
# setMethod("fileData", "league", function(x, playerType=c('Batting', 'Pitching'), files=NULL) {
#   type <- match.arg(playerType)
#   id <- x@options$dataset
#   year <- sub("([0-9]{2})*.*", "20\\1", id)
#   # year for custom dataset
#   if(year == 'CD') year <- format(Sys.time(), "%Y")
#   prevYear <- as.numeric(year) - 1
#   hf <- list()
#   hf$main <- file.path('stats', sub('[?]', type, datasets[datasets$id == id,'file']))
#   hf$extra <- file.path('stats', sprintf("Extra%sStats%s.csv", type, prevYear))
#   hf$pt <- file.path('stats', sprintf("%sPlayingTime%s.csv", type, year))
#   if(!is.null(files)) {
#     for(fn in names(files)) {
#       hf[[fn]] <- files[[fn]]
#     }
#   }
#   hf
# })

setGeneric("nHitters", function(x, ...) { standardGeneric("nHitters") })
setMethod("nHitters", "league", function(x) {
  sum(x@options$hitreq * x@options$numberOfTeams)
})

setGeneric("nPitchers", function(x, ...) { standardGeneric("nPitchers") })
setMethod("nPitchers", "league", function(x) {
  sum(x@options$pitreq * x@options$numberOfTeams)
})

# setGeneric("loadData", function(x, ...) { standardGeneric("loadData") })
# setMethod("loadData", "league", function(x, playerType=c('Hitters', 'Pitchers'), dataType='main') {
#   type <- match.arg(playerType)
#   if(type == 'Hitters') {
#     file <- x@info$battingFiles[[dataType]]
#   } else {
#     file <- x@info$pitchingFiles[[dataType]]
#   }
#   if(file.access(file, mode=4) == -1) {
#     warn <- sprintf("Failed to read %s file [%s=%s]", type, dataType, file)
#     warning(warn)
#     return(NULL)
#   }
#   read.csv(file, stringsAsFactors=FALSE, check.names=FALSE)
# })

setGeneric("loadGamesByPosition", function(x, ...) { standardGeneric("loadGamesByPosition") })
setMethod("loadGamesByPosition", "league", function(x, players, playerType=c('Hitters', 'Pitchers'), useTopPosition=TRUE) {
  type <- match.arg(playerType)
  if(type == 'Hitters') {
    positions <- x@options$hitreq
    minGames <- x@options$posqual['mg']
    default <- 'Util'
  } else {
    positions <- x@options$pitreq
    minGames <- x@options$posqual[2:3]
    default <- 'SP'
    if('P' %in% positions) default <- 'P'
  }
  pos <- rep(default, nrow(players))
  posCats <- intersect(sprintf("G_%s", names(positions[positions > 0])), names(players))
  if(length(posCats) == 0) return(pos)
  if(useTopPosition) {
    topPos <- apply(players[,posCats], MARGIN=1, FUN=function(i) {
      j <- which.max(i)
      if(length(j) == 0 || i[j] == 0) NA
      else j
    })
    pos <- ifelse(is.na(topPos), default, sub('G_', '', posCats[topPos]))
  } else {
    qualPos <- apply(players[,posCats], MARGIN=1, FUN=function(i) {
      paste(sub('G_', '', names(which(i >= minGames))), collapse='|')
    })
    pos <- ifelse(qualPos == '', default, qualPos)
  }
  pos
})

setGeneric("loadPlayers", function(x, ...) { standardGeneric("loadPlayers") })
setMethod("loadPlayers", "league", function(x, playerType=c('Hitters', 'Pitchers')) {
  type <- match.arg(playerType)
#   id <- x@options$dataset
  league <- x@options$league
  players <- switch(playerType, Hitters = x@info$data$batting$main, Pitchers = x@info$data$pitching$main)
  # merge team and league info
  dat <- x@info$data$team
  cats <- c('Team','league')
  if(!is.null(dat) && all(cats %in% names(dat))) {
    players[,cats] <- mergeFromX(players$mlbamID, dat[,c('mlbamID',cats)])
  }
#   players <- loadData(x, type, 'main')
  if(league != 'MLB' && 'league' %in% names(players)) {
    players <- players[players[,'league'] == league,]
  }
  # merge with IDs from different sites
  players[,c('statsID', 'cbsID', 'espnID')] <- loadPlayerIds(players$mlbamID)
  if(type == 'Pitchers') {
    pitcat <- x@options$pitching
    if(any(c('HLD', 'S+HLD') %in% pitcat)) {
#       ep <- loadData(x, type, 'extra')
      ep <- x@info$data$pitching$extra
      cix <- match('HLD', names(ep))
      if(!is.na(cix)) players$HLD <- mergeFromX(players$mlbamID, ep[,c(1,cix)])
      # players$HLD <- ep[match(players$mlbamID, ep$mlbamID), 'HLD']
    }
    if('QS' %in% pitcat) {
      players$qs <- createQualityStartsProjections(players)
    }
#     custom <- x@info$pitchingFiles
  }
  if(type == 'Hitters') {
    hitcat <- x@options$batting
    cats <- intersect(c('E', 'GIDP', 'A'), hitcat)
    if(length(cats)) {
#       eh <- loadData(x, type, 'extra')
      eh <- x@info$data$batting$extra
      cix <- match(cats, names(eh))
      if(all(!is.na(cix))) players[,cats] <- mergeFromX(players$mlbamID, eh[,c(1,cix)])
      # if(!is.null(eh)) players[,cats] <- eh[match(players$mlbamID, eh$mlbamID), cats]
    }
#     custom <- x@info$battingFiles
  }
  ### currently don't load any custom data
#   # load custom stats
#   for(fh in names(custom)) {
#     if(!fh %in% c('main','extra','pt')) {
#       dat <- loadData(x, type, fh)
#       cats <- setdiff(c('mlbamID', 'playerName'), names(dat))
#       if(length(cats) && 'mlbamID' %in% names(dat)) {
#         players[,cats] <- mergeFromX(players$mlbamID, dat[,c('mlbamID',cats)])
#       } else {
#         warn <- sprintf("Unable to merge custom %s dataset [%s=%s]", type, fh, custom[[fh]])
#         warning(warn)
#       }
#     }
#   }
#   if(!is.null(file)) {
#     dat <- read.csv(file, stringsAsFactors=FALSE, check.names=FALSE)
#     cats <- setdiff(c('mlbamID', 'playerName'), names(dat))
#     if(length(cats) && 'mlbamID' %in% names(dat)) {
#       players[,cats] <- dat[match(players$mlbamID, dat$mlbamID), cats]
#     }
#   }
  # adjust years 11-13 if not using season data
  if(x@options$adjustPlayingTime) {
    players <- adjustPT(players, x, type)
  }
  # determine position eligibility
  players$pos <- loadGamesByPosition(x, players, type)
  players
})

setGeneric("buildHitValues", function(x, ...) { standardGeneric("buildHitValues") })
setMethod("buildHitValues", "league", function(x) {
  players <- x@hitters
  # columns expected
  reqCols <- c('H','2B','3B','HR','R','RBI','AB','BB','HBP','SF','SB','CS')
  missCols <- setdiff(reqCols, names(players))
  if(length(missCols)) {
    warning(sprintf("Missing required columns [%s], defaulting to zero", paste(missCols, collapse=', ')))
    players[,missCols] <- 0
  }
  # compute hitting values
  players[,'1B'] <- players[,'H'] - players[,'2B'] - players[,'3B'] - players[,'HR']
  players[,'TB'] <- players[,'1B'] + players[,'2B']*2 + players[,'3B']*3 + players[,'HR']*4
  players[,'2B+3B'] <- players[,'2B'] + players[,'3B']
  players[,'RP'] <- players[,'R'] + players[,'RBI'] - players[,'HR']
  players[,'PA'] <- players[,'AB'] + players[,'BB'] + players[,'HBP'] + players[,'SF']
  players[,'SB-CS'] <- players[,'SB'] - players[,'CS']
  players[,'AVG'] <- buildRateStat(players[,'H'], players[,'AB'])
  players[,'OBP'] <- buildRateStat(rowSums(players[,c('H','BB','HBP')]), players[,'PA'])
  players[,'SLG'] <- buildRateStat(players[,'TB'], players[,'AB'])
  players[,'OPS'] <- players[,'OBP'] + players[,'SLG']
  players[,'SB%'] <- buildRateStat(players[,'SB'], rowSums(players[,c('SB','CS')]))
  players[,'KAB'] <- buildRateStat(players[,'SO'], players[,'AB'])
  # compute standardized values
  nHitPos <- x@options$hitreq * x@options$numberOfTeams
  nHit <- sum(nHitPos)
  batCats <- x@options$batting
  missCols <- setdiff(batCats, union(names(players), c('xAVG','xOBP','xSLG','xOPS','xSB%','xKAB')))
  if(length(missCols)) {
    warning(sprintf("Missing required columns [%s], defaulting to zero", paste(missCols, collapse=', ')))
    players[,missCols] <- 0
  }
  numberOfIterations <- 0
  totalValue <- 0
  pastStd <- NA
  isSettled <- FALSE
  while(!isSettled) {
    xAVG <- buildXStat('H', 'AB', players, nHit)
    xOBP <- buildXStat(c('H','BB','HBP'), c('AB','BB','HBP','SF'), players, nHit)
    xSLG <- buildXStat('TB', 'AB', players, nHit)
    xOPS <- buildXOPS(players, nHit)
    xSB <- buildXStat('SB', c('SB','CS'), players, nHit)
    xKAB <- buildXStat('SO', 'AB', players, nHit)
    xStatAverages <- c(xAVG=xAVG[[2]], xOBP=xOBP[[2]], xSLG=xSLG[[2]], xOPS=xOPS[[2]], 'xSB%'=xSB[[2]], xKAB=xKAB[[2]])
    players[,'xAVG'] <- xAVG[[1]]
    players[,'xOBP'] <- xOBP[[1]]
    players[,'xSLG'] <- xSLG[[1]]
    players[,'xOPS'] <- xOPS[[1]]
    players[,'xSB%'] <- xSB[[1]]
    players[,'xKAB'] <- xKAB[[1]]
    hitasd <- buildAveStandardDeviations(players, nHit, batCats)
    x@info$hitasd <- lapply(hitasd, round, 4)
    stdDevs <- hitasd[[1]]
    averages <- hitasd[[2]]
    hitStandard <- buildStandardScores(players, batCats, hitasd)
    players[,colnames(hitStandard)] <- hitStandard
    players <- players[order(players[,'total'], decreasing=TRUE),]
    players[,'posAdj'] <- adjustForReplacementLevel(players, nHitPos)
    players[,'adjTotal'] <- players[,'total'] - players[,'posAdj']
    players <- players[order(players[,'adjTotal'], decreasing=TRUE),]
    if(numberOfIterations > 0) {
      if(all(abs(stdDevs - pastStd) < 0.000001)) isSettled <- TRUE
      # sometimes std would oscillate so break after 10
      if(numberOfIterations > 10) isSettled <- TRUE
    }
    pastStd <- stdDevs
    hitValue <- buildTotalValue(players, nHit)
    numberOfIterations <- numberOfIterations + 1
    cat(sprintf("iteration: %s\r", numberOfIterations))
  }
  cat("\n")
  x@hitters <- players
  x
})

setGeneric("buildPitValues", function(x, ...) { standardGeneric("buildPitValues") })
setMethod("buildPitValues", "league", function(x) {
  players <- x@pitchers
  pitcats <- names(players)
  # columns expected
  reqCols <- c('W','L','K','BB','IP','H','Outs','ER')
  missCols <- setdiff(reqCols, names(players))
  if(length(missCols)) {
    warning(sprintf("Missing required columns [%s], defaulting to zero", paste(missCols, collapse=', ')))
    players[,missCols] <- 0
  }
  # compute pitching values
  players[,'W-L'] <- players[,'W'] - players[,'L']
  players[,'2W-L'] <- players[,'W']*2 - players[,'L']
  players[,'K-BB'] <- players[,'K'] - players[,'BB']
  if('HLD' %in% pitcats) players[,'S+HLD'] <- players[,'S'] + players[,'HLD']
  players[,'Outs'] <- players[,'IP']*3
  denom <- players[,'H'] + players[,'Outs']
  players[,'BAA'] <- ifelse(denom > 0, players[,'H'] / denom, 0)
  players[,'ERA'] <- buildRateStat(players[,'ER']*9, players[,'IP'])
  players[,'WHIP'] <- buildRateStat(rowSums(players[,c('H','BB')]), players[,'IP'])
  players[,'K9'] <- buildRateStat(players[,'K'], players[,'IP']/9)
  players[,'BB9'] <- buildRateStat(players[,'BB'], players[,'IP']/9)
  players[,'KBB'] <- buildRateStat(players[,'K'], players[,'BB'])
  if('HR' %in% pitcats) players[,'HR9'] <- buildRateStat(players[,'HR'], players[,'IP'])
  players[,'W%'] <- buildRateStat(players[,'W'], rowSums(players[,c('W','L')]))
  # compute standardized values
  nPitPos <- x@options$pitreq * x@options$numberOfTeams
  nPit <- sum(nPitPos)
  pitCats <- x@options$pitching
  missCols <- setdiff(pitCats, union(names(players), c('xERA','xWHIP','xK9','xBB9','xKBB','xHR9','xW%','xBAA')))
  if(length(missCols)) {
    warning(sprintf("Missing required columns [%s], defaulting to zero", paste(missCols, collapse=', ')))
    players[,missCols] <- 0
  }
  numberOfIterations <- 0
  totalValue <- 0
  pastStd <- NA
  isSettled <- FALSE
  while(!isSettled) {
    xERA <- buildXStat('ER', 'IP', players, nPit, TRUE)
    xWHIP <- buildXStat(c('BB','H'), 'IP', players, nPit, TRUE)
    xK9 <- buildXStat('K', 'IP', players, nPit, FALSE)
    xBB9 <- buildXStat('BB', 'IP', players, nPit, TRUE)
    xKBB <- buildXStat('K', 'BB', players, nPit, FALSE)
    if('HR' %in% pitcats) xHR9 <- buildXStat('HR', 'IP', players, nPit, TRUE)
    xW <- buildXStat('W', c('W','L'), players, nPit, FALSE)
    xBAA <- buildXStat('H', c('H','Outs'), players, nPit, TRUE)
    xStatAverages <- c(xERA=xERA[[2]], xWHIP=xWHIP[[2]], xK9=xK9[[2]], xBB9=xBB9[[2]],
      xKBB=xKBB[[2]], xHR9=xHR9[[2]], 'xW%'=xW[[2]], xBAA=xBAA[[2]]
    )
    players[,'xERA'] <- xERA[[1]]
    players[,'xWHIP'] <- xWHIP[[1]]
    players[,'xK9'] <- xK9[[1]]
    players[,'xBB9'] <- xBB9[[1]]
    players[,'xKBB'] <- xKBB[[1]]
    players[,'xHR9'] <- xHR9[[1]]
    players[,'xW%'] <- xW[[1]]
    players[,'xBAA'] <- xBAA[[1]]
    pitasd <- buildAveStandardDeviations(players, nPit, pitCats)
    x@info$pitasd <- lapply(pitasd, round, 4)
    stdDevs <- pitasd[[1]]
    averages <- pitasd[[2]]
    pitStandard <- buildStandardScores(players, pitCats, pitasd)
    players[,colnames(pitStandard)] <- pitStandard
    players <- players[order(players[,'total'], decreasing=TRUE),]
    players[,'posAdj'] <- adjustForReplacementLevel(players, nPitPos)
    players[,'adjTotal'] <- players[,'total'] - players[,'posAdj']
    players <- players[order(players[,'adjTotal'], decreasing=TRUE),]
    if(numberOfIterations > 0) {
      if(all(abs(stdDevs - pastStd) < 0.000001)) isSettled <- TRUE
      # sometimes std would oscillate so break after 10
      if(numberOfIterations > 10) isSettled <- TRUE
    }
    pastStd <- stdDevs
    pitValue <- buildTotalValue(players, nPit)
    numberOfIterations <- numberOfIterations + 1
    cat(sprintf("iteration: %s\r", numberOfIterations))
  }
  cat("\n")
  x@pitchers <- players
  x
})

setGeneric("createDollarValues", function(x, ...) { standardGeneric("createDollarValues") })
setMethod("createDollarValues", "league", function(x, scaleFactor) {
  minBid <- x@options$minimumBid
  x@hitters[,'dollarValue'] <- x@hitters[,'adjTotal'] * scaleFactor[1] + minBid
  x@pitchers[,'dollarValue'] <- x@pitchers[,'adjTotal'] * scaleFactor[2] + minBid
  x
})

setGeneric("createCategoryDollarValues", function(x, ...) { standardGeneric("createCategoryDollarValues") })
setMethod("createCategoryDollarValues", "league", function(x, scaleFactor) {
  hitcat <- x@options$batting
  pitcat <- x@options$pitching
  x@hitters[,sprintf("%s$", hitcat)] <- x@hitters[,sprintf("m%s", hitcat)] * scaleFactor[1]
  x@pitchers[,sprintf("%s$", pitcat)] <- x@pitchers[,sprintf("m%s", pitcat)] * scaleFactor[2]
  x@hitters[,'Pos$'] <- (x@hitters[,'adjTotal'] - x@hitters[,'total']) * scaleFactor[1]
  x@pitchers[,'Pos$'] <- (x@pitchers[,'adjTotal'] - x@pitchers[,'total']) * scaleFactor[2]
  x
})

setGeneric("adjustForInflation", function(x, ...) { standardGeneric("adjustForInflation") })
setMethod("adjustForInflation", "league", function(x, marginalMoney) {
  keeperValues <- 0
  keeperPrices <- 0
  inflationRate <- 0
  useKeepers <- FALSE
  if('keeperPrice' %in% names(x@hitters)) {
    useKeepers <- TRUE
    price <- as.numeric(gsub('[$,]', '', x@hitters[,'keeperPrice']))
    ix <- which(!is.na(price))
    keeperValues <- sum(x@hitters[ix,'dollarValue'])
    keeperPrices <- sum(price[ix])
  }
  if('keeperPrice' %in% names(x@pitchers)) {
    useKeepers <- TRUE
    price <- as.numeric(gsub('[$,]', '', x@pitchers[,'keeperPrice']))
    ix <- which(!is.na(price))
    keeperValues <- keeperValues + sum(x@pitchers[ix,'dollarValue'])
    keeperPrices <- keeperPrices + sum(price[ix])
  }
  if(useKeepers) {
    valueAvailableAtDraft <- marginalMoney - keeperValues
    moneyAvailableAtDraft <- marginalMoney - keeperPrices
    inflationRate <- moneyAvailableAtDraft / valueAvailableAtDraft - 1
    x@hitters[,'adjustedDollarValue'] <- x@hitters[,'dollarValue'] * (1 + inflationRate) - inflationRate
    x@pitchers[,'adjustedDollarValue'] <- x@pitchers[,'dollarValue'] * (1 + inflationRate) - inflationRate
  }
  x@info$inflationRate <- inflationRate
  x
})

setGeneric("buildDollars", function(x, ...) { standardGeneric("buildDollars") })
setMethod("buildDollars", "league", function(x) {
  totalDraftMoney <- x@options$moneyPerTeam * x@options$numberOfTeams
  hsplit <- x@options$hittersSplit
  hval <- buildTotalValue(x@hitters, nHitters(x))
  pval <- buildTotalValue(x@pitchers, nPitchers(x))
  if(hsplit == FALSE) {
    totalValue <- hval + pval
    scaleFactor <- 0
    marginalMoney <- totalDraftMoney - x@options$minimumBid * (nHitters(x) + nPitchers(x))
    if(totalValue > 0) {
      scaleFactor <- rep(marginalMoney / totalValue, 2)
    }
  } else {
    hitterScale <- pitcherScale <- 0
    marginalHitterMoney <- hsplit/100 * totalDraftMoney - x@options$minimumBid * nHitters(x)
    marginalPitcherMoney <- (100 - hsplit)/100 * totalDraftMoney - x@options$minimumBid * nPitchers(x)
    marginalMoney <- marginalHitterMoney + marginalPitcherMoney
    if(hval > 0) hitterScale <- marginalHitterMoney / hval
    if(pval > 0) pitcherScale <- marginalPitcherMoney / pval
    scaleFactor <- c(hitterScale, pitcherScale)
  }
  x <- createDollarValues(x, scaleFactor)
  x <- createCategoryDollarValues(x, scaleFactor)
  x <- adjustForInflation(x, marginalMoney)
  x
})

setGeneric("updateValuesDollars", function(x, ...) { standardGeneric("updateValuesDollars") })
setMethod("updateValuesDollars", "league", function(x) {
  cat("Building player values...\n")
  x <- buildHitValues(x)
  x <- buildPitValues(x)
  cat("Generating dollar values...\n")
  x <- buildDollars(x)
  x
})

setGeneric("view", function(x, playerType=c('All', 'Hitters', 'Pitchers'), trim=TRUE, addCols=NULL) { standardGeneric("view") })
setMethod("view", "league", function(x, playerType, trim, addCols) {
  type <- match.arg(playerType)
  res <- list()
  if(type %in% c('All', 'Hitters')) {
    cols <- c('playerName','team','pos',x@options$batting,'total','posAdj','adjTotal','dollarValue','keeperPrice','adjustedDollarValue')
    if(!is.null(addCols)) cols <- c(cols, addCols)
    cols <- unique(intersect(cols, names(x@hitters)))
    ix <- TRUE
    if(trim) ix <- seq(nHitters(x))
    res <- append(res, list(hitters=x@hitters[ix, cols]))
  }
  if(type %in% c('All', 'Pitchers')) {
    cols <- c('playerName','team','pos',x@options$pitching,'total','posAdj','adjTotal','dollarValue','keeperPrice','adjustedDollarValue')
    if(!is.null(addCols)) cols <- c(cols, addCols)
    cols <- unique(intersect(cols, names(x@pitchers)))
    ix <- TRUE
    if(trim) ix <- seq(nPitchers(x))
    res <- append(res, list(pitchers=x@pitchers[ix, cols]))
  }
  res
})

setGeneric("addKeepers", function(x, dat) { standardGeneric("addKeepers") })
setMethod("addKeepers", "league", function(x, dat) {
  # name or ID in column 1
  # price in column 2
  col1 <- suppressWarnings(as.numeric(as.character(dat[,1])))
  isID <- !is.na(col1) & is.numeric(col1)
  ids <- dat[isID,]
  names <- dat[!isID,]
  hitToAdd <- list()
  pitToAdd <- list()
  # do matches
  hitName <- match(names[,1], x@hitters[,'playerName'])
  valid <- !is.na(hitName)
  if(any(valid)) {
    hitToAdd$name <- data.frame(row=hitName[valid], price=names[valid,2])
  }
  hitID <- match(ids[,1], x@hitters[,'mlbamID'])
  valid <- !is.na(hitID)
  if(any(valid)) {
    hitToAdd$id <- data.frame(row=hitID[valid], price=ids[valid,2])
  }

  pitName <- match(names[,1], x@pitchers[,'playerName'])
  valid <- !is.na(pitName)
  if(any(valid)) {
    pitToAdd$name <- data.frame(row=pitName[valid], price=names[valid,2])
  }
  pitID <- match(ids[,1], x@pitchers[,'mlbamID'])
  valid <- !is.na(pitID)
  if(any(valid)) {
    pitToAdd$id <- data.frame(row=pitID[valid], price=ids[valid,2])
  }

  if(length(hitToAdd)) {
    hitToAdd <- do.call(rbind, hitToAdd)
    x@hitters[hitToAdd$row,'keeperPrice'] <- hitToAdd$price
    cat(sprintf("Hitters added: %s\n", paste(unique(x@hitters[hitToAdd$row,'playerName']), collapse=', ')))
  }
  if(length(pitToAdd)) {
    pitToAdd <- do.call(rbind, pitToAdd)
    x@pitchers[pitToAdd$row,'keeperPrice'] <- pitToAdd$price
    cat(sprintf("Pitchers added: %s\n", paste(unique(x@pitchers[pitToAdd$row,'playerName']), collapse=', ')))
  }
  cat("Generating dollar values...\n")
  buildDollars(x)
})

setGeneric("plotPos", function(x, pos=c('C', '1B', '2B', '3B', 'SS', 'LF', 'CF', 'RF', 'CI', 'MI', 'IF', 'OF', 'Util', 'SP', 'RP', 'P')) { standardGeneric("plotPos") })
setMethod("plotPos", "league", function(x, pos) {
  pos <- match.arg(pos)
  validPos <- posMap(pos)
  if(pos %in% c('SP','RP','P')) {
    players <- x@pitchers
  } else {
    players <- x@hitters
  }
  mxy <- max(players$dollarValue, na.rm=TRUE)
  set <- players[players$pos %in% validPos & players$dollarValue >= x@options$minimumBid ,]
  plot(seq(nrow(set)), set$dollarValue, type='o', xlab=pos, ylab='Dollar Value', ylim=c(0,mxy))
})

setGeneric("exportData", function(x, filename='lpp', trim=TRUE, addCols=NULL) { standardGeneric("exportData") })
setMethod("exportData", "league", function(x, filename, trim, addCols) {
  fdir <- dirname(filename)
  fname <- basename(filename)
  dat <- view(x, playerType='All', trim, addCols)
  write.csv(dat$hitters, file = file.path(fdir, sprintf("%s_hit.csv", fname)), row.names=FALSE)
  write.csv(dat$pitchers, file = file.path(fdir, sprintf("%s_pit.csv", fname)), row.names=FALSE)
  NULL
})
