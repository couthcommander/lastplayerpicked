validateHittingPositions <- function(x) {
  def <- c(C=1, '1B'=1, '2B'=1, '3B'=1, SS=1, LF=0, CF=0, RF=0, CI=1, MI=1, IF=0, OF=5, Util=1)
  bad <- setdiff(names(x), names(def))
  fail <- FALSE
  if(length(bad)) {
    warning(sprintf("Invalid positions specified for hitters [%s], using defaults", paste(bad, collapse=', ')))
    fail <- TRUE
  }
  if(any(x < 0)) {
    warning("Position requirements cannot be negative, using defaults")
    fail <- TRUE
  }
  if(fail) {
    print(def)
  } else if(length(x)) {
    def <- x
  }
  def
}

validatePitchingPositions <- function(x) {
  def <- c(SP=6, RP=3, P=0)
  bad <- setdiff(names(x), names(def))
  fail <- FALSE
  if(length(bad)) {
    warning(sprintf("Invalid positions specified for pitchers [%s], using defaults", paste(bad, collapse=', ')))
    fail <- TRUE
  }
  if(any(x < 0)) {
    warning("Position requirements cannot be negative, using defaults")
    fail <- TRUE
  }
  if(fail) {
    print(def)
  } else if(length(x)) {
    def <- x
  }
  def
}

validateBattingCategories <- function(x) {
  battingOpts <- c("xAVG"="AVG", "R"="R", "RBI"="RBI", "HR"="HR", "SB"="SB",
    "xOBP"="OBP", "xSLG"="SLG", "xOPS"="OPS", "H"="H", "BB"="BB", "SI"="1B", 
    "DB"="2B", "TP"="3B", "2B3B"="2B + 3B", "TB"="TB", "xSB%"="SB%", "SB-CS"="SB - CS", 
    "RP"="RP (R + RBI - HR)", "SO"="K", "xKAB"="K/AB", "GIDP"="GIDP", E="Errors", A="Assists"
  )
  def <- c('xAVG','R','RBI','HR','SB')
  bad <- setdiff(x, names(battingOpts))
  if(length(bad)) {
    warning(sprintf("Invalid categories specified for hitters [%s], using defaults [%s]", paste(bad, collapse=', '), paste(def, collaspe=', ')))
  } else if(length(x)) {
    def <- x
  }
  def
}

validatePitchingCategories <- function(x) {
  pitchingOpts <- c(W="W", S="S", xERA="ERA", xWHIP="WHIP", K="K",
    BB="BB", H="H", HR="HR", "W-L"="W - L", "2W-L"="2W - L", L="L",
    xK9="K/9", xKBB="K/BB", xBB9="BB/9", xHR9="HR/9", "xW%"="W%", IP="IP",
    "K-BB"="K - BB", xBAA="BAA", G="G", QS="QS", HLD="HLD", "S+HLD"="S + HLD"
  )
  def <- c('W','S','xERA','xWHIP','K')
  bad <- setdiff(x, names(pitchingOpts))
  if(length(bad)) {
    warning(sprintf("Invalid categories specified for pitchers [%s], using defaults [%s]", paste(bad, collapse=', '), paste(def, collapse=', ')))
  } else if(length(x)) {
    def <- x
  }
  def
}

validatePosQualify <- function(x) {
  def <- c(mg=20, ms=5, mr=5)
  if(is.null(x)) return(def)
  fail <- FALSE
  if(!isTRUE(all.equal(sort(names(x)), sort(names(def))))) {
    warning("Invalid names given for position qualifications, using defaults")
    fail <- TRUE
  }
  if(any(x < 1) || any(x > 162)) {
    warning("Invalid values given for position qualifications, using defaults")
    fail <- TRUE
  }
  if(fail) {
    print(def)
  } else if(length(x)) {
    def <- x
  }
  def
}

validatePTadj <- function(x) {
  def <- TRUE
  if(!is.null(x) && x == FALSE) def <- FALSE
  def
}

validatePreset <- function(x) {
  opts <- c(S="Standard Roto", E="ESPN", Y="Yahoo!", C="Custom")
  def <- 'Standard Roto'
  if(is.null(x)) return(def)
  if(x %in% opts) {
    def <- x
  } else {
    warning(sprintf("Invalid preset [%s], using default [%s]", x, def))
  }
  def
}

validateNTeams <- function(x) {
  def <- 12
  if(is.null(x)) return(def)
  if(x > 1 && x < 100) {
    def <- x
  } else {
    warning(sprintf("Invalid number of teams [%s], using default[%s]", x, def))
  }
  def
}

validateLeague <- function(x) {
  opts <- c(MLB="MLB", AL="AL", NL="NL")
  def <- 'MLB'
  if(is.null(x)) return(def)
  if(x %in% opts) {
    def <- x
  } else {
    warning(sprintf("Invalid league [%s], using default [%s]", x, def))
  }
  def
}

validateTeamMoney <- function(x, y) {
  def <- 260
  if(is.null(x)) return(def)
  if(x > y && x < Inf) {
    def <- x
  } else {
    warning(sprintf("Invalid team salary cap [%s], using default[%s]", x, def))
  }
  def
}

validateMinBid <- function(x) {
  def <- 1
  if(is.null(x)) return(def)
  if(x > 0 && x < Inf) {
    def <- x
  } else {
    warning(sprintf("Invalid minimum bid [%s], using default[%s]", x, def))
  }
  def
}

validateNPlayers <- function(x, y) {
  def <- y
  if(is.null(x)) return(def)
  if(x > y && x < Inf) {
    def <- x
  } else {
    warning(sprintf("Invalid number of players requested [%s], using default[%s]", x, def))
  }
  def
}

validateSplit <- function(x) {
  def <- 70
  if(is.null(x)) return(def)
  if(x > 0 && x < 100) {
    def <- x
  } else {
    warning(sprintf("Invalid split given to hitters [%s], using default[%s]", x, def))
  }
  def
}
