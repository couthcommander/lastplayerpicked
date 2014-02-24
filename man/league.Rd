\name{league}
\alias{league}
\alias{league-class}
\alias{show,league-method}
\title{
Fantasy Baseball League
}
\description{
Customize your fantasy baseball league and generate dollar values.
}

\usage{
league(...)
}

\arguments{
  \item{\dots}{arguments will be saved to league options}
}

\details{
After specifying the league configuration, players will be loaded and dollar values will be generated.
If data is not specified, the source argument will be used to load default datasets included with this package.
If components of data are missing, the default datasets will be used (by default, extra is NULL).
The following items are useful league options.
\itemize{
  \item{batting}{vector of character strings, batting categories; defaults to c('xAVG','R','RBI','HR','SB')}
  \item{pitching}{vector of character strings, pitching categories; defaults to c('W','S','xERA','xWHIP','K')}
  \item{hitreq}{vector of numeric values, position requirements for hitters; 
    defaults to c(C=1, '1B'=1, '2B'=1, '3B'=1, SS=1, LF=0, CF=0, RF=0, CI=1, MI=1, IF=0, OF=5, Util=1)
  }
  \item{pitreq}{vector of numeric values, position requirements for pitchers; defaults to c(SP=6, RP=3, P=0).
  }
  \item{posqual}{list of numeric values, position eligibility requirements; defaults to c(mg=20, ms=5, mr=5).
    mg is minimum games for a hitter to qualify at a position
    ms is minimum games for a pitcher to qualify as a starter
    mr is minimum games for a pitcher to qualify as a reliever
  }
  \item{numberOfTeams}{numeric value, number of teams in the league; defaults to 12}
  \item{adjustPlayingTime}{logical value, should playing time adjustments be made; defaults to TRUE.}
  \item{preset}{character string, description of league; defaults to 'Standard Roto'.
    Options include 'Standard Roto', 'ESPN', 'Yahoo!', 'Custom'.
    This value is unused at this time.
  }
  \item{league}{character string, league subset of eligible players; defaults to 'MLB'.
    Options include 'MLB', 'AL', 'NL'.
  }
  \item{minimumBid}{numeric value, minimum bid per player; defaults to 1.}
  \item{moneyPerTeam}{numeric value, cap space for each team; defaults to 260.}
  \item{nplayers}{integer value, the number of players to include; defaults to number of players per team times the number of teams.
    This value is unused at this time.
  }
  \item{hittersSplit}{numeric value, the percentage of dollars that should be spent on hitters; defaults to 70.
    The difference between this number and 100 will set the pitchersSplit, thus it defaults to 30.
  }
  \item{data}{list, contains custom datasets; batting and pitching lists should be given, each with main, pt, and extra components.
    main should contain basic hitting/pitching stats and games played.
    pt should contain playing time projections.
    extra should contain extra hitting/pitching stats.
  }
  \item{source}{character string, set the default dataset; defaults to 'steamer14'}
}
}

\value{
league object
}

\examples{
# unspecified arguments will use default values
fandom <- league(
  numberOfTeams=10,
  league='AL',
  minimumBid=1,
  moneyPerTeam=250,
  hittersSplit=65,
  source='steamer14'
)
hit <- view(fandom, 'Hitters')$hitters
par(mfrow=c(4,2))
for(p in unique(hit$pos)) plotPos(fandom, p)
}
