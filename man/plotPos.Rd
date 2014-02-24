\name{plotPos}
\alias{plotPos}
\alias{plotPos,league-method}
\title{
Plot Dollar Values by Position
}
\description{
Simple plot of dollar values by given position.
}

\usage{
plotPos(x, pos=c('C', '1B', '2B', '3B', 'SS', 'LF', 'CF', 'RF',
  'CI', 'MI', 'IF', 'OF', 'Util', 'SP', 'RP', 'P'))
}

\arguments{
  \item{x}{league object}
  \item{pos}{character string; position to examine}
}

\details{
Plot dollar values by position.  Look for clusters of similar players to find good prices.
}

\value{
plot
}

\examples{
l <- league()
plotPos(l, pos='OF')
}
