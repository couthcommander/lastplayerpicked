\name{addKeepers}
\alias{addKeepers}
\alias{addKeepers,league-method}
\title{
Add Keeper Data
}
\description{
Add prices for keeper players.
}

\usage{
addKeepers(x, dat)
}

\arguments{
  \item{x}{league object}
  \item{dat}{data.frame; first column should contain player name or MLBAMid, second column should contain keeper price.}
}

\details{
Dollar values will be adjusted for inflation.
}

\value{
updated league object
}

\examples{
l <- league()
l <- addKeepers(l, data.frame(c('Mike Trout', 'Bryce Harper'), c(1, 3)))
}
