\name{view}
\alias{view}
\alias{view,league-method}
\title{
View Player Data
}
\description{
View league projections and dollar values.
}

\usage{
view(x, playerType=c('All', 'Hitters', 'Pitchers'), trim=TRUE, addCols=NULL)
}

\arguments{
  \item{x}{league object}
  \item{playerType}{character string, type of players (Hitters/Pitchers) to return; defaults to 'All'}
  \item{trim}{logical, limit results to draftable players; defaults to TRUE}
  \item{addCols}{vector of character strings, additional columns to include; defaults to NULL.}
}

\details{
trim is recommended
}

\value{
list with components for hitters and pitchers
}

\examples{
l <- league()
view(l)
view(l, 'Pitchers', addCols='qs')
}
