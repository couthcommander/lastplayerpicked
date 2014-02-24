\name{exportData}
\alias{exportData}
\alias{exportData,league-method}
\title{
Export Data to CSV
}
\description{
Export dollar values to CSV files
}

\usage{
exportData(x, filename='lpp', trim=TRUE, addCols=NULL)
}

\arguments{
  \item{x}{league object}
  \item{filename}{character string, basename for CSV files; defaults to 'lpp'}
  \item{trim}{logical, limit results to draftable players; defaults to TRUE}
  \item{addCols}{vector of character strings, additional columns to include; defaults to NULL.}
}

\details{
Saves CSV files to working directory; makes use of the view method before calling write.csv
}

\examples{
\dontrun{
l <- league(source='zips14')
exportData(l, filename='zips14')
}
}
