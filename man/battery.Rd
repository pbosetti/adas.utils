% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{battery}
\alias{battery}
\title{Battery experiment data}
\format{
A data frame with 36 rows and 6 columns
}
\usage{
battery
}
\description{
Battery life in hour of a factorial experiment with 2 factors and 3 levels
each. Factors are:
}
\details{
\itemize{
\item \code{Temperature}: of the battery during the discharge experiment
\item \code{Material}: Plate material for the battery
}

Other columns are:
\itemize{
\item \code{StandardOrder}: Yate's standard order
\item \code{RunOrder}: randomized order, in which tests have been executed
\item \code{Repeat}: repeat number
\item \code{Response}: battery life in hours
}
}
\references{
Douglas C. Montgomery, "Design and Analysis of Experiments", 8th edition, Wiley, 2019
}
\keyword{datasets}
