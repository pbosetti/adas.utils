% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{expand_formula}
\alias{expand_formula}
\title{Expand a formula}
\usage{
expand_formula(f)
}
\arguments{
\item{f}{a formula}
}
\value{
a formula after expansion, e.g. \code{Y ~ A + B} becomes \code{Y ~ A + B + A:B}
}
\description{
Expand a formula
}
\examples{
expand_formula(Y ~ (A + B)^3)
}
