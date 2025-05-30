% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factorial_plan.R
\name{fp_add_names}
\alias{fp_add_names}
\title{Add factor names to a design matrix}
\usage{
fp_add_names(dm, ...)
}
\arguments{
\item{dm}{the design matrix.}

\item{...}{a set of factors to name, with their respective names, e.g.
\verb{A="Temperature", B="Pressure"}. If the factor is not in the design matrix
factors list, a warning is printed and the factor is skipped.}
}
\value{
the design matrix with the named factors.
}
\description{
Store factor names in the \code{factorial.plan} object, as a list within the
\code{factor.names} attribute.
}
\examples{
fp_design_matrix(3, rep=2) \%>\%
  fp_add_names(A="Temperature", B="Pressure")
}
