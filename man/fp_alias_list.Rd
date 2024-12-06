% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factorial_plan.R
\name{fp_alias_list}
\alias{fp_alias_list}
\title{List All Alias for a Fractional Factorial Plan}
\usage{
fp_alias_list(arg)
}
\arguments{
\item{arg}{A formula for the defining relationship, or the number of factors}
}
\value{
a list of aliases
}
\description{
Given a defining relationship, this function lists all the aliases for a
fractional factorial plan.
}
\examples{
fp_alias_list(~A*B*C*D)
}
\seealso{
\code{\link[=fp_has_alias]{fp_has_alias()}} \code{\link[=fp_alias]{fp_alias()}}
}
