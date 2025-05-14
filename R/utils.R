#' Examples URL
#'
#' Provides the URL for the desired example data, so that it can be more easily
#' downloaded.
#'
#' @param example data file name
#'
#' @return the full URL for the desired example
#' @export
#'
#' @examples
#' examples_url("battery.dat") |> read.table(header=TRUE)
examples_url <- function(example) {
  url = paste0("https://paolobosetti.quarto.pub/data/", example)
  return(url)
}


#' Expand a formula
#'
#' @param f a formula
#'
#' @returns a formula after expansion, e.g. `Y ~ A + B` becomes `Y ~ A + B + A:B`
#' @export
#'
#' @examples
#' expand_formula(Y ~ (A + B)^3)
expand_formula <- function(f) {
  stopifnot(inherits(f, "formula"))
  reformulate(labels(terms(f)), f[[2]])
}

