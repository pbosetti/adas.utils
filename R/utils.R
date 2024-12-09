#' Examples URL
#'
#' Provides the URL for the desired example data, so that it can be more easily
#' downloaded.
#'
#' @param example data file name
#'
#' @return the full URL for the desired example
#' @export
#' @return an URL string
#'
#' @examples
#' examples_url("battery.dat") |> read.table(header=TRUE)
examples_url <- function(example) {
  url = paste0("https://paolobosetti.quarto.pub/data/", example)
  return(url)
}


