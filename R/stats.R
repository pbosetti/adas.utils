library(glue)


#' chauvenet
#' Applies the Chauvenet's criterion to a sample, identifying a possible
#' outlier.
#'
#' @param x the sample vector
#' @param threshold the threshold for the frequency of the suspect outlier
#'
#' @return a list with the following components:
#' \itemize{
#'  \item{s0}{the maximum difference}
#'  \item{index}{the index of the suspect outlier}
#'  \item{value}{the value of the suspect outlier}
#'  \item{reject}{a logical value indicating whether the suspect outlier should be rejected}
#'  }
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' chauvenet(x)
#' chauvenet(x, threshold=0.1)
chauvenet <- function(x, threshold=0.5) {
  abs.diff <- abs(x - mean(x)) / sd(x) # vettore delle differenze
  s0 <- max(abs.diff)                  # massima diff.
  i0 <- which.max(abs.diff)            # posizione mass. diff.
  freq <- length(x) * pnorm(s0, lower.tail = F)
  result <- list(
    s0 = s0,
    index = i0,
    value = x[i0],
    reject = freq < threshold
  )
  samp <- deparse(substitute(x))
  print(glue("Chauvenet's criterion for sample {samp}"))
  print(glue("Suspect outlier: {i0}, value {x[i0]}"))
  print(glue("Expected frequency: {freq}, threshold: {threshold}"))
  print(glue("Decision: {d}", d=ifelse(result$reject, "reject it", "keep it")))
  invisible(result)
}
