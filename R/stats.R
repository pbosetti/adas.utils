library(glue)
library(tidyverse)

#' Chauvenet's criterion
#'
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
  print(glue::glue("Chauvenet's criterion for sample {samp}"))
  print(glue::glue("Suspect outlier: {i0}, value {x[i0]}"))
  print(glue::glue("Expected frequency: {freq}, threshold: {threshold}"))
  print(glue::glue("Decision: {d}", d=ifelse(result$reject, "reject it", "keep it")))
  invisible(result)
}


#' Daniel's plot
#'
#' Given a non-replicated model of a factorial plan, this function provides a
#' QQ plot of the effects of the model, labeling all the effects.
#'
#' @param model a linear model
#' @param alpha the transparency of the horizontal lines
#' @param xlim the limits of the x-axis
#'
#' @return a QQ plot with the effects of the model
#' @export
#'
#' @examples
#' daniel_plot(lm(mpg ~ wt, data=mtcars))
daniel_plot <- function(model, alpha=0.5, xlim=c(-3,3)) {
  e <- effects(model)
  tibble::tibble(
    term = names(e),
    value = as.numeric(e)
  ) |>
    dplyr::slice_tail(n=length(e) - 1) |>
    ggplot2::ggplot(ggplot2::aes(sample=value)) +
    ggplot2::stat_qq() +
    ggplot2::geom_qq_line(color="red") +
    ggplot2::geom_hline(ggplot2::aes(yintercept=value), alpha=alpha) +
    ggplot2::geom_label(ggplot2::aes(y=value, x=xlim[1], label=term), hjust="left") +
    ggplot2::coord_cartesian() +
    ggplot2::labs(x="Theoretical quantiles", y="Sample quantiles")
}


#' Pareto's chart
#'
#' This is a generic function for Pareto's chart.
#'
#' @param model a model
#'
#' @return a Pareto chart of the effects of the model
#' @export
#'
#' @examples
#' pareto_chart(lm(mpg ~ wt + hp, data=mtcars))
pareto_chart <- function(...) {
  UseMethod("pareto_chart")
}


#' Pareto's chart
#'
#' Create a Pareto chart for a data frame.
#'
#' @param data a data frame
#' @param labels the column with the labels of the data frame
#' @param values the column with the values of the data frame
#'
#' @return a Pareto chart of the data frame
#' @export
#' @returns Invisibly returns a data frame with the absolute values of the
#' data frame, their sign, and the cumulative value.
#'
#' @examples
#' pareto_chart(mtcars)
pareto_chart.default <- function(data, labels, values) {
  stopifnot(is.data.frame(data))
  df <- data |>
    dplyr::mutate(
      sign=ifelse({{values}}<0, "negative", "positive"),
      effect = abs({{values}}),
    ) |>
    dplyr::arrange(dplyr::desc(effect)) |>
    dplyr::mutate(
      cum = cumsum(effect),
      labels = factor({{labels}}, levels={{labels}}, ordered=TRUE)
    )

  df |>
    ggplot2::ggplot(ggplot2::aes(x=labels, group=1)) +
    ggplot2::geom_col(ggplot2::aes(y=effect, fill=sign)) +
    ggplot2::geom_line(ggplot2::aes(y=cum)) +
    ggplot2::geom_point(ggplot2::aes(y=cum)) +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::sec_axis(
        \(x) scales::rescale(x, from=c(0, max(df$cum)), to=c(0, 100)),
        name="relative contribution (%)",
        breaks=seq(0, 100, 10)
      )
    )
}

#' Pareto's chart
#'
#' Creates a Pareto chart for the effects of a linear model.
#'
#' @param model a linear model
#'
#' @return a Pareto chart of the effects of the model
#' @export
#' @returns Invisibly returns a data frame with the absolute effects of the
#' model, their sign, and the cumulative effect.
#'
#' @examples
#' pareto_chart(lm(mpg ~ wt + hp, data=mtcars))
pareto_chart.lm <- function(model) {
  tibble::tibble(
    effect = 2*coef(model),
    factor=names(effect)
  ) |>
    dplyr::filter(factor != "(Intercept)") |>
    pareto_chart(labels=factor, values=effect)
}


#' Normal probability plot
#'
#' @param data a data frame
#' @param var the variable to plot (`data` column)
#' @param breaks the breaks for the y-axis
#' @param linecolor the color of the normal probability line
#'
#' @return a normal probability plot (ggplot2)
#' @export
#'
#' @examples
#' normplot(mtcars, mpg)
normplot <- function(data, var, breaks=seq(0.1, 0.9, 0.1), linecolor="red") {
  m <- data |> dplyr::pull({{var}}) |> mean()
  s <- data |> dplyr::pull({{var}}) |> sd()

  data |>
    dplyr::mutate(ecdf = ecdf({{var}})({{var}})) |>
    dplyr::arrange({{var}}) |>
    ggplot2::ggplot(ggplot2::aes(x={{var}}, y=ecdf)) +
    ggplot2::geom_point() +
    ggplot2::geom_function(fun = pnorm, args=list(mean=m, sd=s), color=linecolor) +
    ggplot2::scale_y_continuous(
      trans=scales::probability_trans("norm"),
      breaks=breaks) +
    ggplot2::labs(y="Normal probability")
}
