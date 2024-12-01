require(glue)
require(magrittr)
require(purrr)

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
#' daniel_plot(lm(Y~A*B*C*D, data=filtration))
daniel_plot <- function(model, alpha=0.5, xlim=c(-3,3)) {
  e <- effects(model)
  tibble::tibble(
    term = names(e),
    value = as.numeric(e)
  ) %>%
    dplyr::slice_tail(n=length(e) - 1) %>%
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
#' @seealso [pareto_chart.default()] [pareto_chart.lm()]
#'
#' @examples
#' # For a data frame:
#' library(tidyverse)
#' set.seed(1)
#' tibble(
#'   val=rnorm(10, sd=5),
#'   cat=LETTERS[1:length(val)]
#'   ) %>%
#'   pareto_chart(labels=cat, values=val)
#'
#' # For a linear model:
#' pareto_chart(lm(Y~A*B*C*D, data=filtration))
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
#' library(tidyverse)
#' set.seed(1)
#' tibble(
#'   val=rnorm(10, sd=5),
#'   cat=LETTERS[1:length(val)]
#'   ) %>%
#'   pareto_chart(labels=cat, values=val)
pareto_chart.default <- function(data, labels, values) {
  stopifnot(is.data.frame(data))
  df <- data %>%
    dplyr::mutate(
      sign=ifelse({{values}}<0, "negative", "positive"),
      effect = abs({{values}}),
    ) %>%
    dplyr::arrange(dplyr::desc(effect)) %>%
    dplyr::mutate(
      cum = cumsum(effect),
      labels = factor({{labels}}, levels={{labels}}, ordered=TRUE)
    )

  df %>%
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
#' pareto_chart(lm(Y~A*B*C*D, data=filtration))
pareto_chart.lm <- function(model) {
  tibble::tibble(
    effect = 2*coef(model),
    factor=names(effect)
  ) %>%
    na.omit() %>%
    dplyr::filter(factor != "(Intercept)") %>%
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
#' library(tidyverse)
#' df <- tibble(
#'   xn = rnorm(100, mean=20, sd=5),
#'   xu = runif(100, min=0, max=40)
#' )
#'
#' df %>% normplot(xn)
#' df %>% normplot(xu)
normplot <- function(data, var, breaks=seq(0.1, 0.9, 0.1), linecolor="red") {
  m <- data %>% dplyr::pull({{var}}) %>% mean()
  s <- data %>% dplyr::pull({{var}}) %>% sd()

  data %>%
    dplyr::mutate(ecdf = ecdf({{var}})({{var}})) %>%
    dplyr::arrange({{var}}) %>%
    ggplot2::ggplot(ggplot2::aes(x={{var}}, y=ecdf)) +
    ggplot2::geom_point() +
    ggplot2::geom_function(fun = pnorm, args=list(mean=m, sd=s), color=linecolor) +
    ggplot2::scale_y_continuous(
      trans=scales::probability_trans("norm"),
      breaks=breaks) +
    ggplot2::labs(y="Normal probability")
}


#' Generate the design matrix for a factorial experiment
#'
#' The function produces a design matrix for a factorial experiment. The
#' function takes the number of factors, the number of replications, and the
#' levels of the factors as arguments. The function returns a tibble with the
#' design matrix. The design matrix also has the `StdOrder` and `RunOrder`
#' columns, which are the standard order and the random order of the runs,
#' respectively. Also, a NA column `Y` is added for the response variable.
#'
#' @param n_factors the number of factors (a scalar)
#' @param rep the number of replications (a scalar)
#' @param levels the levels of the factors (a vector)
#'
#' @return a design matrix (as a tibble). The design matrix also has the
#' `StdOrder` and `RunOrder` columns, which are the standard order and the
#' random order of the runs, respectively. Also, a NA column `Y` is added for
#' the response variable.
#'
#' @export
#'
#' @examples
#' # A 3^3 factorial plan, teplicated 2 times:
#' design_matrix(3, rep=2, levels=-1:1)
design_matrix <- function(n_factors, rep = 1, levels = c(-1,1)) {
  LETTERS[1:n_factors] %>%
    purrr::set_names(.) %>%
    purrr::map(~ levels) %>%
    purrr::list_merge(rep=1:rep) %>%
    do.call(what=expand.grid, args=.) %>%
    dplyr::mutate(
      StdOrder = 1:dplyr::n(),
      RunOrder = sample(dplyr::n()),
      .before = A
    ) %>%
    dplyr::relocate(rep, .before=A) %>%
    dplyr::mutate(Y=NA)
}
