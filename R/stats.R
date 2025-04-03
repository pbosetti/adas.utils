#' Chauvenet's criterion
#'
#' Applies the Chauvenet's criterion to a sample, identifying a possible
#' outlier.
#'
#' @param x the sample vector.
#' @param threshold the threshold for the frequency of the suspect outlier.
#'
#' @return an object of class `chauvenet` with the following components:
#' \describe{
#'  \item{`sample`}{the name of the sample}
#'  \item{`s0`}{the maximum difference}
#'  \item{`index`}{the index of the suspect outlier}
#'  \item{`value`}{the value of the suspect outlier}
#'  \item{`expected`}{the expected frequency of the suspect outlier}
#'  \item{`reject`}{a logical value indicating whether the suspect outlier should be rejected}
#' }
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
    sample = deparse(substitute(x)),
    s0 = s0,
    index = i0,
    value = x[i0],
    expected = freq,
    reject = freq < threshold
  )
  class(result) <- "chauvenet"
  return(result)
}


#' Print result of Chauvenet's test
#'
#' @param x a Chauvenet object
#' @param ... further arguments currently ignored
#'
#' @return No return value, just prints the result
#' @export
#' @noRd
#'
print.chauvenet <- function(x, ...) {
  print(glue("Chauvenet's criterion for sample {x$sample}"))
  print(glue("Suspect outlier: {x$index}, value {x$value}"))
  print(glue("Expected frequency: {x$expected}, threshold: {x$threshold}"))
  print(glue("Decision: {d}", d=ifelse(x$reject, "reject it", "keep it")))
}


#' Daniel's plot (quantile-quantile)
#'
#' Given a non-replicated model of a factorial plan, this function provides a
#' QQ plot of the effects of the model, labeling all the effects.
#'
#' @param model a linear model
#' @param alpha the transparency of the horizontal lines
#' @param xlim the limits of the x-axis
#'
#' @return a QQ plot (GGPlot2 object) with the effects of the model
#' @export
#'
#' @examples
#' daniel_plot_qq(lm(Y~A*B*C*D, data=filtration))
daniel_plot_qq <- function(model, alpha=0.5, xlim=c(-3,3)) {
  value <- term <- NULL
  e <- effects(model)
  tibble(
    term = names(e),
    value = as.numeric(e)
  ) %>%
    slice_tail(n=length(e) - 1) %>%
    ggplot(aes(sample=value)) +
    stat_qq() +
    geom_qq_line(color="red") +
    geom_hline(aes(yintercept=value), alpha=alpha) +
    geom_label(aes(y=value, x=xlim[1], label=term), hjust="left") +
    coord_cartesian() +
    labs(x="Theoretical quantiles", y="Sample quantiles")
}


#' Daniel's plot (half-normal)
#'
#' Given a non-replicated model of a factorial plan, this function provides a
#' half-normal plot of the effects of the model, labeling the main n effects.
#'
#' @param model a linear model
#' @param ... further arguments to [gghalfnorm::gghalfnorm()]
#'
#' @return a half-normal plot (GGPlot2 object) with the effects of the model
#' @export
#' @seealso [gghalfnorm::gghalfnorm()]
#'
#' @examples
#' daniel_plot_hn(lm(Y~A*B*C*D, data=filtration))
daniel_plot_hn <- function(model, ...) {
  . <- NULL
  effects(model) %>%
    tail(-1) %>%
    gghalfnorm(labs=names(.),...) +
    labs(y="Theoretical quantiles", x="Sample quantiles")
}


#' Pareto's chart
#'
#' This is a generic function for Pareto's chart.
#'
#' @param obj an object
#' @param ... further parameters to specialized functions
#'
#' @return a Pareto chart of the effects of the model
#' @export
#'
#' @seealso [pareto_chart.data.frame()] [pareto_chart.lm()]
#'
#' @examples
#' # For a data frame:
#' library(tibble)
#' set.seed(1)
#' tibble(
#'   val=rnorm(10, sd=5),
#'   cat=LETTERS[1:length(val)]
#'   ) %>%
#'   pareto_chart(labels=cat, values=val)
#'
#' # For a linear model:
#' pareto_chart(lm(Y~A*B*C*D, data=filtration))
pareto_chart <- function(obj, ...) {
  UseMethod("pareto_chart")
}


#' Pareto's chart
#'
#' Create a Pareto chart for a data frame.
#'
#' @param obj a data frame
#' @param labels the column with the labels of the data frame
#' @param values the column with the values of the data frame
#' @param ... further parameters (currently unused)
#'
#' @return a Pareto chart (GGPlot2 object) of the data frame
#' @export
#' @returns Invisibly returns a data frame with the absolute values of the
#' data frame, their sign, and the cumulative value.
#'
#' @examples
#' library(tibble)
#' set.seed(1)
#' tibble(
#'   val=rnorm(10, sd=5),
#'   cat=LETTERS[1:length(val)]
#'   ) %>%
#'   pareto_chart(labels=cat, values=val)
pareto_chart.data.frame <- function(obj, labels, values, ...) {
  cum <- effect <- NULL
  stopifnot(is.data.frame(obj))
  df <- obj %>%
    mutate(
      sign=ifelse({{values}}<0, "negative", "positive"),
      effect = abs({{values}}),
    ) %>%
    arrange(desc(effect)) %>%
    mutate(
      cum = cumsum(effect),
      labels = factor({{labels}}, levels={{labels}}, ordered=TRUE)
    )

  df %>%
    ggplot(aes(x=labels, group=1)) +
    geom_col(aes(y=effect, fill=sign)) +
    geom_line(aes(y=cum)) +
    geom_point(aes(y=cum)) +
    scale_y_continuous(
      sec.axis = sec_axis(
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
#' @param obj a linear model
#' @param ... further parameters (currently unused)
#'
#' @return a Pareto chart (GGPlot2 object) of the effects of the model
#' @export
#' @returns Invisibly returns a data frame with the absolute effects of the
#' model, their sign, and the cumulative effect.
#'
#' @examples
#' pareto_chart(lm(Y~A*B*C*D, data=filtration))
pareto_chart.lm <- function(obj, ...) {
  effect <- NULL
  tibble(
    effect = 2*coef(obj),
    factor=names(effect)
  ) %>%
    na.omit() %>%
    filter(factor != "(Intercept)") %>%
    pareto_chart(labels=factor, values=effect, ...)
}


#' Normal probability plot
#'
#' @param data a data frame
#' @param var the variable to plot (`data` column)
#' @param breaks the breaks for the y-axis
#' @param linecolor the color of the normal probability line
#'
#' @return a normal probability plot (GGPlot2 object)
#' @export
#'
#' @examples
#' library(tibble)
#' df <- tibble(
#'   xn = rnorm(100, mean=20, sd=5),
#'   xu = runif(100, min=0, max=40)
#' )
#'
#' df %>% normplot(xn)
#' df %>% normplot(xu)
normplot <- function(data, var, breaks=seq(0.1, 0.9, 0.1), linecolor="red") {
  m <- data %>% pull({{var}}) %>% mean()
  s <- data %>% pull({{var}}) %>% sd()

  data %>%
    mutate(ecdf = ecdf({{var}})({{var}})) %>%
    arrange({{var}}) %>%
    ggplot(aes(x={{var}}, y=ecdf)) +
    geom_point() +
    geom_function(fun = pnorm, args=list(mean=m, sd=s), color=linecolor) +
    scale_y_continuous(
      trans=scales::probability_trans("norm"),
      breaks=breaks) +
    labs(y="Normal probability")
}


#' ggTukey
#'
#' This is a generic function for plotting Tukey's HSD test via GGplot2.
#'
#' @param obj an object to plot: either a TukeyHSD object or a data frame
#' @param ... Other parameters passed to the specialized functions
#'
#' @returns a GGPlot2 object
#' @export
#'
ggTukey <- function(obj, ...){
  UseMethod("ggTukey")
}

#' ggTukey for TukeyHSD
#'
#' Plot Tukey's HSD test via GGplot2.
#'
#' @param obj a TukeyHSD object
#' @param which the index of the comparison. Used when the formula in the
#'              undelying aov call has more than one term.
#' @param ... further parameters (currently unused)
#'
#' @returns a GGPlot2 object
#' @export
#' @seealso [TukeyHSD()] [ggTukey.data.frame()] [ggTukey.TukeyHSD()]]]
#'
#' @examples
#' library(tidyverse)
#' cotton %>%
#'   aov(Strength~Cotton, data=.) %>%
#'   TukeyHSD() %>%
#'   ggTukey()
ggTukey.TukeyHSD <- function(obj, which=1, ...){
  id <- lwr <- upr <- diff <- NULL
  nm <- names(obj)[[which]]
  cl <- attr(obj, "conf.level")
  obj[[which]] %>%
    fortify() %>%
    rownames_to_column("id") %>%
    rename(min=lwr, max=upr) %>%
    ggplot(aes(x=id)) +
    geom_hline(yintercept=0, color="red") +
    geom_point(aes(y=diff)) +
    geom_errorbar(aes(ymin=min, ymax=max), width=0.2) +
    labs(
      y="Difference",
      x=nm,
      subtitle=paste0(cl * 100, "% family-wise confidence level")
    ) +
    coord_flip()
}


#' ggTukey for data.frame
#'
#' @param obj a data frame
#' @param formula a formula to be used in the aov call
#' @param which the index of the comparison. Used when the formula in the
#'              undelying aov call has more than one term.
#' @param splt a formula to split the data frame
#' @param ... further parameters passed to `TukeyHSD` (e.g. `conf.level`)
#'
#' @returns a GGPlot2 object
#' @export
#'
#' @examples
#' library(tidyverse)
#' battery %>%
#'   ggTukey(Response~Material, splt=~Temperature, conf.level=0.99)
ggTukey.data.frame <- function(obj, formula, which=1, splt=NULL, ...) {
  id <- lwr <- upr <- diff <- `p adj` <- NULL
  if (is.null(splt)) {
    TukeyHSD(aov(formula, data=obj), ...) %>%
      ggTukey(which)
  } else {
    grp <- (splt %>% terms() %>% attr("term.labels"))[[1]]
    var <- (formula %>% terms() %>% attr("term.labels"))[[1]]
    cl <- ifelse("conf.level" %in% ...names(), list(...)$conf.level, 0.95)
    split(obj, splt) %>%
      map(\(d) {
        TukeyHSD(aov(formula, data = d), ...)[[which]] %>%
          as_tibble(rownames=var) %>%
          select(-`p adj`) %>%
          mutate("{grp}":=unique(pull(d, !!grp)))
      }) %>%
      bind_rows() %>%
      ggplot(aes(x=!!sym(var), y=diff, color=!!sym(grp))) +
      geom_hline(yintercept=0, color="red") +
      geom_point(position = position_dodge(0.5)) +
      geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.1,
                    position = position_dodge(0.5)) +
      labs(
        y="Difference",
        x=var,
        subtitle=paste0(cl * 100, "% family-wise confidence level")
      ) +
      coord_flip()
  }
}
