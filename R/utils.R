require(glue)


#' examples_url
#' provides the URL for the desired example data
#'
#' @param example data file name
#'
#' @return the full URL for the desired example
#' @export
#'
#' @examples
#' examples_url("train.csv")
examples_url <- function(example) {
  url = paste0("https://paolobosetti.quarto.pub/data/", example)
  return(url)
}


#' Daniel's plot
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


#' Pareto's chart
#'
#' @param model a linear model
#'
#' @return a Pareto chart of the effects of the model
#' @export
#'
#' @examples
#' pareto_chart(lm(mpg ~ wt + hp, data=mtcars))
pareto_chart <- function(model) {
  df <- tibble(
    effect = 2*coef(model),
    factor=names(effect),
    sign=sign(effect)
  ) %>%
    filter(factor != "(Intercept)") %>%
    mutate(effect=abs(effect)) %>%
    relocate(factor, effect, sign) %>%
    arrange(desc(effect)) %>%
    mutate(
      cum=cumsum(effect),
      factor=factor(factor, ordered=T, levels=factor)
    )

  p <- df %>%
    mutate(sign=ifelse(sign<0, "negative", "positive")) %>%
    ggplot(aes(x=factor, group=1)) +
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
  print(p)
  invisible(df)
}
