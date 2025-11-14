# Imported with modifications from https://github.com/tidyverse/modelr GPL3.0

response_var <- function(model, data) {
  UseMethod("response_var")
}

#' @export
response_var.default <- function(model, data) {
  stats::formula(model)[[2L]]
}

predictor_vars <- function(model, data) {
  UseMethod("predictor_vars")
}

#' @export
predictor_vars.default <- function(model, data) {
  all.vars(stats::formula(model)[[3L]])
}

response <- function(model, data) {
  eval(response_var(model), as.data.frame(data))
}

#' Add residuals to a data frame
#'
#' @param data A data frame used to generate the residuals
#' @param model,var `add_residuals` takes a single `model`; the
#'   output column will be called `resid`
#' @param ... `gather_residuals` and `spread_residuals` take
#'   multiple models. The name will be taken from either the argument
#'   name of the name of the model.
#' @param .resid,.model The variable names used by `gather_residuals`.
#' @return A data frame. `add_residuals` adds a single new column,
#'   `.resid`, to the input `data`. `spread_residuals` adds
#'   one column for each model. `gather_predictions` adds two columns
#'   `.model` and `.resid`, and repeats the input rows for
#'   each model.
#' @export
#' @examples
#' df <- tibble::tibble(
#'   x = sort(runif(100)),
#'   y = 5 * x + 0.5 * x ^ 2 + 3 + rnorm(length(x))
#' )
#' plot(df)
#'
#' m1 <- lm(y ~ x, data = df)
#' df %>% add_residuals(m1)
#'
#' m2 <- lm(y ~ poly(x, 2), data = df)
#' df %>% spread_residuals(m1, m2)
#' df %>% gather_residuals(m1, m2)
add_residuals <- function(data, model, var = "resid") {
  data[[var]] <- residuals(model, data)
  data
}

#' @rdname add_residuals
#' @export
spread_residuals <- function(data, ...) {
  models <- tibble::lst(...)
  for (nm in names(models)) {
    data[[nm]] <- residuals(models[[nm]], data)
  }
  data
}

#' @rdname add_residuals
#' @export
gather_residuals <- function(data, ..., .resid = "resid", .model = "model") {
  models <- tibble::lst(...)

  df <- purrr::map2(models, .resid, add_residuals, data = data)
  names(df) <- names(models)

  vctrs::vec_rbind(!!!df, .names_to = .model)
}

residuals <- function(model, data) {
  response(model, data) - stats::predict(model, data)
}
