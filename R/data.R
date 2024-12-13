#' Filtration data
#'
#' Non-replicated factorial plan for a slurry filtration process.
#'
#' @format Factors are:
#' * A: Temperature
#' * B: Pressure
#' * C: Concentration of solid phase
#' * D: Agitation speed
#'
#' The yield is in column Y and represents the filtration speed
#'
#'
"filtration"


#' Central Composite Design Experiment Yields
#'
#' Yield data for a two factor CCD experiment
#'
#' @format A list with three vectors:
#' * `base`: the yield for a 2^2 factorial design, replicated 3 times
#' * `center`: the yield for the center points, replicated 4 times
#' * `axial`: the yield for the axial points, replicated 2 times
#'
#'
#'
"ccd_experiment_yield"
