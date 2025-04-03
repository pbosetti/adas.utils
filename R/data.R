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

#' Battery experiment data
#'
#' Battery life in hour of a factorial experiment with 2 factors and 3 levels
#' each. Factors are:
#'
#' * `Temperature`: of the battery during the discharge experiment
#' * `Material`: Plate material for the battery
#'
#' Other columns are:
#'
#' * `StandardOrder`: Yate's standard order
#' * `RunOrder`: randomized order, in which tests have been executed
#' * `Repeat`: repeat number
#' * `Response`: battery life in hours
#'
#' @format A data frame with 36 rows and 6 columns
#' @references Douglas C. Montgomery, "Design and Analysis of Experiments", 8th edition, Wiley, 2019
"battery"

#' Cotton yarn experiment data
#'
#' Yarn tensile strength in a completely randomized experiment with 5 different
#' levels of cotton fiber.
#'
#' @format A data frame with 25 rows and 3 columns. Columns represent:
#'
#'    * `Run`: run order
#'    * `Cotton`: cotton content in mass percentage
#'    * `Strength`: yarn tensile strength in N
#'
#' @references Douglas C. Montgomery, "Design and Analysis of Experiments", 8th edition, Wiley, 2019
'cotton'
