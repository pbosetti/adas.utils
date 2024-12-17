
#' Factorial Plan Defining Relationship
#'
#' Builds a formula from a number of factors
#'
#' @param arg If it is a formula, it is returned verbatim. If it is a number, a
#' formula is built with the number of factors. If it is neither a formula nor a
#' number, an error is thrown.
#'
#' @return A formula.
#' @noRd
#'
#' @examples
#' # Defining relationships with three factors
#' fp_defrel(3)
#'
#' # Defining relationship I=ABC
#' fp_defrel(~A*B*C)
fp_defrel <- function(arg) {
  if (is.numeric(arg))
    formula <- as.formula(paste0("~", paste0(LETTERS[1:arg], collapse="*")))
  else if (is_formula(arg))
    formula <- arg
  else
    stop("Argument must be either a formula or the number of factors")
  return(formula)
}



#' Factorial Plan List of Treatments
#'
#' Builds a list of treatments from a formula, or from a number of factors.
#'
#' Defining relationships are represented as one side formulas, e.g. $I=ABC$
#' becomes `~A*B*C`.
#'
#' @param arg Either a formula or a number of factors. If it is a formula, the
#'   factors are extracted from it. If it is a number, the factors are the first
#'   `n` capital letters.
#'
#' @return A list of treatments (character vector).
#' @export
#'
#' @examples
#' fp_treatments(3)
fp_treatments <- function(arg) {
  formula <- fp_defrel(arg)
  treat <- c(
    "(1)",
    fp_effect_names(formula) %>%
      as.character() %>%
      stringr::str_to_lower() %>%
      stringr::str_remove_all(":")
  )
  return(treat)
}

#' Factorial Plan Design Matrix
#'
#' Builds a design matrix from a one side formula or a number of factors.
#'
#' Defining relationships are represented as one side formulas, e.g. $I=ABC$
#' becomes `~A*B*C`.
#'
#' @param arg Either a formula or a number of factors. If it is a formula, the
#'   factors are extracted from it. If it is a number, the factors are the first
#'   `n` capital letters.
#' @param rep Number of replications.
#' @param levels Levels of the factors.
#'
#' @return A design matrix: a subclass of a tibble of class `factorial.plan`.
#'   The class has the following attributes:
#'   \describe{
#'      \item{`def.rel`}{The defining relationship (a formula).}
#'      \item{`generators`}{The list of generators (formulas) if the factorial plan is fractional.}
#'      \item{`fraction`}{The list of fractions (character vectors) if the factorial plan is fractional.}
#'      \item{`levels`}{The levels of the factors (all equal), in coded units.}
#'      \item{`scales`}{A list: for each factor, a vector of two values corresponding to the extreme values in coded units.}
#'    }
#'
#' @export
#'
#' @examples
#' fp_design_matrix(3, rep=2, levels=c("-", "+"))
fp_design_matrix <- function(arg, rep = 1, levels = c(-1,1)) {
  . <- RunOrder <- any_of <- .rep <- NULL
  if (is_formula(arg))
    fct <- terms(arg) %>% attr("variables") %>% as.character() %>% tail(-1)
  else if (is.numeric(arg))
    fct <- LETTERS[1:arg]
  else
    stop("Argument must be either a formula or an integer")
  dm <- fct %>%
    purrr::set_names(.) %>%
    map(~ levels) %>%
    list_merge(.rep=1:rep) %>%
    do.call(what=expand.grid, args=.) %>%
    as_tibble() %>%
    mutate(
      StdOrder = 1:n(),
      RunOrder = sample(n()),
      # .before = .data[[fct[1]]]
      .before = any_of(fct[1])
    ) %>%
    relocate(.rep, .before=any_of(fct[1])) %>%
    mutate(Y=NA)
  if (length(levels) == 2) dm <- mutate(dm, .treat= rep(fp_treatments(arg), rep), .after=RunOrder)
  class(dm) <- c("factorial.plan", class(dm))
  attr(dm, "def.rel") <- fp_defrel(arg)
  attr(dm, "generators") <- NA
  attr(dm, "factors") <- fct
  attr(dm, "fraction") <- NA
  attr(dm, "levels") <- levels
  attr(dm, "type") <- "plain"
  attr(dm, "scales") <- list()
  return(dm)
}


#' Scale factors levels
#'
#' This function allows to add columns to a design matrix with scaled factor,
#' i.e. factors reported in real units rather in coded units (e.g. -1, 1).
#'
#' @param dm the design matrix to scale.
#' @param ... a set of factors to scale, with their respective ranges, e.g.
#' `A=c(10, 30), B=c(0, 1)`.if the range is not a two-number vector or the
#' factor is not numeric, a warning is printed and the factor is skipped.
#' @param suffix the suffix to add to the scaled factor name in creating new
#' columns. If the suffix is the empty string, factors are replaced.
#'
#' @return the design matrix with the scaled factors.
#' @export
#'
#' @examples
#' fp_design_matrix(3, rep=2) %>%
#'   fp_add_scale(A=c(10, 30), B=c(0, 1), suffix=".scaled")
fp_add_scale <- function(dm, ..., suffix="_s") {
  `:=` <- Y <- last_col <- NULL
  for (i in 1:...length()) {
    name <- ...names()[i]
    rng <- ...elt(i)
    if (!(is.numeric(rng) & length(rng) == 2 & is.numeric(dm[[name]]))) {
      warning("Skipping factor ", name, " (it is not a number, or wrong scale range/type provided)\n")
      next
    }
    dm <- dm %>%
      mutate(
        !!paste0(name, suffix) := scales::rescale(!!sym(name), to=rng)
      )
    attr(dm, "scales") <- append(attr(dm, "scales"), setNames(list(rng), name))
  }
  dm <- dm %>%
    relocate(Y, .after=last_col())
  return(dm)
}


#' Add factor names to a design matrix
#'
#' Store factor names in the `factorial.plan` object, as a list within the
#' `factor.names` attribute.
#'
#' @param dm the design matrix.
#' @param ... a set of factors to name, with their respective names, e.g.
#' `A="Temperature", B="Pressure"`. If the factor is not in the design matrix
#' factors list, a warning is printed and the factor is skipped.
#'
#' @return the design matrix with the named factors.
#' @export
#'
#' @examples
#' fp_design_matrix(3, rep=2) %>%
#'   fp_add_names(A="Temperature", B="Pressure")
fp_add_names <- function(dm, ...) {
  attr(dm, "factor.names") <- list()
  for (i in 1:...length()) {
    factor <- ...names()[i]
    name <- ...elt(i)
    if (!(is.character(name) & factor %in% attr(dm, "factors"))) {
      warning("Skipping factor ", name, " (not in factors list)\n")
      next
    }
    attr(dm, "factor.names") <- append(attr(dm, "factor.names"), setNames(list(name), factor))
  }
  return(dm)
}


#' Factorial plan info
#'
#' Print information about the factorial plan.
#'
#' @param x the factorial plan.
#' @param file the file to write the information to. Use console if empty.
#' @param comment a comment mark to add before each line of the information.
#'
#' @return No return value, just prints the fp information.
#'
#' @export
#'
#' @examples
#' fp_design_matrix(3, rep=2) %>%
#'   fp_info()
fp_info <- function(x, file="", comment="") {
  cat(comment, "Factorial Plan Design Matrix\n", file=file)
  cat(comment, "Defining Relationship: ", as.character(attr(x, "def.rel")), "\n", file=file, append=TRUE)
  cat(comment, "Factors: ", attr(x, "factors"), "\n", file=file, append=TRUE)
  cat(comment, "Levels: ", attr(x, "levels"), "\n", file=file, append=TRUE)
  cat(comment, "Fraction: ", attr(x, "fraction"), "\n", file=file, append=TRUE)
  cat(comment, "Type: ", attr(x, "type"), "\n", file=file, append=TRUE)
  if (attr(x, "scales") %>% length() > 0) {
    cat(comment, "Scaled factors:\n", file=file, append=TRUE)
    iwalk(attr(x, "scales"), \(.x, .y) {
      cat(comment, "  ", glue::glue("{.y}: [{.x[1]}, {.x[2]}]\n\n"), file=file, append=TRUE)
    })
  }
  if (attr(x, "factor.names") %>% length() > 0) {
    cat(comment, "Factor names:\n", file=file, append=TRUE)
    iwalk(attr(x, "factor.names"), \(.x, .y) {
      cat(comment, "  ",  glue::glue("{.y}: {.x}\n\n"), file=file, append=TRUE)
    })
  }
  cat(comment, "\n", file=file, append=TRUE)
}


#' Print a factorial plan design matrix
#'
#' @param x the matrix.
#' @param ... other parameters passed to print().
#'
#' @return No return value, just prints the factorial plan.
#'
#' @export
#' @noRd
print.factorial.plan <- function(x, ...) {
  class(x) <- Filter(function(x) x!="factorial.plan", class(x))
  fp_info(x)
  print(x, ...)
}


#' Save a design matrix to a CSV file
#'
#' Writes the design matrix to a CSV file, with a timestamp and comment lines.
#'
#' Note that the design matrix is saved in the same order of the `RunOrder`
#' column, i.e. random.
#'
#' @param dm the design matrix.
#' @param file the file to write the design matrix to.
#' @param comment a comment mark to add before each line of the information.
#' @param timestamp whether to add a timestamp to the file.
#' @param ... other parameters passed to write_csv().
#' @param type the CSV version (1 or 2).
#'
#' @return Invisibly return the design matrix, unchanged, for further piping.
#' @export
#'
fp_write_csv <- function(dm, file, comment="# ", timestamp=TRUE, type=c(1,2), ...) {
  RunOrder <- NULL
  sf <- lubridate::stamp("Created 20/01/2024 03:34:10", quiet=TRUE)
  fp_info(dm, file=file, comment=comment)
  if (timestamp)
    cat(comment, sf(Sys.time()), "\n", file=file, append=TRUE)
  if (type[1] == 1)
    dm %>%
      arrange(RunOrder) %>%
      write_csv(file, append=TRUE, col_names = TRUE, ...)
  else if (type[1] == 2)
    dm %>%
      arrange(RunOrder) %>%
      write_csv2(file, append=TRUE, col_names = TRUE, ...)
  else
    stop("Invalid CSV type")
  invisible(dm)
}


#' Load a design matrix from a CSV file
#'
#' Load from a CSV file the design matrix that has previously been saved with
#' `fp_write_csv()`. It is an error if the loaded data frame has different
#' dimensions or column names than the original design matrix.
#'
#' Note that the design matrix is sorted by the `StdOrder` column after loading.
#'
#' @param dm the design matrix.
#' @param file the file to read the design matrix from.
#' @param type the CSV version (1 or 2).
#' @param yield the yield column name.
#' @param comment the comment mark.
#'
#' @return the design matrix with the new values.
#' @export
#' @seealso [fp_write_csv()]
fp_read_csv <- function(dm, file, type=c(1,2), yield="Y", comment="#") {
  StdOrder <- NULL
  if (type[1] == 1) {
    n <- read_csv(file, col_names = TRUE, comment=comment, show_col_types = FALSE) %>%
      arrange(StdOrder)
  } else if (type[1] == 2) {
    n <- read_csv2(file, col_names = TRUE, comment=comment, show_col_types = FALSE) %>%
      arrange(StdOrder)
  } else {
    stop("Invalid type")
  }
  if (!all(names(n) %in% names(dm))) {
    cat(names(n), names(dm))
    stop("Column names do not match")
  }
  if(!all(dim(n) == dim(dm))) {
    stop("Table dimensions do not match")
  }
  dm[yield] <- n[yield]
  return(dm)
}



#' Factorial Plan effect names from a formula
#'
#' Returns the effect names from a formula, according to Yates' convention.
#'
#' Defining relationships are represented as one side formulas, e.g. $I=ABC$
#' becomes `~A*B*C`.
#'
#' @param arg A formula.
#'
#' @return An ordered factor with the effect names.
#' @export
#'
#' @examples
#' fp_effect_names(~A*B*C)
fp_effect_names <- function(arg) {
  . <- p <- w <- effect <- NULL
  formula <- fp_defrel(arg)
  terms(formula) %>%
    attr("factors") %>%
    as_tibble() %>%  {
      bind_rows(.,
                mutate(., p = 0:(n() - 1), w = 2 ^ p) %>%
                  mutate(across(-(p:w), ~ .x * w)) %>%
                  select(-(p:w)) %>%
                  summarize_all(sum))
    } %>% {
      tibble(
        effect = colnames(.) %>% str_remove_all(":"),
        order = tail(., 1) %>% as.numeric()
      )
    } %>%
    arrange(order) %>%
    pull(effect) %>% factor(., ordered = T, levels = .)
}


#' Check for Factor Aliases
#'
#'  Checks if two factors are aliased in a formula. This function is
#'  mostly used internally to build the alias matrix.
#'
#' Defining relationships are represented as one side formulas, e.g. $I=ABC$
#' becomes `~A*B*C`.
#'
#' @param arg A formula for the defining relationship.
#' @param A a string representing an effect (e.g. `"AB"`).
#' @param B a string representing an effect (e.g. `"CD"`).
#'
#' @return A logical value.
#' @noRd
#'
#' @seealso [fp_defrel()] [fp_alias()] [fp_alias_list()] [fp_gen2alias()]
#' @examples
#' fp_has_alias(~A*B*C*D, "AB", "CD")
fp_has_alias <- function(arg, A, B) {
  formula <- fp_defrel(arg)
  m <- formula %>% terms() %>% attr("factors") %>% as_tibble() %>%
    rename_with(~ str_remove_all(., ":"))
  all(xor(m[A], m[B]))
}

#' Build an alias table from a formula
#'
#' Given a Defining relationship or a number of factors, this function builds
#' a matrix of aliases.
#'
#' Defining relationships are represented as one side formulas, e.g. $I=ABC$
#' becomes `~A*B*C`.
#'
#' @param arg A formula or a number of factors.
#'
#' @return A matrix of logical values.
#' @noRd
#' @examples
#' fp_alias(~A*B*C*D)
#' fp_alias(3)
fp_alias <- function(arg) {
  formula <- fp_defrel(arg)
  nm <- fp_effect_names(formula)
  m <- matrix(NA, nrow=length(nm), ncol=length(nm))
  colnames(m) <- nm
  rownames(m) <- nm
  for (r in nm) {
    for (c in nm) {
      m[r, c] <- fp_has_alias(formula, r, c)
    }
  }
  return(m)
}

#' Given a generator, find the alias
#'
#' Given a generator and an effect, this function returns the alias.
#'
#' Generators and aliases are strings of capital letters.
#'
#' @param generator a generator, in the form of `ABCD...`.
#' @param effect an effect, in the form of `BD...`.
#'
#' @return An effect (string).
#' @export
#'
#' @examples
#' fp_gen2alias("ABCD", "BD")
fp_gen2alias <- function(generator, effect) {
  . <- NULL
  paste0(generator, effect) %>%
    strsplit(split = "") %>%
    unlist() %>%
    table() %>%
    `[`(.==1) %>%
    names() %>%
    sort() %>%
    paste0(collapse="")
}



#' List All Alias for a Fractional Factorial Plan
#'
#' Given a defining relationship, as a one side firmula,, this function lists
#' all the aliases for a fractional factorial plan.
#'
#' Defining relationships are represented as one side formulas, e.g. $I=ABC$
#' becomes `~A*B*C`.
#'
#' @param arg A formula for the defining relationship, or the number of factors.
#'
#' @return a list of aliases (as formulas).
#' @noRd
#'
#' @examples
#' fp_alias_list(~A*B*C*D)
fp_alias_list <- function(arg) {
  effect <- . <- NULL
  formula <- fp_defrel(arg)
  m <- fp_alias(formula) %>% as_tibble(rownames="effect")
  f <- m %>% pull(effect) %>% purrr::set_names()
  f %>%
    purrr::map(~ filter(m, .data[[.]]) %>% pull(effect))
}




#' Reduce a Factorial Plan by 1/2 Fraction
#'
#' @param dm A factorial plan table.
#' @param formula A formula for the defining relationship.
#' @param remove A logical value indicating if the removed columns should be
#'  removed. This setting is sticky: if it is FALSE and  you pipe the result of
#'  this function to another `fp_fraction()` call, the columns will be
#'  kept by default.
#'
#' @return A reduced factorial plan table (a `factorial.plan` object).
#' @export
#'
#' @seealso [fp_design_matrix()]
#'
#' @examples
#' # build a 2^5-2 fractional factorial plan with defining relationships
#' #   I=ABCD and I=BCDE
#' fp_design_matrix(5) %>%
#'   fp_fraction(~A*B*C*D) %>%
#'   fp_fraction(~B*C*D*E)
fp_fraction <- function(dm, formula, remove=TRUE) {
  `:=` <- NULL
  stopifnot(is_formula(formula))
  stopifnot("factorial.plan" %in% class(dm))

  if (!is.na(attr(dm, "fraction"))) remove = attr(dm, "removed")

  t <- terms(formula)
  f <- t %>% attr("factors")
  l <- t %>% attr("variables") %>% as.character() %>% tail(-1)

  if (all(!as.logical(f[1,])))
    l <- tail(l, -1)
  i <- paste0(l, collapse = "")

  sign <- ifelse(attr(t, "intercept") == 1, +1, -1)

  e <- l %>% paste0(collapse="*") %>% str2lang()

  dm <- dm %>%
    mutate(
      !!i := eval(e)
    )
  if (is.na(attr(dm, "fraction"))) {
    attr(dm, "fraction") <- paste0("I=", i)
    attr(dm, "generators") <- list(formula)
  }
  else {
    attr(dm, "fraction") <- c(attr(dm, "fraction"), paste0("I=", i))
    attr(dm, "generators") <- c(attr(dm, "generators"), formula)
  }

  attr(dm, "removed") <- remove
  attr(dm, "type") <- "fractional"

  if (remove)
    return(dm %>% filter (!!rlang::sym(i) == sign))
  else
    return(dm)
}



#' Augment to a centered design
#'
#' Add the central points to an existing $2^n$ factorial plan.
#'
#' @param dm A factorial plan table.
#' @param rep The number of replications.
#'
#' @return A central composite design (a `factorial.plan` object).
#' @export
#'
#' @examples
#' fp_design_matrix(3) %>%
#'   fp_augment_center()
fp_augment_center <- function(dm, rep=5) {
  stopifnot("factorial.plan" %in% class(dm))
  r <- nrow(dm)
  fct <- attr(dm, "factors")

  dm <- dm %>%
    add_row(
      StdOrder = (r+1):(r+rep),
      RunOrder = sample((r+1):(r+rep)),
      .treat = "center",
      .rep = 1:rep,
    ) %>%
    mutate(
      across({fct}, ~ if_else(.treat=="center", 0, .))
    )
  attr(dm, "type") <- "centered"
  return(dm)
}


#' Augment to a central composite design
#'
#' Adds the axial points to a $2^n$ centered factorial plan.
#'
#' @param dm A factorial plan table, with central points.
#' @param rep The number of replications.
#'
#' @return A central composite design (a `factorial.plan` object).
#' @export
#'
#' @examples
#' fp_design_matrix(3) %>%
#'   fp_augment_center(rep=4) %>%
#'   fp_augment_axial()
fp_augment_axial <- function(dm, rep=1) {
  . <- axial <- center <- StdOrder <- NULL
  n <- length(attr(dm, "factors"))
  dm <- dm %>%
    bind_rows(
      fp_design_matrix(attr(dm, "def.rel"), rep=rep, levels=(-1:1)*2^(n/4)) %>%
        rowwise() %>%
        mutate(
          axial=prod(c_across(attr(., "factors")))==0,
          center=sum(c_across(attr(., "factors")))==0
        ) %>%
        ungroup() %>%
        filter(axial & !center) %>%
        select(-axial, -center) %>%
        mutate(
          StdOrder=nrow(dm) + 1:n(),
          RunOrder=sample(StdOrder),
          .treat="axial"
        )
    )
  attr(dm, "type") <- "composite"
  return(dm)
}



#' Build a third defining relationship
#'
#' Given two defining relationships, it builds the third, dependent one.
#' For example, given $I=ABCD$ and $I=BCDE$, it will return $I=AE$, i.e.
#' the result of $ABCD * BCDE = AE$, since $A * A = I$
#'
#' Defining relationships are represented as one side formulas, e.g. $I=ABC$
#' becomes `~A*B*C`.
#'
#' @param f1 a one side formula.
#' @param f2 a one side formula.
#'
#' @return a formula.
#' @noRd
#' @examples
#' fp_third_dr(~A*B*C, ~B*C*D)
fp_third_dr <- function(f1, f2) {
  . <- NULL
  f1_l <- terms(f1) %>% attr("term.labels") %>% keep(~ nchar(.) == 1)
  f2_l <- terms(f2) %>% attr("term.labels") %>% keep(~ nchar(.) == 1)
  f <- c(f1_l, f2_l) %>%
    unique() %>%
    discard(~ (. %in% f1_l) & (. %in% f2_l)) %>%
    sort() %>%
    paste0(collapse="*") %>% {
      as.formula(paste0("~",.))
    }
  return(f)
}

#' Return a list of all defining relationships
#'
#' Given two or more independent refining relationships, represented as one
#' side formulas,, this function returns a list of all possible defining
#' relationships, including the dependent ones.
#'
#' Defining relationships are represented as one side formulas, e.g. $I=ABC$
#' becomes `~A*B*C`.
#'
#' @param ... formulas, or a single list of formulas.
#'
#' @return a list of formulas.
#' @export
#'
#' @examples
#' fp_all_drs(~A*B*C, ~B*C*D)
fp_all_drs <- function(...) {
  if (is.list(..1)) l <- ..1
  else l <- list(...)
  if (length(l) == 1) return(list(l[[1]]))
  n <- length(l)
  m <- (1:(2^n-1)) %>%
    accumulate(~ {
      a <- ceiling(log2(.y))
      x <- 2^floor(log2(.y))
      if (.y %% x == 0) return(a+1)
      else return(c(x, .y %% x))
    })
  for (i in seq_along(m)) {
    el <- m[[i]]
    if (length(el) == 2) {
      x <- el[[1]]
      y <- el[[2]]
      m[[i]] <- fp_third_dr(m[[x]], m[[y]])
    } else {
      m[[i]] <- l[[el]]
    }
  }
  return(m)
}


#' Return a merged defining relationship
#'
#' This function, given one or more independent refining relationships, returns
#' the most complete relationship, i.e. that which includes all the factors.
#'
#' Defining relationships are represented as one side formulas, e.g. $I=ABC$
#' becomes `~A*B*C`.
#'
#' @param f1 a formula.
#' @param ... other formulas.
#'
#' @return a formula.
#' @export
#'
#' @examples
#' fp_merge_drs(~A*B*C, ~B*C*D)
fp_merge_drs <- function(f1, ...) {
  . <- NULL
  f <- c(f1, ...) %>%
    map(~ terms(.) %>% attr("term.labels") %>% keep(~ nchar(.) == 1)) %>%
    unlist() %>%
    unique() %>%
    sort() %>%
    paste0(collapse="*") %>% {
      as.formula(paste0("~",.))
    }
  return(f)
}



#' Transforms a formula into a list of effects
#'
#' @param f a one-side formula.
#'
#' @return a string representation of the generator.
#' @noRd
#' @examples
#' formula2effect(~A*B*C)
formula2effect <- function(f) {
  f %>%
    as.character() %>%
    tail(-1) %>%
    str_remove_all("[*\\s]")
}




#' Build the alias matrix
#'
#' Given a list of formulas (defining relationships), this function returns
#' a matrix of all possible aliases.
#'
#' It is also possible to pass a fractional factorial plan, in which case the
#' defining relationships will be extracted from it.
#'
#' @param ... one or more formulas, or a single list of formulas, or a
#' fractional factorial plan.
#'
#' @return a square matrix: each cell is `0` if there is no alias, or an
#'   integer representing the index of the generator that produced that alias
#'   in the list of generators.
#' @export
#'
#' @seealso [fp_fraction()]
#' @examples
#' # with formulas:
#' fp_alias_matrix(~A*B*C, ~B*C*D)
#'
#' # with a fractional factorial plan:
#' fp_design_matrix(5) %>%
#'   fp_fraction(~A*B*C*D) %>%
#'   fp_fraction(~B*C*D*E) %>%
#'   fp_alias_matrix() %>%
#'   plot()
fp_alias_matrix <- function(...) {
  . <- NULL
  if ("factorial.plan" %in% class(..1))
    drs <- fp_all_drs(attr(..1, "generators"))
  else
    drs <- fp_all_drs(...)
  m <- drs %>%
    fp_merge_drs() %>%
    fp_alias_list() %>%
    names() %>% {
      matrix(0L, nrow=length(.), ncol=length(.), dimnames=list(., .))
    }
  for (rd_n in seq_along(drs)) {
    rd <- formula2effect(drs[[rd_n]])
    for (effect in rownames(m)) {
      als <- fp_gen2alias(rd, effect)
      if (nchar(als)> 0)  m[effect, als] <- rd_n
    }
  }
  class(m) <- c("alias.matrix", class(m))
  attr(m, "defining.relationships") <- drs
  return(m)
}


#' Convert an alias matrix to a tibble
#'
#' Given an alias matrix, this function returns a tidy tibble of the
#' alias structures, with the added `generator` column containing the generator
#' (i.e. right-hand side) of the defining relationship that generates each
#' alias.
#'
#' @param x the alias matrix object.
#' @param ... additional arguments to `as_tibble`.
#' @param compact a logical: if TRUE, it reports all possible effects
#'  combinations, even those with no alias.
#'
#' @return a tibble representation of the alias matrix
#' @export
#'
#' @examples
#' tibble::as_tibble(fp_alias_matrix(~A*B*C, ~B*C*D))
as_tibble.alias.matrix <- function(x, ..., compact=TRUE) {
  Effect.x <- . <- generator <- NULL
  class(x) <- Filter(function(x) x!="alias.matrix", class(x))
  drs <- attr(x, "defining.relationships")
  drs_list <- drs %>%
    map(~ formula2effect(.)) %>%
    unlist()
  as_tibble(x, ..., rownames="Effect.x") %>%
    pivot_longer(-Effect.x, names_to="Effect.y", values_to="generator") %>% {
      if (compact)
        filter(., generator>0)
      else .
    }  %>%
    mutate(
      generator = ifelse(generator>0, drs_list[generator], NA)
    )
}

#' Print an alias matrix
#'
#' @export
#' @noRd
#'
print.alias.matrix <- function(x, ...) {
  . <- NULL
  class(x) <- Filter(function(x) x!="alias.matrix", class(x))
  attr(x, "defining.relationships") %>%
    map(~ paste0("I=", formula2effect(.))) %>%
    unlist() %>%
    cat("Defining relationships:\n", ., "\n\n")
  attr(x, "defining.relationships") <- NULL
  print(x, ...)
}

#' Plot the alias matrix
#'
#' Produces a tile plot of the alias matrix.
#'
#' @param x an alias matrix.
#' @param ... additional arguments to [ggplot2::geom_tile()].
#' @param compact logical, if TRUE only positive aliases are shown, omitting
#' empty rows and columns.
#'
#' @return a ggplot object.
#' @export
#'
#' @examples
#' fp_alias_matrix(~A*B*C, ~B*C*D) %>%
#'   plot()
plot.alias.matrix <- function(x, ..., compact=TRUE) {
  Effect.x <- Effect.y <- generator <- . <- NULL
  drs_list <- attr(x, "defining.relationships") %>%
    map(~ paste0("I=",formula2effect(.))) %>%
    unlist()
  x %>%
    as_tibble() %>%
    ggplot(aes(x=Effect.x, y=Effect.y, fill=generator)) +
    geom_tile(color=grey(1/4), ...) +
    scale_fill_viridis_d() +
    labs(
      x = "Effect",
      y = "Effect",
      fill = "Generator",
      title = paste("Aliases for def. relationships:", paste(drs_list, collapse=", "))
    ) +
    theme(
      axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)
    )
}
