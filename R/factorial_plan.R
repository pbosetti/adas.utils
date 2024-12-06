#' Factorial Plan Defining Relationship
#'
#'  Builds a formula from a number of factors
#'
#' @param arg If it is a formula, it is returned verbatim. If it is a number, a
#' formula is built with the number of factors. If it is neither a formula nor a
#' number, an error is thrown.
#'
#' @return A formula
#' @export
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
#'  Builds a list of treatments from a formula, or from a number of factors.
#'
#' @param arg Either a formula or a number of factors
#'
#' @return A list of treatments
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
#' @param arg Either a formula or a number of factors
#' @param rep Number of replications
#' @param levels Levels of the factors
#'
#' @return A design matrix: a subclass of a tibble of class `factorial.plan`
#' @export
#'
#' @examples
#' fp_design_matrix(3, rep=2, levels=c("-", "+"))
fp_design_matrix <- function(arg, rep = 1, levels = c(-1,1)) {
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
      .treat= rep(fp_treatments(arg), rep),
      .before = .data[[fct[1]]]
    ) %>%
    relocate(.rep, .before=.data[[fct[1]]]) %>%
    mutate(Y=NA)
  class(dm) <- c("factorial.plan", class(dm))
  attr(dm, "def.rel") <- fp_defrel(arg)
  attr(dm, "fraction") <- NA
  return(dm)
}

#' Factorial Plan effect names from a formula
#'
#'  Returns the effect names from a formula, according to Yate's convention.
#'
#' @param arg A formula
#'
#' @return An ordered factor with the effect names
#' @export
#'
#' @examples
#' fp_effect_names(~A*B*C)
fp_effect_names <- function(arg) {
  formula <- fp_defrel(arg)
  terms(formula) %>%
    attr("factors") %>%
    as_tibble() %>%  {
      bind_rows(.,
                mutate(.,
                       p = 0:(n()-1),
                       w = 2^p
                ) %>%
                  mutate(
                    across(
                      -(p:w),
                      ~ .x * w
                    )
                  ) %>%
                  select(-(p:w)) %>%
                  summarize_all(sum)
      )
    } %>% {
      tibble(
        effect = colnames(.) %>% str_remove_all(":"),
        order = tail(., 1) %>% as.numeric()
      )
    } %>%
    arrange(order) %>%
    pull(effect) %>% factor(., ordered=T, levels=.)
}


#' Check for Factor Aliases
#'
#'  Checks if two factors are aliased in a formula. This function is
#'  moslty used internally to build the alias matrix.
#'
#' @param arg A formula for the defining relationship
#' @param A a string representing an effect (e.g. `"AB"`)
#' @param B a string representing an effect (e.g. `"CD"`)
#'
#' @return A logical value
#' @export
#'
#'@seealso [fp_defrel()] [fp_alias()] [fp_alias_list()]
#'
#' @examples
#' fp_has_alias(~A*B*C*D, "AB", "CD")
fp_has_alias <- function(arg, A, B) {
  formula <- fp_defrel(arg)
  m <- formula %>% terms() %>% attr("factors") %>% as_tibble() %>%
    rename_with(~ str_remove_all(., ":"))
  all(xor(m[A], m[B]))
}


#' Tabulate all the Aliases from a Defining Relationship
#'
#'  Tabulates all the aliases from a defining relationship.
#'
#' @param arg A formula for the defining relationship
#'
#' @return A matrix of aliases
#' @export
#'
#' @seealso [fp_defrel()]
#'
#' @examples
#' fp_alias(~A*B*C*D)
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



#' List All Alias for a Fractional Factorial Plan
#'
#' Given a defining relationship, this function lists all the aliases for a
#' fractional factorial plan.
#'
#' @param arg A formula for the defining relationship, or the number of factors
#'
#' @return a list of aliases
#' @export
#'
#' @seealso [fp_has_alias()] [fp_alias()]
#'
#' @examples
#' fp_alias_list(~A*B*C*D)
fp_alias_list <- function(arg) {
  formula <- fp_defrel(arg)
  m <- fp_alias(formula) %>% as_tibble(rownames="effect")
  f <- m %>% pull(effect) %>% purrr::set_names()
  f %>%
    purrr::map(~ filter(m, .data[[.]]) %>% pull(effect))
}




#' Reduce a Factorial Plan by 1/2 Fraction
#'
#' @param dm A factorial plan table
#' @param formula A formula for the defining relationship
#' @param remove A logical value indicating if the removed columns should be removed
#'
#' @return A reduced factorial plan table
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
  if (is.na(attr(dm, "fraction")))
    attr(dm, "fraction") <- i
  else
    attr(dm, "fraction") <- c(attr(dm, "fraction"), i)

  attr(dm, "removed") <- remove

  if (remove)
    return(dm %>% filter (!!rlang::sym(i) == sign))
  else
    return(dm)
}
