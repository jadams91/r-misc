
#### --------------------------------- DPLL ------------------------------------

find_pure_symbol <- function(l, vals) {
  syms <- lapply(names(Filter(is.na, vals)), llit)
  for (s in syms) {
    s1 <- sum(vapply(l, function(x)  s %in2% x,logical(1)))
    s2 <- sum(vapply(l, function(x) -s %in2% x,logical(1)))
    if (s1 && s2) next
    if (s1) return( s)
    if (s2) return(-s)
  }
  
  NULL
}

find_unit_clause <- function(l, vals) {
  s <- Filter(function(x) length(x) == 1, l)
  s <- Filter(function(x) is.na(vals[[unlist(x)]]), s)
  if (length(s) > 0) return(s[[1]])
  
  negs <- lapply(names(Filter(Negate(is.na), vals)),
                 function(x) ifelse(vals[[x]], `-`, identity)(llit(x)))
  for (d in l) {
    w <- !vapply(d, function(x) -x %in2% negs, logical(1))
    if (sum(w) == 1) return(d[w][[1]])
  }
  
  NULL
}

dpll <- function(l, vals) {
  r <- Reduce(`&`, vapply(l, function(x) lsolve(x, vals), logical(1)))
  if (!is.na(r)) return(r)
  
  s <- find_pure_symbol(l, vals)
  if (!is.null(s)) {
    vals[[unlist(s)]] <- !not(s)
    return(dpll(l, vals))
  }
  
  s <- find_unit_clause(l, vals)
  if (!is.null(s)) {
    vals[[unlist(s)]] <- !not(s)
    return(dpll(l, vals))
  }
  
  s <- names(vals[vapply(vals, is.na, logical(1))])[[1]]
  vals_ <- vals
  vals[[s]] <- FALSE
  vals_[[s]] <- TRUE
  dpll(l, vals) || dpll(l, vals_)
}

dpll_sat <- function(l) {
  l <- remove_tau(as_lcnf(l))
  lits <- literals(l)
  vals <- as.list(rep(NA, length(lits)))
  names(vals) <- lits
  
  dpll(l, vals)
}

#### ------------------------------ WALK-SAT ----------------------------------

walk_iter <- function(l, vals, p, max_flips, i) {
  if (i > max_flips)   return(FALSE)
  if (lsolve(l, vals)) return(TRUE)
  
  false  <- Filter(function(x) !lsolve(x, vals), l)
  clause <- false[[sample(seq_along(false), 1)]]
  if (runif(1) < p) {
    w <- clause[[sample(seq_along(clause), 1)]][[1]]
    vals[[w]] <- !vals[[w]]
  } else {
    lits <- literals(clause)
    sat <- vapply(lits, function(x) {
      vals_ <- vals
      vals_[[x]] <- !vals_[[x]]
      sum(vapply(l, function(y) lsolve(y, vals_), logical(1)))
    }, numeric(1))
    w <- lits[(sat == max(sat))][[1]]
    vals[[w]] <- !vals[[w]]
  }
  
  walk_iter(l, vals, p, max_flips, i + 1)
}

walk_sat <- function(l, p, max_flips) {
  l <- remove_tau(as_lcnf(l))
  lits <- literals(l)
  vals <- as.list(sample(c(FALSE, TRUE), length(lits), replace = TRUE))
  names(vals) <- lits
  
  walk_iter(l, vals, p, max_flips, i = 1)
}
