
#### ---------------------------- LCNF CLASS -----------------------------------

new_lcnf <- function(l, rec) {
  stopifnot(is.list(l))
  
  l <- Map(function(x) do.call(`|`, x), l)
  l <- do.call(`&`, l)
  l <- remove_tau(l)
  l <- sort(l, rec)
  
  structure(
    l, 
    not = FALSE, 
    op  = `&`, 
    tag = NA_character_, 
    class = c("lcnf", "lsen", "logi")
  )
}

.validate_clause <- function(l) {
  not <- attr(l, "not")
  op  <- attr(l, "op")
  
  if (not) {
    stop(
      "The 'not' attribute of a clause must be FALSE", 
      call. = FALSE
    )
  }
  if (!(is.function(op) && op %in2% logical_ops)) {
    stop(
      "The 'op' attribute of a clause must be an `|` operator", 
      call. = FALSE
    )
  }
  
  invisible(Map(validate_llit, l))
  
  l
}

validate_lcnf <- function(l) {
  invisible(validate_logi(l))
  not <- attr(l, "not")
  op  <- attr(l, "op")
  
  if (not) {
    stop(
      "The 'not' attribute of a CNF must be FALSE", 
      call. = FALSE
    )
  }
  if (!(is.function(op) && op %in2% logical_ops)) {
    stop(
      "The 'op' attribute of a CNF must be an `&` operator", 
      call. = FALSE
    )
  }
  
  invisible(Map(.validate_clause, l))
  
  l
}

lcnf <- function(l) {
  validate_lcnf(new_lcnf(l, rec = T))
}


#### -------------------------- LCNF GENERICS ----------------------------------

is_lcnf <- function(l) "lcnf" %in% class(l)

unlcnf <- function(l) {
  stopifnot(is_lcnf(l))
  class(l) <- c("lsen", "logi")
  l
}

`-.lcnf` <- function(e1, e2) {
  stopifnot(missing(e2))
  as_lcnf(move_neg(NextMethod()))
}

`|.lcnf` <- function(e1, e2) {
  if (is_lcnf(e1) && is_lcnf(e2)) new_lcnf(cross(e1, e2), rec = TRUE)
  else NextMethod()
}

`&.lcnf` <- function(e1, e2) {
  if (is_lcnf(e1) && is_lcnf(e2)) new_lcnf(c(e1, e2), rec = FALSE)
  else NextMethod()
}

`%=>%.lcnf` <- function(e1, e2) {
  if (is_lcnf(e1) && is_lcnf(e2)) -e1 | e2
  else NextMethod()
}

`%<=>%.lcnf` <- function(e1, e2) {
  if (is_lcnf(e1) && is_lcnf(e2)) (e1 %=>% e2) & (e2 %=>% e1)
  else NextMethod()
}

`%xor%.lcnf` <- function(e1, e2) {
  if (is_lcnf(e1) && is_lcnf(e2)) -(e1 %<=>% e2)
  else NextMethod()
}


as_lcnf <- function(l) UseMethod("as_lcnf")

as_lcnf.lcnf <- function(l) l
as_lcnf.llit <- function(l) new_lcnf(list(list(l)), rec = FALSE)
as_lcnf.lsen <- function(l) lsweep(lMap(as_lcnf, l))


#### -------------------------- LCNF METHODS -----------------------------------

remove_tau <- function(l) {
  lFilter(function(x)
    Reduce(`&&`, Map(function(y)
      ifelse(not(y), TRUE, !(-y %in2% x)), x)), l)
}
