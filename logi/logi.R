
#### ------------------------ NEW GENERIC FUNCTIONS ----------------------------

`%=>%`  <- function(...) UseMethod("%=>%")
`%<=>%` <- function(...) UseMethod("%<=>%")
`%xor%` <- function(...) UseMethod("%xor%")
cross   <- function(...) UseMethod("cross")


#### -------------------------- LOGICAL OPERATORS ------------------------------

`%=>%.logical`  <- function(e1, e2) !e1 | e2
`%<=>%.logical` <- function(e1, e2) (!e1 & !e2) | (e1 & e2)
`%xor%.logical` <- function(e1, e2) (!e1 & e2) | (e1 & !e2)

`%in2%` <- function(e, es) {
  Reduce(`|`, Map(function(x) identical(e, x), es), init = FALSE)
}

logical_ops <- list(`|`, `&`, `%=>%`, `%<=>%`, `%xor%`)
op_to_char <- function(op) {
  stopifnot(op %in2% logical_ops)
  if (identical(op, `|`))     return(' | ')
  if (identical(op, `&`))     return(' & ')
  if (identical(op, `%=>%`))  return(' => ')
  if (identical(op, `%<=>%`)) return(' <=> ')
  if (identical(op, `%xor%`)) return(' xor ')
}


#### ---------------------------- LOGI HELPERS ---------------------------------

not <- function(l) attr(l, "not", exact = TRUE)
tag <- function(l) attr(l, "tag", exact = TRUE)
op  <- function(l) attr(l, "op",  exact = TRUE)

n <- function(l) ifelse(not(l), `-`, identity)

op_char <- function(l) op_to_char(op(l))

is_logi <- function(l) "logi" %in% class(l)
is_llit <- function(l) "llit" %in% class(l)
is_lsen <- function(l) "lsen" %in% class(l)

lMap <- function(f, l, .tag = tag(l)) {
  attr(l, "tag") <- .tag
  `attributes<-`(Map(f, l), attributes(l))
}

lFilter <-function(f, l, .tag = tag(l)) {
  attr(l, "tag") <- .tag
  `attributes<-`(Filter(f, l), attributes(l))
}

cross <- function(l1, l2) {
  Reduce(c, Map(function(x) Map(function(y) c(x, y), l1), l2))
}


#### --------------------------- LOGI TEMPLATE ---------------------------------

new_logi <- function(val, not) {
  structure(
    val, 
    not   = not, 
    class = "logi"
  )
}

validate_logi <- function(l) {
  if (is_llit(l)) validate_llit(l)
  else if (is_lsen(l)) validate_lsen(l)
}


#### ---------------------------- LLIT CLASS -----------------------------------

new_llit <- function(val, not) {
  structure(
    val, 
    not   = not, 
    class = c("llit", "logi")
  )
}

validate_llit <- function(l) {
  val <- unclass(l)
  not <- attr(l, "not")
  
  if (!(is.character(val) && length(val) == 1)) {
    stop(
      "The value of a literal must a character vector of length 1", 
      call. = FALSE
    )
  }
  if (!(is.logical(not) && length(not) == 1)) {
    stop(
      "The 'not' attribute must be a logical vector of length 1", 
      call. = FALSE
    )
  }
  
  l
}

llit <- function(val = character(), not = FALSE) {
  val <- as.character(val)
  not <- as.logical(not)
  
  validate_llit(new_llit(val, not))
}


#### ---------------------------- LSEN CLASS -----------------------------------

new_lsen <- function(val, not, op, tag) {
  structure(
    val, 
    not   = not, 
    op    = op, 
    tag   = tag, 
    class = c("lsen", "logi")
  )
}

validate_lsen <- function(l) {
  val <- unclass(l)
  not <- attr(l, "not")
  op  <- attr(l, "op")
  tag <- attr(l, "tag")
  
  if (!(is.list(val))) {
    stop(
      "The value of a sentence must be a list of at least one logi element", 
      call. = FALSE
    )
  }
  if (!(is.logical(not) && length(not) == 1)) {
    stop(
      "The 'not' attribute must be a logical vector of length 1", 
      call. = FALSE
    )
  }
  if (!(is.function(op) && op %in2% logical_ops)) {
    stop(
      "The 'op' attribute must be a logical operator", 
      call. = FALSE
    )
  }
  if (!(is.character(tag) & length(tag) == 1)) {
    stop(
      "The 'tag' attribute must be a character vector of length 1", 
      call. = FALSE
    )
  }
  
  invisible(Map(validate_logi, val))
  
  l
}

lsen <- function(val = list(), not = FALSE, op = `|`, tag = NA_character_) {
  val <- as.list(val)
  not <- as.logical(not)
  op  <- as.function(op)
  tag <- as.character(tag)
  
  validate_lsen(new_lsen(val, not, op, tag))
}


#### -------------------------- LOGI FUNCTIONS ---------------------------------

lquote <- function(expr) {
  .lquote <- function(expr, not = FALSE) {
    expr <- as.list(expr)
    if (length(expr) == 1 && is.symbol(expr[[1]])) {
      llit(as.character(expr[[1]]), not)
    } else if (identical(expr[[1]], as.symbol('('))) { 
      .lquote(expr[[2]], not)
    } else if (identical(expr[[1]], as.symbol('-'))) { 
      .lquote(expr[[2]], not = !not)
    } else {
      op  <- eval(expr[[1]])
      val <- expr[-1]
      lsen(Map(.lquote, val), not, op, tag = "binary")
    }
  }
  
  .lquote(substitute(expr))
}

lsolve <- function(l, vals) {
  `~` <- ifelse(not(l), `!`, identity)
  if (is_llit(l)) return(~vals[[l]])
  ~Reduce(op(l), Map(function(x) lsolve(x, vals), l), right = TRUE)
}

lsweep <- function(l) n(l)(Reduce(op(l), l, right = TRUE))

literals <- function(l) {
  if (is_llit(l)) return(l)
  sort(Reduce(union, Map(literals, l)))
}

truth_table <- function(lits = character()) {
  n <- length(lits)
  f <- function(x) rep(c(F, T), times = 2 ^ (x - 1), each = 2 ^ (n - x))
  as.data.frame(lapply(seq_len(n), f), col.names = lits)
}

move_neg <- function(l) {
  if (is_llit(l)) return(l)
  if (length(l) == 1 && not(l)) return(move_neg(n(l)(l[[1]])))
  if (length(l) > 2) l <- lsweep(l)
  if (not(l) && identical(op(l), `|`)) l <- -l[[1]] & -l[[2]]
  else if (not(l) && identical(op(l), `&`)) l <- -l[[1]] | -l[[2]]
  lMap(move_neg, l, .tag = "binary")
}


#### -------------------------- LOGI GENERICS ----------------------------------

as.expression.logi <- function(l) {
  
}

as.character.llit <- function(l) {
  n <- ifelse(not(l), "-", "")
  paste0(n, unclass(l))
}

as.character.lsen <- function(l) {
  n <- ifelse(not(l), "-", "")
  paste0(n, '(', paste0(Map(as.character, l), collapse = op_char(l)), ')')
}

print.logi <- function(l) {
  print(as.character(l))
}

`-.logi` <- function(e1, e2) {
  stopifnot(missing(e2))
  attr(e1, "not") <- !not(e1)
  e1
}

`|.logi` <- function(..., .tag = NA_character_) {
  if (is_logi(..1)) lis <- list(...) else lis <- ..1
  lsen(lis, not = FALSE, op = `|`, tag = .tag)
}

`&.logi` <- function(..., .tag = NA_character_) {
  if (is_logi(..1)) lis <- list(...) else lis <- ..1
  lsen(lis, not = FALSE, op = `&`, tag = .tag)
}

`%=>%.logi` <- function(..., .tag = NA_character_) {
  if (is_logi(..1)) lis <- list(...) else lis <- ..1
  lsen(lis, not = FALSE, op = `%=>%`, tag = .tag)
}

`%<=>%.logi` <- function(..., .tag = NA_character_) {
  if (is_logi(..1)) lis <- list(...) else lis <- ..1
  lsen(lis, not = FALSE, op = `%<=>%`, tag = .tag)
}

`%xor%.logi` <- function(..., .tag = NA_character_) {
  if (is_logi(..1)) lis <- list(...) else lis <- ..1
  lsen(lis, not = FALSE, op = `%xor%`, tag = .tag)
}

sort.logi <- function(l, recursive = FALSE, .tag = tag(l)) {
  if (is_llit(l)) return(l)
  if (recursive) l <- lMap(sort, l)
  o <- order(vapply(l, as.character, character(1)))
  lsen(unique(l[o]), not(l), op(l), tag = .tag)
}

merge.logi <- function(x, y, .tag = NA_character_) {
  stopifnot(identical(op(x), op(y)))
  sort(lsen(c(x, y), op = op(x), tag = .tag))
}


#### ------------------------- STANDARDIZATION ---------------------------------

remove_xor <- function(l) {
  if (is_llit(l)) return(l)
  l <- lsweep(l)
  if (identical(op(l), `%xor%`)) l <- -n(l)(l[[1]] %<=>% l[[2]])
  lMap(remove_xor, l)
}

remove_eqv <- function(l) {
  if (is_llit(l)) return(l)
  l <- lsweep(l)
  if (identical(op(l), `%<=>%`)) 
    l <- n(l)((l[[1]] %=>% l[[2]]) & (l[[2]] %=>% l[[1]]))
  lMap(remove_eqv, l)
}

remove_imp <- function(l) {
  if (is_llit(l)) return(l)
  l <- lsweep(l)
  if (identical(op(l), `%=>%`)) l <- n(l)(-l[[1]] | l[[2]])
  lMap(remove_imp, l)
}

standardize <- function(l) move_neg(remove_imp(remove_eqv(remove_xor(l))))
