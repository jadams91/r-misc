
#### --------------------------- LITERAL S3 CLASS -----------------------------

new_literal <- function(sym = character(), neg = logical()) {
  stopifnot(is.character(sym))
  stopifnot(is.logical(neg))
  
  structure(
    sym, 
    negated = neg, 
    class   = "literal"
  )
}

validate_literal <- function(lt) {
  val <- unclass(lt)
  neg <- attr(lt, "negated")
  
  if (!(length(val) == 1) || is.na(val)) {
    stop(
      "The symbol must be a non-NA character vector of length 1",
      call. = FALSE
    )
  }
  
  if (!(length(neg) == 1) || is.na(neg)) {
    stop(
      "The negated attribute must be a non-NA character vector of length 1",
      call. = FALSE
    )
  }
  
  lt
}

literal <- function(sym = character(), neg = F) {
  sym <- as.character(sym)
  neg <- as.logical(neg)
  
  validate_literal(new_literal(sym, neg))
}

is_negated <- function(x = literal()) {
  attr(x, "negated")
}

is_literal <- function(x) {
  class(x) == "literal"
}

as.character.literal <- function(x = literal()) {
  if (is_negated(x)) s <- paste0('~', x)
  else s <- paste0(x)
  s
}

print.literal <- function(x = literal()) {
  print(as.character(x))
}


#### --------------------------- OPERATOR S3 CLASS ----------------------------

new_operator <- function(op = character()) {
  stopifnot(is.character(op))
  
  structure(
    op, 
    class = "operator"
  )
}


validate_operator <- function(op) {
  val <- unclass(op)
  
  valid_operators <- c('or', 'and', '=>', '<=>')
  if (!(length(val) == 1)) {
    stop(
      "The operator must be a character vector of length 1",
      call. = FALSE
    )
  }
  
  if (!(val %in% valid_operators)) {
    stop(
      paste0("The operator must be on of the following: ", 
             paste0(valid_operators, collapse = ' ')),
      call. = FALSE
    )
  }
  
  op
}

operator <- function(op = character()) {
  op <- as.character(op)
  
  validate_operator(new_operator(op))
}

is_operator <- function(x) {
  class(x) == "operator"
}

as.character.operator <- function(x = operator()) {
  paste0(" ", x, " ")
}

print.operator <- function(x = operator()) {
  print(as.character(x))
}


#### ---------------------- SENTENCE S3 CLASS ----------------------------------

is_clauselist <- function(x) {
  is.list(x) && prod(vapply(x, 
                            function(x) is_literal(x) || is_sentence(x), 
                            logical(1))) && length(x) > 1 
}

new_sentence <- function(op = operator(), clauses = list(), neg = logical()) {
  stopifnot(is_operator(op))
  stopifnot(is_clauselist(clauses))
  stopifnot(is.logical(neg))
  
  structure(
    clauses, 
    operator = op, 
    negated  = neg, 
    class    = "sentence"
  )
}

validate_sentence <- function(sen) {
  val <- unclass(sen)
  op  <- attr(sen, "operator")
  neg <- attr(sen, "negated")
  
  for (v in val) {
    if (is_sentence(v)) validate_sentence(v)
    else if (is_literal(v)) validate_literal(v)
    else stop(
      "The list of clauses contains an improper clause"
    )
  }
  
  if (is_operator(op)) validate_operator(op)
  else stop(
    "The operator attribute is not of class operator", 
    call. = FALSE
  )
  
  if (!(length(neg) == 1) || is.na(neg)) {
    stop(
      "The negated attribute must be a non-NA character vector of length 1",
      call. = FALSE
    )
  }
  
  sen
}

sentence <- function(op = operator(), clauses = list(), neg = F) {
  validate_sentence(new_sentence(op, clauses, neg))
}

is_sentence <- function(x) {
  class(x) == "sentence"
}

as.character.sentence <- function(x = sentence()) {
  val <- unclass(x)
  op  <- attr(x, "operator")
  neg <- attr(x, "negated")
  
  if (neg) s <- "~("
  else     s <- "("
  
  s <- paste0(s, as.character(val[[1]]))
  for (v in val[2:length(val)]) {
    s <- paste0(s, as.character(op), as.character(v))
  }
  
  s <- paste0(s, ")")
  s
}

print.sentence <- function(x = sentence()) {
  print(as.character(x))
}


#### -------------------------- SENTENCE OPERATORS ----------------------------

not <- function(x = sentence()) {
  attr(x, "negated") <- !attr(x, "negated")
  x
}

or <- function(...) {
  sentence(operator('or'), list(...))
}

and <- function(...) {
  sentence(operator('and'), list(...))
}

`%or%` <- function(x, y) {
  sentence(operator('or'), list(x, y))
}

`%and%` <- function(x, y) {
  sentence(operator('and'), list(x, y))
}

`%=>%` <- function(x, y) {
  sentence(operator('=>'), list(x, y))
}

`%<=>%` <- function(x, y) {
  sentence(operator('<=>'), list(x, y))
}


#### ---------------------------- SATISFIABILITY ------------------------------

`%imp%` <- function(x = logical(), y = logical()) {
  !x || y
}

`%eqv%`<- function(x = logical(), y = logical()) {
  (x %imp% y) && (y %imp% x)
}

satisfies <- function(prop = logical(), sen = sentence()) {
  if (is_literal(sen)) {
    if (is_negated(sen)) return(!prop[[sen]])
    else return(prop[[sen]])
  }
  
  val <- unclass(sen)
  op  <- attr(sen, "operator")
  neg <- attr(sen, "negated")
  
  s <- val[[1]]
  if (is_literal(s) && !is_negated(s)) acc <- prop[[s]]
  else if (is_literal(s)) acc <- !prop[[s]]
  else acc <- satisfies(prop, s)
  
  for (v in val[2:length(val)]) {
    if (is.na(acc)) stop(
      "Not all symbols were provided",
      call. = FALSE
    )
    
    if (op == operator('or')) {
      acc <- acc || satisfies(prop, v)
    } else if (op == operator('and')) {
      acc <- acc && satisfies(prop, v)
    } else if (op == operator('=>')) {
      acc <- acc %imp% satisfies(prop, v)
    } else if (op == operator('<=>')) {
      acc <- acc %eqv% satisfies(prop, v)
    } else {
      stop(
        "Invalid sentence",
        call. = FALSE
      )
    }
  }
  
  if (neg) acc <- !acc
  acc
}

generate_truth_table <- function(n, symbols = NULL) {
  lst <- vector("list", n)
  for (i in seq_len(n)) {
    lst[[i]] <- rep(c(F, T), times = 2 ^ (i - 1), each = 2 ^ (n - i))
  }
  
  tab <- matrix(unlist(lst), ncol = n)
  colnames(tab) <- symbols
  tab
}

get_symbols <- function(sen = sentence()) {
  val <- unclass(sen)
  symbols <- character()
  
  if (is_literal(sen)) return(val)
  
  for (i in seq_along(val)) {
    if (is_literal(val[[i]]))
      symbols <- union(symbols, val[[i]])
    else
      symbols <- union(symbols, get_symbols(val[[i]]))
  }
  
  symbols
}

find_all <- function(sen = sentence(), unknown = get_symbols(sen), 
                     known = logical()) {
  n   <- length(unknown)
  tab <- generate_truth_table(n, unknown)
  
  idx <- 1
  for (i in seq_len(2 ^ n)) {
    p <- c(tab[idx, ], known)
    if (!satisfies(p, sen)) {
      tab <- tab[-idx, ]
      if (is.null(dim(tab)) && length(tab) > 0) { 
        dim(tab) <- c(1, length(tab))
        colnames(tab) <- unknown
      }
    } else {
       idx <- idx + 1
    }
  }
  
  if (length(tab) == 0) tab <- logical()
  tab
}


#### ---------------------------- KNOWLEDGE BASE ------------------------------

new_knowledge_base <- function(symbols = logical(), rules = list()) {
  stopifnot(is.logical(symbols))
  stopifnot(is.list(rules))
  
  structure(
    symbols, 
    rules = rules,
    class = "knowledge base"
  )
}

validate_knowledge_base <- function(kb) {
  val <- unclass(kb)
  rul <- attr(kb, "rules")
  
  if (!is.logical(val) || is.null(names(val))) {
    stop(
      "The symbols must be stored in a named logical vector",
      call. = FALSE
    )
  }
  
  for (r in rul) {
    if (is_sentence(r)) validate_sentence(r)
    else if (is_literal(r)) validate_literal(r)
    else stop(
      "The list of rules contains an improper sentence"
    )
  }
  
  kb
}

knowledge_base <- function(symbols = logical(), rules = list()) {
  validate_knowledge_base(new_knowledge_base(symbols, rules))
}

#### ---------------------------------- CNF -----------------------------------

is_elemental <- function(sen = sentence()) {
  val <- unclass(sen)
  neg <- attr(sen, "negated")
  
  !neg && prod(vapply(val, is_literal, logical(1)))
}

is_cnf <- function(sen = sentence()) {
  if (is_literal(sen)) return(T)
  
  val <- unclass(sen)
  op  <- attr(sen, "operator")
  neg <- attr(sen, "negated")
  
  if (op == operator('or') && is_elemental(sen)) {
    return(TRUE)
  }
  
  if (neg || op != operator('and')) {
    return(FALSE)
  }
  
  f <- function(x) {
    is_literal(x) || 
      (is_elemental(x) && (attr(x, "operator") == operator('or')))
  }
  
  as.logical(prod(vapply(val, f, logical(1))))
}

contains <- function(sen = sentence(), op = operator()) {
  
}

remove_eqv <- function(sen = sentence()) {
  if (is_literal(sen)) return(sen)
  
  val <- unclass(sen)
  op  <- attr(sen, "operator")
  neg <- attr(sen, "negated")
  
  if (op == operator('<=>')) {
    s <- (val[[1]] %=>% val[[2]]) %and% (val[[2]] %=>% val[[1]])
    if (length(val) == 2 && !neg) {
      return(remove_eqv(s))
    } else if (length(val) == 2) {
      return(remove_eqv(not(s)))
    } else {
      return(remove_eqv(sentence(
        operator('<=>'), 
        c(list(s), val[c(-1, -2)]), 
        neg = neg
      )))
    }
  } else {
    return(sentence(
      op, 
      lapply(val, remove_eqv), 
      neg = neg
    ))
  }
}

remove_imp <- function(sen = sentence()) {
  if (is_literal(sen)) return(sen)
  
  val <- unclass(sen)
  op  <- attr(sen, "operator")
  neg <- attr(sen, "negated")
  
  if (op == operator('=>')) {
    s <- (not(val[[1]]) %or% val[[2]])
    if (length(val) == 2 && !neg) {
      return(remove_imp(s))
    } else if (length(val) == 2) {
      return(remove_imp(not(s)))
    } else {
      return(remove_imp(sentence(
        operator('=>'), 
        c(list(s), val[c(-1, -2)]), 
        neg = neg
      )))
    }
  } else {
    return(sentence(
      op, 
      lapply(val, remove_imp), 
      neg = neg
    ))
  }
}

move_neg <- function(sen = sentence()) {
  if (is_literal(sen)) return(sen)
  
  val <- unclass(sen)
  op  <- attr(sen, "operator")
  neg <- attr(sen, "negated")
  
  if (neg && (op == operator('and') || op == operator('or'))) {
    map <- list(and = operator('or'), or = operator('and'))
    return(sentence(
      map[[op]], 
      lapply(val, function(x) move_neg(not(x))), 
      neg = F
    ))
  }
  
  sentence(
    op, 
    lapply(val, move_neg), 
    neg = neg
  )
}

join_sentences <- function(sens = list(sentence())) {
  acc <- list()
  for (sen in sens) {
    if (!is_literal(sen) && attr(sen, "operator") == operator('and')) {
      val <- unclass(sen)
    } else {
      val <- list(sen)
    }
    
    acc <- c(acc, val)
  }
  
  sentence(operator('and'), acc)
}

intersect_sentences <- function(sens = list(sentence())) {
  acc <- list()
  for (sen in sens) {
    if (!is_literal(sen) && attr(sen, "operator") == operator('and')) {
      val <- unclass(sen)
    } else {
      val <- list(sen)
    }
    
    acc <- c(acc, list(val))
  }
  
  while (length(acc) > 1) {
    acc2 <- list()
    for (s1 in acc[[1]]) {
      if (!is_literal(s1)) v1 <- unclass(s1)
      else v1 <- list(s1)
      for (s2 in acc[[2]]) {
        if (!is_literal(s2)) v2 <- unclass(s2)
        else v2 <- list(s2)
        
        acc2 <- c(acc2, list(sentence(operator('or'), c(v1, v2))))
      }
    }
    if (length(acc2) > 1) {
      acc <- c(list(sentence(operator('and'), acc2)), acc[c(-1, -2)])
    } else {
      acc <- c(acc2, acc[c(-1, -2)])
    }
  }
  
  acc[[1]]
}

distribute <- function(sen = sentence()) {
  if (is_cnf(sen)) return(sen)
  
  val <- unclass(sen)
  op  <- attr(sen, "operator")
  
  val <- lapply(val, distribute)
  
  if (op == operator('and')) {
    return(join_sentences(val))
  } else if (op == operator('or')) {
    return(intersect_sentences(val))
  }
}

library(magrittr)

convert_to_cnf <- function(sen = sentence()) {
  sen %>% remove_eqv %>% remove_imp %>% move_neg %>% distribute
}


#### ---------------------------- OBSOLETE -------------------------------------

# lsen(c(l[[1]], l[[2]]), op = `&`, tag = "cnf")
# l <- Map(function(x) lsen(c(x[[1]], x[[2]]), op = `|`), l)
# lsen(l, op = `&`, tag = "cnf")
# `&.logi`(c(l[[1]], l[[2]]), .tag = "cnf")
# l <- Map(function(x) `|.logi`(c(x[[1]], x[[2]])), l)
# `&.logi`(l, .tag = "cnf")


# join_sorted <- function(x, y) {
#   n <- length(x); m <- length(y)
#   r <- vector("list", n + m)
#   i <- 1; j <- 1; k <- 1
#   while (i <= n && j <= m) {
#     if (x[[i]] < y[[j]]) { r[[k]] <- x[[i]]; i <- i + 1; k <- k + 1 }
#     else if (x[[i]] > y[[j]]) { r[[k]] <- y[[j]]; j <- j + 1; k <- k + 1 }
#     else i <- i + 1
#   }
#   if (i > n) {
#     while(j <= m) { r[[k]] <- y[[j]]; j <- j + 1; k <- k + 1 }
#   } else if (j > m) {
#     while(i <= n) { r[[k]] <- x[[i]]; i <- i + 1; k <- k + 1 }
#   } 
#   r[vapply(r, Negate(is.null), logical(1))]
# }
# 
# sort_joined <- function(x, y) {
#   l <- c(x, y)
#   unique(l[order(unlist(l))])
# }
# 
# sort_joined_shell <- function(x, y) {
#   l <- c(x, y)
#   unique(l[order(unlist(l), method = "shell")])
# }
# 
# sort_joined_radix <- function(x, y) {
#   l <- c(x, y)
#   unique(l[order(unlist(l), method = "radix")])
# }
# 
# l1 <- as.list(cumsum(sample(20, 1e6, replace = TRUE)))
# l2 <- as.list(cumsum(sample(20, 1e6, replace = TRUE)))
# 
# m1 <- as.list(sample(1e6, replace = TRUE))
# m2 <- as.list(sample(1e6, replace = TRUE))
# 
# l1 <- as.list(sort(unlist(m1)))
# l2 <- as.list(sort(unlist(m2)))
# 
# b <- bench::mark(
#   join_sorted(l1, l2), 
#   sort_joined(l1, l2)
# )
# 
# bench::mark(sort_joined(l1, l2))
# bench::mark(sort_joined(m1, m2))
# 
# 
# let <- as.character(outer(letters, outer(letters, letters, paste0), paste0))
# let <- paste0(c("", "X", ")"), let)
# l1  <- as.list(sample(let, 2e6, replace = TRUE))
# l2  <- as.list(sample(let, 3e6, replace = TRUE))
# s1  <- as.list(sort(unlist(l1)))
# s2  <- as.list(sort(unlist(l2)))
# 
# # bench::mark(sort_joined(l1, l2))
# bench::mark(sort_joined (s1, s2))
# bench::mark(sort_joined2(s1, s2))
# bench::mark(join_sorted (s1, s2))
# 
# bench::mark(
#   sort_joined_shell(s1, s2), 
#   sort_joined_radix(s1, s2)
# )

#### ---------------------------------- CNF ------------------------------------

merge_cnf <- function(x, y) {
  merge(x, y, .tag = "cnf")
}

cross_cnf <- function(x, y) {
  l <- Reduce(c, Map(function(.x) Map(function(.y) list(.x, .y), x), y))
  l <- lMap(function(x) merge(x[[1]], x[[2]]), l)
  sort(lsen(l, op = `&`, tag = "cnf"))
}

as_cnf <- function(l) UseMethod("as_cnf")

as_cnf.llit <- function(l) {
  `&.logi`(`|.logi`(l), .tag = "cnf")
}

as_cnf.lsen <- function(l) {
  if (identical(tag(l), "cnf")) return(l)
  if (!identical(tag(l), "standard")) l <- standardize(l)
  l <- lMap(as_cnf, l)
  if (identical(op(l), `|`)) cross_cnf(l[[1]], l[[2]])
  else merge_cnf(l[[1]], l[[2]])
}

binarize <- function(l) {
  if (is_llit(l) || identical(tag(l), "binary")) return(l)
  `~` <- ifelse(not(l), `-`, identity)
  if (length(l) > 2) l <- ~Reduce(op(l), l, right = TRUE)
  lsen(Map(binarize, l), not(l), op(l), tag = "binary")
}

.lsimplify <- function(l) {
  if (is_llit(l)) return(l)
  if (op(l) %in2% list(`|`, `&`))
    w <- !unlist(Map(identical, l, c(l[-1], list(NULL))))
  else w <- rep(T, length(l))
  lsen(l[w], not(l), op(l), tag(l))
}

lsimplify <- function(l, sort = TRUE) {
  if (sort) l <- sort(l)
  if (!is_llit(l)) l <- lMap(function(x) lsimplify(x, FALSE), l)
  .lsimplify(l)
}
