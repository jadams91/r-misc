

#### ----------------------------- GLOBAL DOMAIN ------------------------------

set_gd <- function(x = c(-10, 10)) {
  stopifnot(is.numeric(x) && length(x) == 2)
  .gd <<- x
}

get_gd <- function() {
  if (exists(".gd")) .gd
  else stop("Run set_gd() first!", call. = FALSE)
}


#### ---------------------------- ARFUN S3 CLASS ------------------------------

new_arfun <- function(f) {
  stopifnot(is.function(f))
  
  class(f) <- c("arfun", "function")
  f
}

arfun <- function(f = function(x) x) {
  f <- as.function(f)
  new_arfun(f)
}


#### ---------------------------- ARFUN FUNCTIONS -----------------------------

`%|%` <- function(a, b) {
  arfun(function(x) a(b(x)))
}

as.function.numeric <- function(a) {
  function(x) {
    f <- function(x) a
    vapply(x, f, numeric(1))
  }
}

is_arfun <- function(a) {
  prod(class(a) == c("arfun", "function"))
}

as_arfun <- function(a) {
  if (is_arfun(a)) a
  else arfun(a)
}

plot.arfun <- function(a, range = get_gd(), n = 1000, ...) {
  plot.function(a, from = range[[1]], to = range[[2]], 
                n = n, xlab = "x", ylab = "y", ...)
}

as.function.arfun <- function(a) {
  function(x) a(x)
}


#### ------------------------------ ARFUN MATH --------------------------------

`+.arfun` <- function(a, b) {
  if (missing(b)) return(a)
  a <- as_arfun(a)
  b <- as_arfun(b)
  
  arfun(function(x) a(x) + b(x))
}

`-.arfun` <- function(a, b) {
  if (missing(b)) return(-1 * a)
  a <- as_arfun(a)
  b <- as_arfun(b)
  
  arfun(function(x) a(x) - b(x))
}

`*.arfun` <- function(a, b) {
  a <- as_arfun(a)
  b <- as_arfun(b)
  
  arfun(function(x) a(x) * b(x))
}

`/.arfun` <- function(a, b) {
  a <- as_arfun(a)
  b <- as_arfun(b)
  
  arfun(function(x) a(x) / b(x))
}

`^.arfun` <- function(a, b) {
  a <- as_arfun(a)
  b <- as_arfun(b)
  
  arfun(function(x) a(x) ^ b(x))
}

sin.arfun <- function(a) {
  arfun(function(x) sin(a(x)))
}

cos.arfun <- function(a) {
  arfun(function(x) cos(a(x)))
}

tan.arfun <- function(a) {
  arfun(function(x) tan(a(x)))
}

exp.arfun <- function(a) {
  arfun(function(x) exp(a(x)))
}

abs.arfun <- function(a) {
  arfun(function(x) abs(a(x)))
}


#### ---------------------------- ARFUN CALCULUS ------------------------------

integrate <- function(f, ...) {
  UseMethod("integrate")
}

integrate.arfun <- function(a, lower = -Inf, upper = Inf, ...) {
  stats::integrate(a, lower, upper, stop.on.error = F, ...)
}

integrate.default <- function(...) {
  stats::integrate(...)
}
