
#### ----------------------------- GLOBAL DOMAIN ------------------------------

set_gd <- function(x = c(-10, 10)) {
  stopifnot(is.numeric(x) && length(x) == 2)
  .gd <<- x
}

get_gd <- function() {
  if (exists(".gd")) .gd
  else stop("Run set_gd() first!", call. = FALSE)
}


#### -------------------------- ANALYTIC S3 CLASS -----------------------------

new_analytic <- function(f) {
  stopifnot(is.function(f))
  
  e   <- new.env()
  e$f <- f
  
  fun <- function(a) {
    a <- analytic(a)
    f <- function(x) {
      g <- function(x) e$f(a[x])
      vapply(x, g, numeric(1))
    }
    
    analytic(f)
  }
  
  class(fun)       <- c("analytic", "function")
  attr(fun, "env") <- e
  fun
}

analytic <- function(f = function(x) x) {
  f <- as.function(f)
  new_analytic(f)
}


#### -------------------------- ANALYTIC FUNCTIONS ----------------------------

`[.analytic` <- function(a, x) {
  attr(a, "env")$f(x)
}

as.function.numeric <- function(a) {
  function(x) {
    f <- function(x) a
    vapply(x, f, numeric(1))
  }
}

is_analytic <- function(a) {
  "analytic" %in% class(a)
}

as_analytic <- function(a) {
  if (is_analytic(a)) a
  else analytic(a)
}

plot.analytic <- function(a, range = get_gd(), n = 1000, ...) {
  f <- as.function(a)
  plot.function(f, from = range[[1]], to = range[[2]], 
                n = n, xlab = "x", ylab = "y", ...)
}

as.function.analytic <- function(a) {
  function(x) a[x]
}


#### ---------------------------- ANALYTIC MATH -------------------------------

`+.analytic` <- function(a, b) {
  if (missing(b)) return(a)
  a <- as_analytic(a)
  b <- as_analytic(b)
  
  analytic(function(x) a[x] + b[x])
}

`-.analytic` <- function(a, b) {
  if (missing(b)) return(-1 * a)
  a <- as_analytic(a)
  b <- as_analytic(b)
  
  analytic(function(x) a[x] - b[x])
}

`*.analytic` <- function(a, b) {
  a <- as_analytic(a)
  b <- as_analytic(b)
  
  analytic(function(x) a[x] * b[x])
}

`/.analytic` <- function(a, b) {
  a <- as_analytic(a)
  b <- as_analytic(b)
  
  analytic(function(x) a[x] / b[x])
}

`^.analytic` <- function(a, b) {
  a <- as_analytic(a)
  b <- as_analytic(b)
  
  analytic(function(x) a[x] ^ b[x])
}

sin.analytic <- function(a) {
  analytic(function(x) sin(a[x]))
}

cos.analytic <- function(a) {
  analytic(function(x) cos(a[x]))
}

tan.analytic <- function(a) {
  analytic(function(x) tan(a[x]))
}

exp.analytic <- function(a) {
  analytic(function(x) exp(a[x]))
}


#### -------------------------- ANALYTIC CALCULUS -----------------------------

integrate <- function(f, ...) {
  UseMethod("integrate")
}

integrate.analytic <- function(a, lower = -Inf, upper = Inf, ...) {
  f <- as.function(a)
  stats::integrate(f, lower, upper, stop.on.error = F, ...)
}

integrate.default <- function(...) {
  stats::integrate(...)
}
