
#### ------------------------ GLOBAL DISCRETE DOMAIN --------------------------

set_gdd <- function(x = seq(-10, 10, length.out = 1000)) {
  stopifnot(is.numeric(x))
  .gdd <<- x
}

get_gdd <- function() {
  if (exists(".gdd")) .gdd
  else stop("Run set_gdd() first!", call. = FALSE)
}


#### -------------------------- DISCRETE S3 CLASS -----------------------------

new_discrete <- function(f, xs, body = NULL) {
  stopifnot(is_unary_function(f))
  stopifnot(is.numeric(xs))
  
  e <- new.env()
  e$f <- f
  e$x <- xs
  e$y <- f(xs)

  
  if (is.null(body)) body <- get_body(f)
  e$bd <- body
  
  fun <- function(d) {
    d <- as_discrete(d)
    new_discrete(function(x) e$f(d$f(x)), d$x, 
                 paste0(e$bd, d$bd))
  }
  
  class(fun)       <- c("discrete", "function")
  attr(fun, "env") <- e
  
  fun
}

discrete <- function(f = function(x) x, dd = get_gdd(), body = NULL) {
  f  <- as.function(f)
  dd <- as.numeric(dd)
  
  new_discrete(f, dd, body)
}


#### -------------------------- DISCRETE FUNCTIONS ----------------------------

`$.discrete` <- function(x, idx = c("f" ,"x", "y")) {
  attr(x, "env")[[idx]]
}

is_unary_function <- function(f) {
  if (is.primitive(f)) {
    tryCatch(
      error = function(cnd) FALSE, 
      { invisible(f(FALSE)); TRUE }
    )
  } else {
    length(formals(f)) == 1
  }
}

get_body <- function(f) {
  stopifnot(is_unary_function(f))
  
  if (is.primitive(f)) {
    bd <- deparse(substitute(f))
    return(paste0('(', bd, '(x)', ')'))
  } else {
    bd <- deparse(body(f))
    return(paste0('(', bd, ')'))
  }
}

is_discrete <- function(x) {
  "discrete" %in% class(x)
}

as_discrete <- function(y, body = NULL) {
  if (is_discrete(y)) { 
    y
  } else if (is.function(y)) {
    new_discrete(y, get_gdd(), body = body)
  } else if (is.numeric(y) && length(y) == 1) {
    xs <- get_gdd()
    new_discrete(function(x) vapply(x, function(x) y, numeric(1)), 
                 xs, body = paste0('(', y, ')'))
  } else {
    stop(
      "Cannot convert given type to a discrete",
      call. = FALSE
    )
  }
}

`%<%` <- function(d, v) {
  stopifnot(is_discrete(d))
  d$f(v)
}


#### -------------------------- DISCRETE GENERICS -----------------------------

as.character.discrete <- function(d) {
  as.character(d$bd)
}

print.discrete<- function(d) {
  print(as.character(d))
}

plot.discrete <- function(d) {
  plot(d$x, d$y, type = 'l', xlab = "x", ylab = "y")
}


#### ---------------------------- DISCRETE MATH -------------------------------

`+.discrete` <- function(a, b) {
  if (missing(b)) return(a)
  
  a <- as_discrete(a)
  b <- as_discrete(b)
  
  new_discrete(function(x) (a %<% x) + (b %<% x), 
               get_gdd(), paste0('(', a, ' + ', b, ')'))
}

`-.discrete` <- function(a, b) {
  if (missing(b)) return(-1 * a)
  
  a <- as_discrete(a)
  b <- as_discrete(b)
  
  new_discrete(function(x) (a %<% x) - (b %<% x), 
               get_gdd(), paste0('(', a, ' - ', b, ')'))
}

`*.discrete` <- function(a, b) {
  a <- as_discrete(a)
  b <- as_discrete(b)
  
  new_discrete(function(x) (a %<% x) * (b %<% x), 
               get_gdd(), paste0('(', a, ' * ', b, ')'))
}

`/.discrete` <- function(a, b) {
  a <- as_discrete(a)
  b <- as_discrete(b)
  
  new_discrete(function(x) (a %<% x) / (b %<% x), 
               get_gdd(), paste0('(', a, ' / ', b, ')'))
}

`^.discrete` <- function(a, b) {
  a <- as_discrete(a)
  b <- as_discrete(b)
  
  new_discrete(function(x) (a %<% x) ^ (b %<% x), 
               get_gdd(), paste0('(', a, ' ^ ', b, ')'))
}

sin.discrete <- function(a) {
  new_discrete(function(x) sin(a %<% x), 
               get_gdd(), paste0('(sin', a, ')'))
}

cos.discrete <- function(a) {
  new_discrete(function(x) cos(a %<% x), 
               get_gdd(), paste0('(cos', a, ')'))
}

tan.discrete <- function(a) {
  new_discrete(function(x) tan(a %<% x), 
               get_gdd(), paste0('(tan', a, ')'))
}

exp.discrete <- function(a) {
  new_discrete(function(x) exp(a %<% x), 
               get_gdd(), paste0('(exp', a, ')'))
}


#### -------------------------- DISCRETE CALCULUS -----------------------------

dif <- function(x) {
  n = length(x)
  (c(x[2:n], x[n]) - c(x[1], x[1:(n - 1)])) / 2
}

dintegrate <- function(a) {
  sum(a$y * dif(a$x))
}

make_dirac <- function(sig) {
   dirac <- function(x) (2 * pi * sig ^ 2) ^ (-1 / 2) * exp(-x ^ 2 / (2 * sig ^ 2))
   as_discrete(dirac, "(dir(x))")
}
