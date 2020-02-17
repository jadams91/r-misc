
#### --------------------------- DISTINCT S3 CLASS ----------------------------

# helper methods

`%~=%` <- function(xs, ys) {
  d <- (xs - ys) ^ 2
  d == min(d)
}

dif <- function(xs) {
  n = length(xs)
  dxs = (c(xs[2:n], xs[n]) - c(xs[1], xs[1:(n - 1)])) / 2
  dxs
}

is_increasing <- function(xs) {
  as.logical(prod(dif(xs) > 0))
}

yex <- function(dnt) {
  if (attr(dnt, "constant")) attr(dnt, "data")$Y[[1]]
  else attr(dnt, "data")$Y
}

`$.distinct` <- function(x, idx = c("X", "Y")) {
  attr(x, "data")[[idx]]
}

`$<-.distinct` <- function(x, idx = c("X", "Y"), value) {
  d <- new_distinct(x$X, x$Y)
  attr(d, "data")[[idx]] <- value
  d
}


# constructor
new_distinct <- function(xs, ys, str = NULL, const = F) {
  stopifnot(is.numeric(xs))
  stopifnot(is.numeric(ys))
  
  d1 = dif(ys) / dif(xs)
  d2 = dif(d1) / dif(xs)
  
  e <- new.env()
  e$X  <- xs
  e$Y  <- ys
  e$D1 <- d1
  e$D2 <- d2
  
  fun <- function(x, yex = F) {
    x <- as_distinct(x)
    X <- attr(x, "data")$X
    Y <- attr(x, "data")$Y
    c <- attr(x, "constant")
    
    n <- vapply(Y, function(x) which(e$X %~=% x)[[1]], numeric(1))
    d <- Y - e$X[n]

    val <- e$Y[n] + e$D1[n] * d + 0.5 * e$D2[n] * d ^ 2
    res <- new_distinct(X, val, const = c)
    if (yex) yex(res) else res
  }
  
  class(fun)            <- c("distinct", "function")
  attr(fun, "data")     <- e
  attr(fun, "constant") <- const
  if (const) attr(fun, "structure") <- as.character(e$Y[[1]])
  else attr(fun, "structure") <- str
  
  fun
}

# validator
validate_distinct <- function(dnt) {
  dat <- attr(dnt, "data")
  
  if (length(dat$X) != length(dat$Y)) {
    stop(
      "The distinct has an ambiguous mapping",
      call. = FALSE
    )
  }
  if (!is_increasing(dat$X)) {
    stop(
      "The distinct has non-increasing arguments",
      call. = FALSE
    )
  }
  if (sum(is.na(dat$Y) || is.nan(dat$Y))) {
    warning(
      "The distinct mapping has missing values",
      call. = FALSE
    )
  }
  
  dnt
}

# helper
distinct <- function(f = function(x) rep_len(0, length(x)), 
                     xs = seq(-10, 10, length.out = 1022)) {
  f  <- as.function(f)
  xs <- as.numeric(xs)
  
  validate_distinct(new_distinct(xs, f(xs)))
}

new_var <- function(xs = seq(-10, 10, length.out = 1022)) {
  distinct(function(x) x, xs)
}


# generics

is_distinct <- function(x) {
  "distinct" %in% class(x)
}

as_distinct <- function(x) {
  if (is_distinct(x)) return(x)
  if (is.numeric(x) && length(x) == 1) {
    return(new_distinct(c(0, 1), c(x, x), const = T))
  }
  if (is.numeric(x)) {
    return(new_distinct(seq_along(x) / length(x), x))
  }
  stop(
    "Cannot convert given type to a distinct",
    call. = FALSE
  )
}

print.distinct <- function(x) {
  print(attr(x, "data")$Y)
}

plot.distinct <- function(x) {
  dat <- attr(x, "data")
  plot(dat$X, dat$Y, type = 'l', xlab = "X", ylab = "Y")
}


# arithmetics

`+.distinct` <- function(x, y) {
  if (missing(y)) return(x)
  
  if (is.numeric(x)) {
    xs <- attr(y, "data")$X
  } else if (is.numeric(y)) {
    xs <- attr(x, "data")$X
  } else {
    xs <- union(attr(x, "data")$X, attr(y, "data")$X)
  }
  
  x <- as_distinct(x)
  y <- as_distinct(y)
  
  validate_distinct(new_distinct(xs, x(xs, T) + y(xs, T)))
}

`-.distinct` <- function(x, y) {
  if (missing(y)) return(-1 * x)
  
  if (is.numeric(x)) {
    xs <- attr(y, "data")$X
  } else if (is.numeric(y)) {
    xs <- attr(x, "data")$X
  } else {
    xs <- union(attr(x, "data")$X, attr(y, "data")$X)
  }
  
  x <- as_distinct(x)
  y <- as_distinct(y)
  
  validate_distinct(new_distinct(xs, x(xs, T) - y(xs, T)))
}

`*.distinct` <- function(x, y) { 
  if (is.numeric(x)) {
    xs <- attr(y, "data")$X
  } else if (is.numeric(y)) {
    xs <- attr(x, "data")$X
  } else {
    xs <- union(attr(x, "data")$X, attr(y, "data")$X)
  }
  
  x <- as_distinct(x)
  y <- as_distinct(y)
  
  validate_distinct(new_distinct(xs, x(xs, T) * y(xs, T)))
}

`/.distinct` <- function(x, y) { 
  if (is.numeric(x)) {
    xs <- attr(y, "data")$X
  } else if (is.numeric(y)) {
    xs <- attr(x, "data")$X
  } else {
    xs <- union(attr(x, "data")$X, attr(y, "data")$X)
  }
  
  x <- as_distinct(x)
  y <- as_distinct(y)
  
  validate_distinct(new_distinct(xs, x(xs, T) / y(xs, T)))
}

`^.distinct` <- function(x, y) { 
  if (is.numeric(x)) {
    xs <- attr(y, "data")$X
  } else if (is.numeric(y)) {
    xs <- attr(x, "data")$X
  } else {
    xs <- union(attr(x, "data")$X, attr(y, "data")$X)
  }
  
  x <- as_distinct(x)
  y <- as_distinct(y)
  
  validate_distinct(new_distinct(xs, x(xs, T) ^ y(xs, T)))
}

exp.distinct <- function(x) {
  exp(1) ^ x
}


`%^%` <- function(x = distinct(), y = integer()) {
  stopifnot(is.numeric(y) && as.integer(y) == y)
  
  if (y == 0) return(x)
  
  X <- attr(x, "data")$X
  Y <- attr(x, "data")$Y
  n <- length(X)
  rem <- c(-1, -n)
  
  if (y > 0) {
    Yprim <- dif(Y) / dif(X)
    new_distinct(X[rem], Yprim[rem]) %^% (y - 1)
  } else {
    Yprim <- cumsum(Y * dif(X))
    new_distinct(X, Yprim - mean(Yprim)) %^% (y + 1)
  }
  
}
