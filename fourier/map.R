
mdv <- function(x, y, n) {
  stopifnot((l <- length(x)) > n)
  
  f <- function(n) {
    for (i in l:(l - n + 1)) {
      y <- diff(y) / diff(x)
      x <- (x[2:i - 1] + x[2:i]) / 2
    }
    
    median(y)
  }
  
  vapply(n, f, numeric(1))
}

`%~%` <- function(xs, ys) {
  d <- (xs - ys) ^ 2
  d == min(d)
}

dif <- function(xs) {
  n = length(xs)
  dxs = (c(xs[2:n], xs[n]) - c(xs[1], xs[1:(n - 1)])) / 2
  dxs
}

`%:%` <- function(a, b) {
  if (b < a) numeric()
  else a:b
}

map <- function(xs, ys, d = 2) {
  stopifnot(is.numeric(xs))
  stopifnot(is.numeric(ys))
  stopifnot(length(xs) == length(ys))
  
  fun <- function(x) {
    dlist <- vector("list", d)
    dlist[[1]] <- dif(ys) / dif(xs)
    for (i in 2%:%d) dlist[[i]] <- dif(dlist[[i - 1]]) / dif(xs)
    
    f <- function(x) {
      i  <- which(xs %~% x)[[1]]
      dx <- x - xs[[i]]
      ds <- vapply(1%:%d, function(d) dlist[[d]][[i]], numeric(1))
      
      ys[[i]] + sum(1 / factorial(1%:%d) * ds * dx ^ (1%:%d))
    }
    
    vapply(x, f, numeric(1))
  }
  
  fun
}

# fun <- function(x, d = 2) {
#   f <- function(x) {
#     i <- which(xs %~% x)[[1]]
#     dx <- x - xs[[i]]
#     if ((low <- i - floor(d / 2)) < 1) low <- 1
#     if ((high <- i + ceiling(d / 2)) > length(xs)) high <- length(xs)
#     sx <- xs[low:high]
#     sy <- ys[low:high]
#     d <- high - low
# 
#     ys[[i]] + sum(1 / factorial(1:d) * mdv(sx, sy, 1:d) * dx ^ (1:d))
#   }
# 
#   vapply(x, f, numeric(1))
# }

f <- function(x) sin(tan((pi / 2 - 0.01) * exp(-0.01 * (x - 5) ^ 2)))

for(i in 0:10) {
  x <- seq(0, 10, length.out = 100000)
  t <- runif(100, 1, 100)
  m <- map(x, f(x), i)
  
  tst <- vapply(t, m, numeric(1))
  act <- vapply(t, f, numeric(1))
  
  print(mean((tst - act) ^ 2))
}


