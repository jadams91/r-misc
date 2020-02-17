integrate <- function(xs, ys, from = xs[1], to = xs[length(xs)]) {
  l <- length(xs)
  
  steps <- double(l)
  steps[1] <- (xs[2] - xs[1]) / 2
  for (i in 2:(l - 1)) {
    steps[i] <- (xs[i + 1] - xs[i - 1]) / 2
  }
  steps[l] <- (xs[l] - xs[l - 1]) / 2
  
  return(sum(steps * ys))
}

computeSeries <- function(xs, ys, L = max(xs) - min(xs), h = 20) {
  cf <- double(2 * h + 1)
  
  cf[1] <- integrate(xs, ys) / L
  
  for (n in 1:h) {
    cf[2 * n] <- integrate(xs, cos(n * pi / L * xs) * ys) / L
    cf[2 * n + 1] <- integrate(xs, sin(n * pi / L * xs) * ys) / L
  }
  
  return(cf)
}

drawSeries <- function(xs, cf, L = max(xs) - min(xs), h = (length(cf) - 1) / 2) {
  from = min(xs)
  to   = max(xs)
  
  newxs = seq(from, to, length.out = 100)
  newys = rep(0.5 * cf[1], 100)
  
  for (n in 1:h) {
    newys <- newys + cf[2 * n] * cos(n * pi / L * newxs)
    newys <- newys + cf[2 * n + 1] * sin(n * pi / L * newxs)
  }
  
  return(newys)
}
