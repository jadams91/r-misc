dirac <- function(xs, d = 0) {
  N = length(xs)
  ys = rep(0, N)
  
  idx = (1:N)[abs(xs - d) == min(abs(xs - d))][1]
  r = (xs[idx + 1] - xs[idx - 1]) / 2
  
  ys[idx] = 1 / r
  return(ys)
}
