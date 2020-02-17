differential <- function(xs) {
  n = length(xs)
  dxs = (c(xs[2:n], xs[n]) - c(xs[1], xs[1:(n - 1)])) / 2
  return(dxs)
}

integral <- function(xs, ys) {
  dxs = differential(xs)
  return(sum(ys * dxs))
}

derivative <- function(xs, ys) {
  dxs = differential(xs)
  dys = differential(ys)
  return(dys / dxs)
}

series_error <- function(xs, f_ys, s_ys) {
  ys = Re((f_ys - s_ys) * Conj(f_ys - s_ys))
  return(integral(xs, ys))
}
