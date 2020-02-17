.c_coef = function(xs, ys, m) {
  L = max(xs)
  f = exp(xs * -1i * m * pi / L)
  return(integral(xs, f * ys) / (2 * L))
}

complex_coefficients = function(xs, ys, mmax) {
  # assumption: xs are symetrically distributed from -L to L
  cs = rep(NA, 2 * mmax + 1)
  
  for (m in -mmax:mmax) {
    cs[m + mmax + 1] = .c_coef(xs, ys, m)
  }
  
  return(cs)
}

calculate_complex = function(cs, xs, r = c(min(xs), max(xs)), l = length(xs)) {
  L = max(xs)
  M = (length(cs) - 1) / 2
  
  xs = seq(r[1], r[2], length.out = l)
  ys = 0
  
  for (m in -M:M) {
    ys = ys + cs[M + m + 1] * exp(xs * 1i * m * pi / L)
  }
  
  return(list("xs" = xs, "ys" = ys))
}
