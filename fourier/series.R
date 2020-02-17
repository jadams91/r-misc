.a_coef = function(xs, ys, m) {
  L = max(xs)
  f = cos(xs * m * pi / L)
  return(integral(xs, f * ys) / L)
}

.b_coef = function(xs, ys, m) {
  L = max(xs)
  f = sin(xs * m * pi / L)
  return(integral(xs, f * ys) / L)
}

sine_coefficients = function(xs, ys, mmax) {
  # assumption: xs are symetrically distributed from -L to L
  as = rep(NA, mmax)
  bs = rep(NA, mmax)
  
  a0 = .a_coef(xs, ys, 0)
  for (m in 1:mmax) {
    as[m] = .a_coef(xs, ys, m)
    bs[m] = .b_coef(xs, ys, m)
  }
  
  return(list("a0" = a0, "as" = as, "bs" = bs))
}

calculate_sine = function(cf, xs, r = c(min(xs), max(xs)), l = length(xs)) {
  L = max(xs)
  M = length(cf$as)
  
  xs = seq(r[1], r[2], length.out = l)
  ys = 0.5 * cf$a0
  
  for(m in 1:M) {
    ys = ys + cf$as[m] * cos(xs * m * pi / L)
    ys = ys + cf$bs[m] * sin(xs * m * pi / L)
  }
  
  return(list("xs" = xs, "ys" = ys))
}
