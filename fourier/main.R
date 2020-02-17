setwd("/home/sebastian/Documents/psnc/Fourier Analysis")
source("fun.R")


# ------------------------- DIRAC DELTA FUNCTION ------------------------------
source("dirac.R")

L  <- 10
xs <- seq(-L, L, 0.01)

ddf <- dirac(xs, 2)
ys  <- xs ^ 2

integral(xs, ddf)
integral(xs, ddf * ys)
plot(xs, ddf, type = "l")


# ---------------------------- FOURIER SERIES ---------------------------------
source("series.R")

L  <- 1
xs <- seq(-L, L, 0.01)

m <- 2
n <- 2
integral(xs, cos(xs * m * pi / L) * cos(xs * n * pi / L)) # = L
integral(xs, sin(xs * m * pi / L) * sin(xs * n * pi / L)) # = L
integral(xs, cos(xs * m * pi / L) * sin(xs * n * pi / L)) # = 0
n <- 3
integral(xs, cos(xs * m * pi / L) * cos(xs * n * pi / L)) # = 0
integral(xs, sin(xs * m * pi / L) * sin(xs * n * pi / L)) # = 0

ys  <- exp(-abs(xs))
cf1 <- sine_coefficients(xs, ys, 1)
cf5 <- sine_coefficients(xs, ys, 5)
series1 <- calculate_sine(cf1, xs)
series2 <- calculate_sine(cf5, xs)
plot(xs, ys, type = "l")
lines(series1$xs, series1$ys, type = "l", col = "blue")
lines(series2$xs, series2$ys, type = "l", col = "red")

ys <- xs ^ 2
cf <- sine_coefficients(xs, ys, 10)
series <- calculate_sine(cf, xs, c(-5, 5))
barplot(cf$as, ylim = 1.5 * c(min(cf$as), max(cf$as)))
gs = seq(-5, 5, 0.1)
plot(gs, gs ^ 2, xlim = c(-5, 5), ylim = c(0, 2), type = "l")
lines(series$xs, series$ys, type = "l", col = "red")


# ------------------------- COMPLEX FOURIER SERIES ----------------------------
source("series_complex.R")

L  <- 1
xs <- seq(-L, L, 0.01)

m <- 2
n <- 2
integral(xs, exp(xs * 1i * m * pi / L) * exp(xs * -1i * n * pi / L)) # = 2L
n <- 3
integral(xs, exp(xs * 1i * m * pi / L) * exp(xs * -1i * n * pi / L)) # = 0

ys  <- xs ^3 - xs ^ 2
ccf <- complex_coefficients(xs, ys, 5)
Im(ccf + rev(ccf)) # = 0, as cf[m] == Conj(cf[-m])
series <- calculate_complex(ccf, xs)
plot(xs, ys, type = "l")
lines(series$xs, Re(series$ys), type = "l", col = "red")

xs_off <- seq(-L, L, length.out = 1000)
ys     <- 1 / (4 * sqrt(abs(xs_off)))
ccf    <- complex_coefficients(xs_off, ys, 15)
series <- calculate_complex(ccf, xs_off)
plot(xs_off, ys, xlim = c(0, 1), ylim = c(0, 3), type = "l")
lines(series$xs, Re(series$ys), type = "l", col = "red")
ys_p   <- derivative(xs_off, ys)
s_ys_p <- derivative(series$xs, series$ys)
plot(xs_off, ys_p, xlim = c(0, 1), ylim = c(-10, 10), type = "l")
lines(series$xs, Re(s_ys_p), type = "l", col = "red")


# -------------------------- GIBB'S PHENOMENON --------------------------------

L  <- 1
xs <- seq(-L, L, length.out = 2000)

ys  <- sign(xs)
err <- numeric()
for (i in (seq(10, 250, 10))) {
  ccf    <- complex_coefficients(xs, ys, i)
  series <- calculate_complex(ccf, xs)
  if (i %in% c(10, 50, 100, 250)) {
    plot(xs, ys, type = "l", ylim = c(-1.2, 1.2))
    lines(series$xs, Re(series$ys), type = "l", col = "red")
  }
  err <- c(err, series_error(xs, ys, series$ys))
}
barplot(err)


# ------------------------- PARSEVAL'S THEOREM --------------------------------

L  <- 1
xs <- seq(-L, L, length.out = 5000)

ys <- 1 / (xs + 2L) ^ 2
ccf    <- complex_coefficients(xs, ys, 500)
Re(sum(ccf * Conj(ccf)))
integral(xs, ys * Conj(ys)) / (2 * L)

pi_apx  <- (90 * cumsum(1 / (1:10000) ^ 4)) ^ (1/4)
pi_diff <- pi - pi_apx
plot(pi_diff, log = "y", type = "l")

