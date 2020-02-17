

set_gd()
x <- arfun()

# ------------------------- DIRAC DELTA FUNCTION ------------------------------

diracA <- make_tophat(1e-3)
diracB <- make_gaussian(1e-3)

plot(diracA, get_gd() / 100)
plot(diracB, get_gd() / 100)

integrate2(diracA, n = 1e6)
integrate2(diracA %|% (x - 4) * 2 ^ x, n = 1e6)
integrate2(diracB, n = 1e6)
integrate2(diracB %|% (x - 4) * 2 ^ x, n = 1e6)


# ---------------------------- FOURIER SERIES ---------------------------------

L <- 10
f <- function(n) sin(n * pi * x / L)
g <- function(n) cos(n * pi * x / L)

plot(f(2) * f(2))
plot(g(2) * g(2))
plot(f(2) * g(2))
plot(f(2) * f(3))
plot(abs(f(3) * g(11)), n = 10000)

integrate(f(2) * f(2), -L, L) # L
integrate(g(2) * g(2), -L, L) # L
integrate(f(2) * g(2), -L, L) # 0
integrate(f(2) * f(3), -L, L) # 0
integrate(g(2) * g(3), -L, L) # 0
