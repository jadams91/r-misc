library(magrittr)

setwd("/home/marco/Documents/PSNC/2019/Fourier Analysis")
source("arfun.R")
source("map.R")



`%*%` <- function(f, g) {
  fun <- function(x) {
    int <- function(x) {
      h <- function(y) f(y) * g(x - y)
      integrate(h, -Inf, Inf)$value
    }
    
    vapply(x, int, numeric(1))
  }
  
  arfun(fun)
}

make_tophat <- function(width = 1, height = 1 / width) {
  fun <- function(x) {
    f <- function(x) {
      if (x > -width / 2 && x < width / 2) height
      else 0
    }
    
    vapply(x, f, numeric(1))
  }
  
  arfun(fun)
}

make_gaussian <- function(sig = 1) {
  fun <- function(x) {
    f <- function(x) {
      (2 * pi * sig ^ 2) ^ (-1 / 2) * exp(-x ^ 2 / (2 * sig ^ 2))
    }
    
    vapply(x, f, numeric(1))
  }
  
  arfun(fun)
}

der <- function(a, delta = 1e-5) {
  fun <- function(x) (a(x + delta) - a(x - delta)) / (2 * delta)
  arfun(fun)
}

int <- function(a, alt = F) {
  if (alt) return(integrate3(a))
  
  fun <- function(x) {
    f <- function(x) integrate(a, -Inf,  x)$value
    vapply(x, f, numeric(1)) 
  }
  
  arfun(fun)
}

integrate2 <- function(f, rg = get_gd(), n = 100000) {
  x  <- seq(min(rg), max(rg), length.out = n)
  dx <- (max(rg) - min(rg)) / n
  sum(f(x) * dx)
}

integrate3 <- function(f, rg = get_gd(), n = 100000) {
  x  <- seq(min(rg), max(rg), length.out = n)
  dx <- (max(rg) - min(rg)) / n
  y <- cumsum(f(x) * dx)
  arfun(map(x, y))
}

diracA <- make_tophat(0.01)
diracB <- make_gaussian(0.01)





