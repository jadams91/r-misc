library(magrittr)

setwd("/home/marco/Documents/PSNC/2019/Fourier Analysis")
source("analytic.R")

make_tophat <- function(width = 1, height = 1 / width) {
  fun <- function(x) {
    f <- function(x) {
      if (abs(x) < width / 2) height
      else if (abs(x) == width / 2) height / 2
      else 0
    }
    
    vapply(x, f, numeric(1))
  }
  
  analytic(fun)
}

make_gaussian <- function(sig = 1) {
  fun <- function(x) {
    f <- function(x) {
      (2 * pi * sig ^ 2) ^ (-1 / 2) * exp(-x ^ 2 / (2 * sig ^ 2))
    }
    
    vapply(x, f, numeric(1))
  }
  
  analytic(fun)
}

`%*%` <- function(a, b) {
  fun <- function(x) {
    int <- function(x) {
      y <- analytic()
      c <- a(y) * b(x - y)
      integrate(c)$value
    }
    
    vapply(x, int, numeric(1))
  }
  
  analytic(fun)
}
