library(profvis)

setwd("/home/sebastian/Documents/2019")
source("profiler_example.R")
profvis(f())

profvis({
  x <- integer()
  for (i in 1:1e5) {
    x <- c(x, i)
  }
})

# ERROR D: 
profvis({
  f <- function(n = 1e5) {
    x <- rep(1, n)
    rm(x)
  }
}, torture = TRUE)
