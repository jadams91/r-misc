library(magrittr)

new_wworld <- function() {
  structure(
    list(
      wumpus = sample(1:16, 1), 
      gold   = sample(1:16, 1), 
      pits   = which(matrix(runif(16)) > 0.8)
    ), 
    class = "wworld"
  )
}

validate_wworld <- function(w) {
  x <- unclass(w)
  if (!length(intersect(c(x$wumpus, x$gold), x$pits))) {
    return(w)
  } else {
    stop(
      "World generation failed, please try again",
      call. = FALSE
    )
  }
    
}

wworld <- function() {
  validate_wworld(new_wworld())
}

print.wworld <- function(w) {
  for (i in 1:4) {
    for (j in 1:4) {
      k <- 4 * (j - 1) + i
      if ((k == w$wumpus) && (k == w$gold)) {
        cat("@ ")
      } else if (k == w$wumpus) {
        cat("W ")
      } else if (k == w$gold) {
        cat("G ")
      } else if (k %in% w$pits) {
        cat("P ")
      } else {
        cat("X ")
      }
    }
    cat("\n")
  }
}

neighbours <- function(x) {
  m = matrix(1:16, nrow = 4)
  coord <- which(m == x, arr.ind = T)
  res <- numeric()
  for (v in list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))) {
    if (prod(coord + v > 0) && prod(coord + v < 5)) {
      res <- c(res, m[coord + v])
    }
  }
  sort(res)
}

get_state <- function(x, w) {
  state <- list()
  if (w$gold == x) { 
    state <- c(state, list(glitter = T))
  } else {
    state <- c(state, list(glitter = F))
  }
  if (w$wumpus %in% neighbours(x)) {
    state <- c(state, list(stench = T))
  } else {
    state <- c(state, list(stench = F))
  }
  if (length(intersect(w$pits, neighbours(x)))) {
    state <- c(state, list(breeze = T))
  } else {
    state <- c(state, list(breeze = F))
  }
  state
}


assign_dispatch <- function(fun, envir = globalenv()) {
  function(x, idx, value) {
    assign(x, fun(get(x), idx, value), envir = envir)
  }
}

sub_assign <- assign_dispatch(`[[<-`)

setwd("/home/marco/Documents/PSNC/2019")
source("logic.R")

for (ch in c('s', 'b', 'W', 'G', 'P')) {
  assign(ch, vector("list", 16))
  for (i in seq_len(16)) {
    sym <- paste0(ch, i)
    sub_assign(ch, i, literal(sym))
  }
}

rules <- list()
map <- c(W = 's', P = 'b', s = 'W', b = 'P')
for (ch in c('s', 'b')) {
  for (i in seq_len(16)) {
    cs  <- lapply(neighbours(i), function(x) get(map[[ch]])[[x]])
    dis <- sentence(operator('or'), clauses = cs)
    new_rule <- get(ch)[[i]] %<=>% dis
    rules <- c(rules, list(new_rule))
  }
}

all <- list(s, b, W, G, P)
all <- unlist(lapply(symbols, unlist))
symbols <- rep_len(NA, length(all))
names(symbols) <- all
wumpus_kb <- knowledge_base(symbols, rules)
