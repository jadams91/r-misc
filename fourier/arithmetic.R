
#### --------------------------- VARIABLE S3 CLASS ----------------------------

new_variable <- function(v = character()) {
  stopifnot(is.character(v))
  
  structure(
    v, 
    class = "variable"
  )
}

validate_variable <- function(vr) {
  val <- unclass(vr)
  
  if (!(length(val) == 1) || is.na(val)) {
    stop(
      "The symbol must be a non-NA character vector of length 1",
      call. = FALSE
    )
  }
  
  vr
}

variable <- function(v = character()) {
  v <- as.character(v)
  
  validate_variable(new_variable(v))  
}

is_variable <- function(x) {
  class(x) == "variablle"
}

print.variable <- function(x = variable()) {
  print(as.character(x))
}


#### ----------------------------- ARFUN S3 CLASS -----------------------------

new_arfun <- function(v = character()) {
  stopifnot(is.character(v))
  
  structure(
    v, 
    class = "variable"
  )
}

new_basefun <- function(f) {
  stopifnot(is.function(f))
  
  class(f) <- c("basefun", "arfun", "function")
  
  f
}

new_var <- function() {
  new_basefun(function(x) x)
}



is_arfun <- function(f) {
  "arfun" %in% class(f)
}

`+` <- new_basefun(function(x, y) {
  if (is_arfun(x) || is_arfun(y)) {
    stopifnot(is_arfun(x) || is.numeric(x))
    stopifnot(is_arfun(y) || is.numeric(y))
    
    if (is.numeric(x)) x <- new_const(function() x)
    if (is.numeric(y)) y <- new_const(function() y)
    
    
    
  } else {
    base::`+`(x, y)
  }
})

ff <- function(x) {
  a <- x
  function() a
}

new_arfun <- function(f) {
  stopifnot(is.function(f))
  
  fun <- function(...) {
    if (prod(vapply(l <- list(...), is_ar, F)) && length(l) > 0) {
      stopifnot(length(l) == length(formals(f)))
      l <- lapply(l, as_arfun)
      
      
    } else {
      f(...)
    }
  }
  
  class(fun) <- c("arfun", "function")
  if (length(formals(fun)) == 0) {
    attr(f, "tree") <- fun()
  } else {
    attr(f, "tree") <- fun
  }
}


ff <- function(f) {
  function(...) {
    if (prod(vapply(l <- list(...), is.numeric, F)) && length(l) > 0) {
      l <- lapply(l, as.logical)
      l
    } else {
      f(...)
    }
  }
}

f <- function(...) {
  stopifnot(product(as.c))
}


new_arfun <- function(tree, args) {
  fun <- function(...) {
    stopifnot(length(ll <- list(...)) == args)
    evaluate(tree, ll)
  }
  
  class(fun) <- c("arfun", "function")
  attr(fun, "tree") <- tree
  attr(fun, "args") <- args
  
  fun
}

evaluate <- function(tree, ll) {
  
}
