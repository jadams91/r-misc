library(rlang)

##### ENVIRONMENT BASICS #####

e1 <- env(
  a = FALSE, 
  b = "a", 
  c = 2.3, 
  d = 1:3
)

e1$d <- e1

env_print(e1)
env_names(e1)

# instead of == 
identical(global_env(), current_env())

e2a <- env(d = 4, e = 5)
e2b <- env(e2a, a = 1, b = 2, c = 3)

env_parent(e2b)
env_parent(e2a)

e2c <- env(empty_env(), d = 4, e = 5)
e2d <- env(e2c, a = 1, b = 2, c = 3)

env_parents(e2b)
env_parents(e2d)
env_parents(e2b, last = empty_env())


# super assignment
x <- 0
f <- function(x) x <<- 1
f()
x


# getting and setting
e3 <- env(x = 1, y = 2)
e3$x
e3$z <- 3
e3[["z"]]

e3$xyz # NULL, use env_get() instead
env_get(e3, "xyz", default = NA)
env_poke(e3, "a", 100)
e3$a
env_bind(e3, a = 10, b = 20)
env_names(e3)

env_has(e3, "a")
e3$a <- NULL # doesn't work
env_has(e3, "a")
env_unbind(e3, "a")
env_has(e3, "a")


# advanced bindings
env_bind_lazy(current_env(), b = {Sys.sleep(1); 1})
system.time(print(b))
system.time(print(b))

env_bind_active(current_env(), z1 = function(val) runif(1))
z1
z1


# exercises

# 2
e1 <- env()
e1$loop <- e1

# 3
e1 <- env()
e2 <- env()
e1$loop   <- e2
e2$dedoop <- e1

# 5
env_poke2 <- function(env, name, value) {
  if (env_has(env, name)) {
    abort(paste0("\"", name, "\" is already assigned to a value."))
  }
  
  env_poke(env, name, value)
  invisible(env)
}

# 6
rebind <- function(name, value, env = caller_env()) {
  if (identical(env, empty_env())) {
    stop("Can't find `", name, "`", call. = FALSE)
  } else if (env_has(env, name)) {
    env_poke(env, name, value)
  } else {
    rebind(name, value, env_parent(env))
  }
}


##### RECURSING OVER ENVIRONMENTS #####

where <- function(name, env = caller_env()) {
  if (identical(env, empty_env())) {
    # Base case
    stop("Can't find ", name, call. = FALSE)
  } else if (env_has(env, name)) {
    # Success case
    env
  } else {
    # Recursive case
    where(name, env_parent(env))
  }
}
