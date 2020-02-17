create_random_df <- function(nrow, ncol) {
  random_matrix <- matrix(runif(nrow * ncol), nrow = nrow)
  as.data.frame(random_matrix)
}

subtract_medians <- function(x, medians){
  for (i in seq_along(medians)) {
    x[[i]] <- x[[i]] - medians[[i]]
  }
  x
}

subtract_medians_l <- function(x, medians){
  x <- as.list(x)
  x <- subtract_medians(x, medians)
  as.data.frame(x)
}

compare_speed <- function(ncol){
  df_input   <- create_random_df(nrow = 1e4, ncol = ncol)
  medians <- vapply(df_input, median, numeric(1))
  
  bench::mark(`Data Frame` = subtract_medians(df_input, medians),
              List = as.data.frame(subtract_medians(as.list(df_input), medians)))
}

results <- bench::press(
  ncol = c(1, 5, 10, 50, 100, 200, 400, 600, 800, 1000, 1500),
  compare_speed(ncol)
)

library(ggplot2)
ggplot(results, aes(ncol, median, col = result)) +
  geom_point(size = 2) + 
  geom_smooth() +
  labs(x = "Number of Columns of Input Data", y = "Computation Time",
       color = "Input Data Structure",
       title = "Benchmark: Median Subtraction")
