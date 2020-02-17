
`%=>%`  <- function(e1, e2) !e1 | e2 
`%<=>%` <- function(e1, e2) (!e1 & !e2) | (e1 & e2) 
`%xor%` <- function(e1, e2) (!e1 & e2) | (e1 & !e2) 


form <- function(lg = logi()) attr(lg, "form", exact = TRUE)

new_logi <- function(val, not, form) {
  stopifnot(is.character(val) || is.list(val))
  stopifnot(is.logial(not) && length(not) == 1)
  stopifnot(is.character(form) && length(form) == 1)
  
  structure(
    val, 
    not   = not, 
    form  = form, 
    class = "logi"
  )
}

validate_logi <- function(lg) {
  val  <- unclass(lg)
  not  <- attr(lg, "not")
  form <- attr(lg, "form")
  
  if (form == "literal" && !(is.character(val) && length(val) == 1)) {
    stop(
      "The value of a literal must a character vector of length 1",
      call. = FALSE
    )
  } 
  
  if (form != "literal") {
    if (!(is.list(x) || (length(x) > 1) && is.function(list["op"]))) {
      stop(
        paste0("The value of a logical expression must be a list with an ", 
               "element fun being a logical function and at least one other ", 
               "logical element"),
        call. = FALSE
      )
    }
    for (i in 2:(length(val))) invisible(validate_logi(val[[i]]))
  }
  
  lg
}

literal <- function(val = character(), not = FALSE) {
  val <- as.character(val)
  not <- as.logical(not)
  
  validate_logi(new_logi(val, not, form = "literal"))
}

logi <- function(val = list(), not = FALSE, form = "binary") {
  val  <- as.list(val)
  not  <- as.logical(not)
  form <- as.character(form)
  
  validate_logi(new_logi(val, not, form))
}
