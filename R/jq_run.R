#' Apply a jq filter to an json input
#' @export
jrun <- function(filter, input = jq_input(filter), args = c("--compact-output", "--monochrome-output"), ...) {
  if(is.character(input)) {
    if(file.exists(input)){
      file <- input
      json <- NULL
    } else {
      file <- NULL
      json <- input
    }
  } else {
    json <- toJSON(input, ...)
    file <- NULL
  }
  filter <- paste(filter, collapse = " | ")
  args <- c(shQuote(filter), args)
  if(is.null(file))
    paste(system2("jq", args = args, stdout = T, input = json), collapse = "")
  else
    paste(system2("jq", args = args, stdout = T, stdin = file), collapse = "")
}

#' @export
jq <- function(input = NULL, filter = '.') {
  as.jq(filter, input = input)
}



