#' Apply a jq filter to an json input
#'
#' @param jq
#' @param args
#'
#' @return
#' @export
#' @import jsonlite
jq_run <- function(filter, json, file = NULL, args = c("--compact-output", "--monochrome-output")) {
  filter <- paste(filter, collapse = " | ")
  args <- c(shQuote(filter), args)
  if(is.null(file))
    paste(system2("jq", args = args, stdout = T, input = json), collapse = "")
  else
    paste(system2("jq", args = args, stdout = T, stdin = file), collapse = "")
}
