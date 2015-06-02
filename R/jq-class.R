#' @export
as.jq <- function(code, ...) structure(code, class = c('jq', ...))

#' @export
print.jq <- function(x) {
  out <- paste("jq", shQuote(x))
  cat(paste(strwrap(out), collapse = "\n"), "\n", sep = "")
}

#' @export
`[.jq` <- function(filter, ..., from = NULL, to = NULL, opt = F) {
  args <- deparse(substitute(...))
  if (!is.null(from) || !is.null(to)) {
    if (length(list(...)) != 0)
      warning(sprintf('arguments ignored since "from" or "to" are specified: ', paste(c(...), collapse = ', ')))

    if (is.null(from))
        from <- ''
    else if (from > 0)
      from <- from - 1

    if (is.null(to))
      to <- ''
    else if (to > 0)
      to <- to - 1

    as.jq(paste0(filter, '[', from, ':', to, ']'))

  } else if (args == "") {
    as.jq(paste0(filter, '[]'))

  } else {
    index <- c(...)
    if (is.numeric(index))
      index <- index - 1
    else
      index <- sapply(as.character(index), deparse, USE.NAMES = F)

    as.jq(sprintf('%s[%s]%s', filter, paste(index, collapse = ","), if (opt) '?' else ''))
  }
}

#' @export
`+.jq` <- function(filter, rhs) override_operator('+', filter, rhs)
#' @export
`-.jq` <- function(filter, rhs) override_operator('-', filter, rhs)
#' @export
`*.jq` <- function(filter, rhs) override_operator('*', filter, rhs, allow_unary = F)
#' @export
`/.jq` <- function(filter, rhs) override_operator('/', filter, rhs, allow_unary = F)
#' @export
`>.jq` <- function(filter, rhs) override_operator('>', filter, rhs, allow_unary = F)
#' @export
`>=.jq` <- function(filter, rhs) override_operator('>=', filter, rhs, allow_unary = F)
#' @export
`<.jq` <- function(filter, rhs) override_operator('<', filter, rhs, allow_unary = F)
#' @export
`<=.jq` <- function(filter, rhs) override_operator('<=', filter, rhs, allow_unary = F)

override_operator <- function(operator, filter, rhs, allow_unary = T) {
  if (missing(rhs)) {
    as.jq(paste0(operator, filter))
  } else {
    if (!inherits(rhs, 'jq') && !inherits(rhs, 'json'))
      rhs <- toJSON(rhs, auto_unbox = T)
    as.jq(paste(filter, operator, rhs))
  }
}

