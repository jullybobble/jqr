#' @export
as.jq <- function(code, ...) structure(code, class = c('jq', ...))

#' @export
jq <- function(code, ...) as.jq(code, ...)

#' @export
print.jq <- function(x) {
  out <- paste("jq", shQuote(x))
  cat(paste(strwrap(out), collapse = "\n"), "\n", sep = "")
}

#' @export
`[.jq` <- function(filter, ..., from = NULL, to = NULL, opt = F) {
  if (!is.null(from) || !is.null(to)) {
    if (length(list(...)) != 0)
      warning(sprintf('arguments ignored since "from" or "to" are specified: ', paste(c(...), collapse = ', ')))

    jslice(from, to)

  } else if (deparse(substitute(...)) == '') {
    as.jq(paste0(filter, '[]'))

  } else {
    index <- c(...)
    if (is.numeric(index))
      index <- index - 1
    else
      index <- vapply(as.character(index), toJSON, auto_unbox = T, FUN.VALUE = character(1), USE.NAMES = F)

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
  if (allow_unary && missing(rhs)) {
    as.jq(paste0(operator, filter))
  } else {
    if (!inherits(rhs, 'jq') && !inherits(rhs, 'json'))
      rhs <- toJSON(rhs, auto_unbox = T)
    as.jq(paste(filter, operator, rhs))
  }
}

`%=%` <- function(lhs, rhs) UseMethod('%=%')

`%=%.jq` <- function(filter, rhs)
  as.jq(paste(filter, '=', expected_filter_or_json(rhs)))

`%|=%` <- function(lhs, rhs) UseMethod('%|=%')

`%|=%.jq` <- function(filter, rhs)
  as.jq(paste(filter, '|=', expected_filter_or_json(rhs)))


