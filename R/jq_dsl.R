## jq function filter factories
fun_json <- function(fun_name) function(json)
  as.jq(sprintf('%s(%s)', fun_name, expected_json(json)))

fun_string <- function(fun_name) function(text)
  as.jq(sprintf('%s(%s)', fun_name, expected_string(text)))

fun_regex <- function(fun_name) function(regex, modifiers = NULL) {
  regex <- expected_json(regex)
  if(missing(modifiers) || is.null(modifiers))
    as.jq(sprintf('%s(%s)', fun_name, regex))
  else
    as.jq(sprintf('%s(%s; %s)', fun_name, regex, expected_string(modifiers)))
}

fun_filter <- function(fun_name, allow_missing)
  function(filter)
    if(allow_missing && missing(filter)) {
      as.jq(fun_name)
    } else {
      as.jq(sprintf('%s(%s)', fun_name, filter))
    }

fun_path <- fun_filter # we want eventually to check the parameters,
                       # i.e. will be different from fun_filter

## create the jq filters

#' @import jsonlite
#' @export
`%|%` <- function(lhs, rhs) {
  as.jq(paste(lhs, rhs, sep = ' | '))
}

### json builders

#### json array
#' @export
jarray <- function(...) {
  elements <-
    lapply(list(...), function(el) {
      if (!inherits(el, 'jq') && !inherits(el, 'json'))
        el <- toJSON(el, auto_unbox = T)
      else
        el
    })

  as.jq(paste0('[',
         paste(elements, collapse = ', '),
         ']'))
}

#### json object
#' @export
#' @import lazyeval
jobject <- function(...) {
  jobject_(lazy_dots(...))
}

#' @export
#' @import lazyeval
jobject_ <- function(entries) {
  fields <- lazy_eval(entries, data = internal_members)
  field_names <- names(fields)
  if (is.null(field_names))
    field_names <- rep_len("", length(fields))

  entries <- mapply(function(field, value) {
    if (field == '') {
      if (inherits(value, 'character')) {
        internal_eval(value %:% j.[value])
      } else {
        stopifnot(inherits(value, 'jq_entry'))
        value
      }
    } else {
      internal_eval(field %:% value)
    }
  }, field_names, fields)

  as.jq(sprintf('{%s}',
             paste(entries,
                   collapse = ', ')))
}

#### colon operator to build json object entries
internal_members <- within(list(), {
  `%:%` <- function(field, value) {
    value <- expected_filter_or_json(value)

    if (inherits(field, 'jq')) {
      structure(sprintf('(%s): %s',
                        field,
                        value),
                class = 'jq_entry')
    } else {
      field_json <- toJSON(as.character(field), auto_unbox = T)
      structure(sprintf('%s: %s',
                        field_json,
                        value),
                class = 'jq_entry')

    }
  }
})

internal_eval <- function(expr) lazy_eval(lazy(expr), data = internal_members)

### simple jq filters without parameters

#' jq filter \code{.}
#' @export
j. <- as.jq('.')

#' jq filter \code{length}
#' @export
jlength <- as.jq('length')

#' jq filter \code{keys}
#' @export
jkeys <- as.jq('keys')

#' jq filter \code{to_entries}
#' @export
jto_entries <- as.jq('to_entries')

#' jq filter \code{from_entries}
#' @export
jfrom_entries <- as.jq('from_entries')

#' jq filter \code{numbers}
#' @export
jnumbers <- as.jq('numbers')

#' jq filter \code{empty}
#' @export
jempty <- as.jq('empty')

#' jq filter \code{leaf_paths}
#' @export
jleaf_paths <- as.jq('leaf_paths')

#' jq filter \code{arrays}
#' @export
jarrays <- as.jq('arrays')

#' jq filter \code{objects}
#' @export
jobjects <- as.jq('objects')

#' jq filter \code{iterables}
#' @export
jiterables <- as.jq('iterables')

#' jq filter \code{booleans}
#' @export
jbooleans <- as.jq('booleans')

#' jq filter \code{numbers}
#' @export
jnumbers <- as.jq('numbers')

#' jq filter \code{strings}
#' @export
jstrings <- as.jq('strings')

#' jq filter \code{nulls}
#' @export
jnulls <- as.jq('nulls')

#' jq filter \code{values}
#' @export
jvalues <- as.jq('values')

#' jq filter \code{scalars}
#' @export
jscalars <- as.jq('scalars')

#' jq filter \code{add}
#' @export
jadd <- as.jq('add')

#' jq filter \code{any}
#' @export
jany <- as.jq('any')

#' jq filter \code{all}
#' @export
jall <- as.jq('all')

#' jq filter \code{floor}
#' @export
jfloor <- as.jq('floor')

#' jq filter \code{sqrt}
#' @export
jsqrt <- as.jq('sqrt')

#' jq filter \code{tonumber}
#' @export
jtonumber <- as.jq('tonumber')

#' jq filter \code{tostring}
#' @export
jtostring <- as.jq('tostring')

#' jq filter \code{type}
#' @export
jtype <- as.jq('type')

#' jq filter \code{reverse}
#' @export
jreverse <- as.jq('reverse')

#' jq filter \code{explode}
#' @export
jexplode <- as.jq('explode')

#' jq filter \code{implode}
#' @export
jimplode <- as.jq('implode')

#' jq filter \code{tojson}
#' @export
jtojson <- as.jq('tojson')

#' jq filter \code{fromjson}
#' @export
jfromjson <- as.jq('fromjson')


### jq functions filters expecting a json parameter
#' @export
jhas <- fun_json('has')
#' @export
jcontains <- fun_json('contains')
#' @export
jindices <- fun_json('indices')
#' @export
jindex <- fun_json('index')
#' @export
jrindex <- fun_json('rindex')

### jq functions filters expecting a regex parameter
#' @export
jmatch <- fun_regex('match')
#' @export
jtest <- fun_regex('test')
#' @export
jsub <- fun_regex('sub')
#' @export
jgsub <- fun_regex('gsub')
#' @export
jscan <- fun_regex('scan')
#' @export
jsplit2 <- fun_regex('split2')

### jq functions filters expecting a string parameter
#' @export
jsplit <- fun_string('split')
#' @export
jjoin <- fun_string('join')
#' @export
jstartswith <- fun_string('startswith')
#' @export
jendswith <- fun_string('endswith')
#' @export
jltrimstr <- fun_string('ltrimstr')
#' @export
jrtrimstr <- fun_string('rtrimstr')

### jq function filters expecting any filter parameter
#' @export
jmap <- fun_filter('map', allow_missing = F)
#' @export
jdel <- fun_filter('del', allow_missing = F)
#' @export
jselect <- fun_filter('select', allow_missing = F)
#' @export
jrecurse <- fun_filter('recurse', allow_missing = T)
#' @export
jpaths <- fun_filter('paths', allow_missing = T)
#' @export
jsort <- fun_filter('sort', allow_missing = T)

### jq function filters expecting a path filter parameter
#' @export
jgroup <- fun_path('group', allow_missing = F)
#' @export
jmin <- fun_path('min', allow_missing = T)
#' @export
jmax <- fun_path('max', allow_missing = T)
#' @export
junique <- fun_path('unique', allow_missing = T)

### other jq filter

#' @export
jslice <- function(from, to) {
  if (missing(from) && missing(to)) sep <- ''
  else sep <- ':'
  if (missing(from)) from <- ''
  else if (from > 0 ) from <- from - 1
  if (missing(to)) to <- ''
  else if (to > 0) to <- to - 1
  as.jq(sprintf('.[%s%s%s]', from, sep, to))
}

#' @export
jmux <- function(...) {
  as.jq(paste(..., sep = ', '))
}

#' @export
jflatten <- function(depth) {
  if(missing(depth)) {
    as.jq('flatten')
  } else {
    as.jq(sprintf('flatten(%d)', depth))
  }
}

#' @export
jrange <- function(from = 0, to) {
  if(missing(to)) {
    to <- from
    from <- 0
  }
  as.jq(sprintf('range(%d; %d)', from, to))
}

### sequence operator

#' @export
jseq <- function(...)
  as.jq(paste(lapply(list(...), expected_filter_or_json),
           collapse = ', '))

## parameter transformations

expected_filter_or_json <- function(x) {
  if(!inherits(x, 'json') && !inherits(x, 'jq')) {
    x <- toJSON(x, auto_unbox = T)
  }
  x
}

expected_json <- function(json) {
  if(!inherits(json, 'json')) {
    if(inherits(json, 'jq')) {
      class(json) <- 'json'
      json
    } else {
      toJSON(json, auto_unbox = T)
    }
  }
}

expected_string <- function(s) {
  unlist(lapply(as.character(s), toJSON, auto_unbox = T))
}
