
jq <- function(expr)
  eval(substitute(expr), envir = jq_filters)

jq_ <- function(expr)
  eval(expr, envir = jq_filters)

jq_filters <- new.env(parent = environment())

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
evalq(envir = jq_filters, {

  ### json builders

  #### json array
  arr <- function(...) {
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
  obj <- function(...) {
    fields <- list(...)
    field_names <- names(fields)
    if (is.null(field_names))
      field_names <- rep_len("", length(fields))

    entries <- mapply(function(field, value) {
      if (field == '') {
        if (inherits(value, 'character')) {
          value %:% .[value]
        } else {
          stopifnot(inherits(value, 'jq_entry'))
          value
        }
      } else {
        field %:% value
      }
    }, field_names, fields)

    as.jq(sprintf('{%s}',
               paste(entries,
                     collapse = ', ')))
  }

  #### colon operator to build json object entries
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

  ### simple jq filters without parameters
  keywords <- c('.', 'length', 'keys', 'to_entries', 'from_entries', 'numbers',
                'empty', 'leaf_paths', 'arrays', 'objects', 'iterables',
                'booleans', 'numbers', 'strings', 'nulls', 'values', 'scalars',
                'add', 'any', 'all', 'floor', 'sqrt', 'tonumber', 'tostring',
                'type', 'reverse', 'explode', 'implode', 'tojson', 'fromjson')

  for(keyword in keywords) {
    assign(keyword,
           as.jq(keyword))
  }

  ### jq functions filters expecting a json parameter
  has <- fun_json('has')
  contains <- fun_json('contains')
  indices <- fun_json('indices')
  index <- fun_json('index')
  rindex <- fun_json('rindex')

  ### jq functions filters expecting a regex parameter
  match <- fun_regex('match')
  test <- fun_regex('test')
  sub <- fun_regex('sub')
  gsub <- fun_regex('gsub')
  scan <- fun_regex('scan')
  split2 <- fun_regex('split2')

  ### jq functions filters expecting a string parameter
  split <- fun_string('split')
  join <- fun_string('join')
  startswith <- fun_string('startswith')
  endswith <- fun_string('endswith')
  ltrimstr <- fun_string('ltrimstr')
  rtrimstr <- fun_string('rtrimstr')

  ### jq function filters expecting any filter parameter
  map <- fun_filter('map', allow_missing = F)
  del <- fun_filter('del', allow_missing = F)
  select <- fun_filter('select', allow_missing = F)
  recurse <- fun_filter('recurse', allow_missing = T)
  paths <- fun_filter('paths', allow_missing = T)
  sort <- fun_filter('sort', allow_missing = T)

  ### jq function filters expecting a path filter parameter
  group <- fun_path('group', allow_missing = F)
  min <- fun_path('min', allow_missing = T)
  max <- fun_path('max', allow_missing = T)
  unique <- fun_path('unique', allow_missing = T)

  ### other jq filter
  slice <- function(from, to) {
    if (missing(from) && missing(to)) sep <- ''
    else sep <- ':'
    if (missing(from)) from <- ''
    else if (from > 0 ) from <- from - 1
    if (missing(to)) to <- ''
    else if (to > 0) to <- to - 1
    as.jq(sprintf('.[%s%s%s]', from, sep, to))
  }

  mux <- function(...) {
    as.jq(paste(..., sep = ', '))
  }

  flatten <- function(depth) {
    if(missing(depth)) {
      as.jq('flatten')
    } else {
      as.jq(sprintf('flatten(%d)', depth))
    }
  }

  range <- function(from = 0, to) {
    if(missing(to)) {
      to <- from
      from <- 0
    }
    as.jq(sprintf('range(%d; %d)', from, to))
  }

  ### sequence operator

  seq <- function(...)
    as.jq(paste(lapply(list(...), expected_filter_or_json),
             collapse = ', '))


})

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

`%|%` <- function(lhs, rhs) {
  as.jq(paste(jq_(substitute(lhs)), jq_(substitute(rhs)), sep = ' | '))
}
