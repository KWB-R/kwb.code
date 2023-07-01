# find_weaknesses_in_scripts ---------------------------------------------------

#' Find weaknesses in R scripts
#' 
#' @param x list of named parse trees as returned by
#'   \code{\link{parse_scripts}}. Not required if \code{root} is given.
#' @param root path to folder containing R scripts
#' @param min_duplicate_string_length minimum number of characters that a 
#'   string constant must have to be considered as a duplicate
#' @param min_duplicate_frequency minimum frequency of a string constant to
#'   be considered as a duplicate
#' @return data frame with columns \code{file}, \code{expression},
#'   \code{frequency}, \code{recommendation}
#' @export
#' 
find_weaknesses_in_scripts <- function(
    x = parse_scripts(root), 
    root = NULL,
    min_duplicate_string_length = 6L,
    min_duplicate_frequency = 3L
)
{
  is_expression <- sapply(x, is.expression)
  is_error <- sapply(x, kwb.utils::isTryError)
  
  stopifnot(all(is_expression | is_error))
  
  # Remove scripts that could not be parsed by setting elements to NULL first
  x[is_error] <- lapply(which(is_error), function(i) NULL)
  x <- kwb.utils::excludeNULL(x)
  
  results <- list(
    find_code_snippets(
      x, 
      check_function = is_colon_seq_1_to_variable, 
      recommendation = "use seq_len()"
    ),
    find_code_snippets(
      x, 
      check_function = is_colon_seq_1_to_length, 
      recommendation = "use seq_along()"
    ),
    find_code_snippets(
      x, 
      check_function = is_bad_function_name, 
      recommendation = "avoid dot in function name", 
      type = "element_2"
    ),
    find_code_snippets(
      x, 
      check_function = is_logical_constant,
      recommendation = "use TRUE/FALSE instead of T/F",
      type = "parent"
    ),
    find_code_snippets(
      x, 
      check_function = is_comparison_with_true,
      recommendation = "use 'x' instead of 'x == TRUE/T'"
    ),
    find_code_snippets(
      x, 
      check_function = is_comparison_with_false,
      recommendation = "use '!x' instead of 'x == FALSE/F'"
    )
  )
  
  strings <- find_code_snippets(x, is.character, "check for duplicated strings")
  
  is_relevant <- 
    nchar(as.character(strings$expression)) >= min_duplicate_string_length & 
    strings$frequency >= min_duplicate_frequency

  if (any(is_relevant)) {
    results <- c(results, list(strings[is_relevant, ]))
  }
  
  do.call(rbind, results)
}

# find_code_snippets -----------------------------------------------------------
find_code_snippets <- function(
    x, check_function, recommendation = "", type = "self"
)
{
  matches <- to_matches_function(check_function, type = type)
  
  result <- summarise_extracted_matches(
    extract_from_parse_tree(x, matches = matches)
  )
  
  if (nrow(result) == 0L) {
    return(NULL)
  }
  
  cbind(result, recommendation = recommendation)
}

# to_matches_function ----------------------------------------------------------
to_matches_function <- function(check_function, type = "self", max_chars = 50L)
{
  function(x, parent, index) {
    
    if (!check_function(x)) {
      return(FALSE)
    }
    
    structure(
      TRUE, 
      name = kwb.utils::shorten(max_chars = max_chars, kwb.utils::collapsed(
        if (identical(type, "self")) {
          deparse(x)
        } else if (identical(type, "element_2")) {
          deparse(x[[2L]])
        } else if (identical(type, "parent")) {
          deparse(parent)
        } else {
          stop("unknown type: ", type)
        }
      ))
    )
  }
}

# is_logical_constant_false ----------------------------------------------------
is_logical_constant_false <- function(x, type = "short")
{
  is_logical_constant(x, type, use_true = FALSE)
}

# is_logical_constant_true -----------------------------------------------------
is_logical_constant_true <- function(x, type = "short")
{
  is_logical_constant(x, type, use_false = FALSE)
}

# is_logical_constant ----------------------------------------------------------
is_logical_constant <- function(
    x, 
    type = "short", 
    use_false = TRUE, 
    use_true = TRUE
)
{
  if (!is.symbol(x)) {
    return(FALSE)
  }

  deparse(x) %in% deparsed_logical_values(type, use_false, use_true)
}

# deparsed_logical_values ------------------------------------------------------
deparsed_logical_values <- function(
    type = c("short", "long", "either")[3L],
    use_false = TRUE,
    use_true = TRUE
)
{
  values <- c("F", "T", "FALSE", "TRUE")
  use_false_true <- c(use_false, use_true)
  
  if (type == "short") {
    values[1:2][use_false_true]
  } else if (type == "long") {
    values[3:4][use_false_true]
  } else if (type == "either") {
    values[rep(use_false_true, 2L)]
  } else {
    stop("Unknown type: ", type)
  }
}

# is_colon_seq_1_to_length -----------------------------------------------------
is_colon_seq_1_to_length <- function(x)
{
  is_colon_seq_1_to_any(x) &&
    mode(x[[3]]) == "call" &&
    identical(deparse(x[[3]][[1]]), "length")
}

# is_colon_seq_1_to_variable ---------------------------------------------------
is_colon_seq_1_to_variable <- function(x)
{
  is_colon_seq_1_to_any(x) &&
    !is.numeric(x[[3]]) &&
    mode(x[[3]]) != "call"
}

# is_colon_seq_1_to_any --------------------------------------------------------
is_colon_seq_1_to_any <- function(x)
{
  is_colon_seq(x) && identical(x[[2]], 1)
}

# is_colon_seq -----------------------------------------------------------------
is_colon_seq <- function(x)
{
  is.language(x) && 
    length(x) == 3L && 
    is.symbol(x[[1L]]) &&
    identical(as.character(x[[1]]), ":")
}

# is_bad_function_name ---------------------------------------------------------
is_bad_function_name <- function(x)
{
  if (!is_function_assignment(x)) {
    return(FALSE)
  }
  
  function_name <- split_assignment(x)$leftSide
  
  is.name(function_name) && 
    grepl("\\.", deparse(function_name))
}


# is_comparison_with_false -----------------------------------------------------
is_comparison_with_false <- function(x)
{
  is_comparison_with_logical(x, use_true = FALSE)
}

# is_comparison_with_true ------------------------------------------------------
is_comparison_with_true <- function(x)
{
  is_comparison_with_logical(x, use_false = FALSE)
}

# is_comparison_with_logical ---------------------------------------------------
is_comparison_with_logical <- function(x, use_false = TRUE, use_true = TRUE)
{
  if (!is.call(x)) {
    return(FALSE)
  }

  operator <- deparse(x[[1]])
  
  operator %in% c("==", "!=") && (
    is_logical_constant(x[[2]], type = "either", use_false, use_true) ||
      is_logical_constant(x[[3]], type = "either", use_false, use_true)
  )
}

# summarise_extracted_matches --------------------------------------------------
summarise_extracted_matches <- function(x)
{
  result <- kwb.utils::excludeNULL(x, dbg = FALSE)
  
  result <- lapply(result, function(xx) {
    stats::setNames(
      as.data.frame(table(xx)),
      c("expression", "frequency")
    )
  })
  
  dplyr::bind_rows(result, .id = "file")
}
