# find_weaknesses_in_scripts ---------------------------------------------------

#' Find weaknesses in R scripts
#' 
#' @param x list of named parse trees as returned by
#'   \code{\link{parse_scripts}}. Not required if \code{root} is given.
#' @param root path to folder containing R scripts
#' @return data frame with columns \code{file}, \code{expression},
#'   \code{frequency}, \code{recommendation}
#' @export
#' 
find_weaknesses_in_scripts <- function(x = parse_scripts(root), root = NULL)
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
      check_function = is_true_or_false_constant,
      recommendation = "use TRUE/FALSE instead of T/F",
      type = "parent"
    )
  )
  
  strings <- find_code_snippets(x, is.character, "check for duplicated strings")
  
  is_relevant <- nchar(as.character(strings$expression)) > 5L & 
    strings$frequency > 1

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
to_matches_function <- function(check_function, type = "self")
{
  function(x, parent, index) {
    
    if (!check_function(x)) {
      return(FALSE)
    }
    
    structure(TRUE, name = kwb.utils::collapsed(
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
  }
}

# is_true_or_false_constant ----------------------------------------------------
is_true_or_false_constant <- function(x)
{
  if (!is.symbol(x)) {
    return(FALSE)
  }
  
  deparse(x) %in% c("T", "F")
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
