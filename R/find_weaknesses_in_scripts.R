# find_weaknesses_in_scripts ---------------------------------------------------

#' Find weaknesses in R scripts
#' 
#' @param root path to folder containing R scripts
#' @return data frame with columns \code{file}, \code{expression},
#'   \code{frequency}, \code{recommendation}
#' @export
#' 
find_weaknesses_in_scripts <- function(root)
{
  x <- parse_scripts(root)
  
  rbind(
    find_code_snippets(x, is_colon_seq_1_to_variable, "use seq_len()"),
    find_code_snippets(x, is_colon_seq_1_to_length, "use seq_along()"),
    find_code_snippets(x, is_bad_function_name, "avoid dot in function name", 
                       type = 2L)
  )
}

# find_code_snippets -----------------------------------------------------------
find_code_snippets <- function(x, check_function, recommendation = "", ...)
{
  cbind(
    summarise_extracted_matches(
      extract_from_parse_tree(x, matches = to_matches_function(
        check_function, ...
      ))
    ), 
    recommendation = recommendation
  )
}

# to_matches_function ----------------------------------------------------------
to_matches_function <- function(check_function, type = 1L)
{
  function(x, parent, index) {
    
    if (!check_function(x)) {
      return(FALSE)
    }
    
    structure(TRUE, name = kwb.utils::collapsed(
      if (type == 1L) {
        deparse(x)
      } else if (type == 2L) {
        deparse(x[[2L]])
      }
    ))
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
