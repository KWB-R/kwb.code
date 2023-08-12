# get_elements_by_type ---------------------------------------------------------

#' Extract Sections of Same "Type" from Parse Tree
#' 
#' @param x parse tree as returned by \code{\link{parse}}
#' @param result optional. Result as returned by \code{\link{analyse}}
#' @param dbg if \code{TRUE}, debug messages are shown
#' @export
#' @examples 
#' # Parse an R script file (here, a file from kwb.utils)
#' x <- parse("https://raw.githubusercontent.com/KWB-R/kwb.utils/master/R/log.R")
#' 
#' # For each "type" of code segment, extract all occurrences
#' elements <- get_elements_by_type(x)
#' 
#' # Show all for-loops
#' elements$`language|call|for|4|`
#' 
#' # Show all if-statements
#' elements$`language|call|if|3|`
#' 
#' # Show all if-else-statements
#' elements$`language|call|if|4|`
#' 
get_elements_by_type <- function(x, result = NULL, dbg = TRUE)
{
  if (is.null(result)) {
    kwb.utils::catAndRun(
      "Analysing the parse tree", 
      dbg = dbg, 
      expr = {
        result <- analyse(x)
      }
    )
  }
  
  type_paths <- get_paths_to_types(result)
  
  type_paths %>%
    lapply(extract_by_path, x = x) %>%
    stats::setNames(names(type_paths))
}

# extract_by_path --------------------------------------------------------------
extract_by_path <- function(x, paths)
{
  stopifnot(is.recursive(x))
  
  paths %>%
    
    # Split the path strings into vectors of integer
    split_index_path() %>%
    
    # Use the segments of the type path as (recursive) list indices
    lapply(function(indices) {
      if (length(indices)) {
        x[[indices]]
      }
    })
}

# split_index_path -------------------------------------------------------------
split_index_path <- function(x)
{
  stopifnot(is.character(x))
  stopifnot(all(is_index_path(x)))
  
  x %>%
    remove_first_and_last_slash() %>%
    strsplit("/") %>%
    lapply(as.integer)
}

# is_index_path ----------------------------------------------------------------
# @examples is_index_path(c("1", "/1", "/11", "1/2/3"))
is_index_path <- function(x)
{
  grepl("^/?([0-9]+/?)*$", x)
}
