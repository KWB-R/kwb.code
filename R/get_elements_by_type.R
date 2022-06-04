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
    kwb.utils::catAndRun(dbg = dbg, "Analysing the parse tree", {
      result <- analyse(x)
    })
  }
  
  type_paths <- get_paths_to_types(result)
  
  code_parts <- lapply(type_paths, extract_by_path, x = x)
  
  stats::setNames(code_parts, names(type_paths))
}

# extract_by_path --------------------------------------------------------------
extract_by_path <- function(x, paths)
{
  # Remove leading slash from the type path
  clean_paths <- gsub("^/", "", paths)
  
  # Use the segments of the type path as (recursive) list indices
  lapply(strsplit(clean_paths, "/"), function(indices) {
    if (length(indices)) {
      x[[as.integer(indices)]]
    }
  })
}
