# get_elements_by_type ---------------------------------------------------------

#' Extract Sections of Same "Type" from Parse Tree
#' 
#' @param x parse tree as returned by \code{\link{parse}}
#' @param result optional. Result as returned by \code{\link{analyse}}
#' @export
#' @examples 
#' # Parse an R script file (here, a file from kwb.utils)
#' x <- parse("https://raw.githubusercontent.com/KWB-R/kwb.utils/master/R/column.R")
#' 
#' # For each "type" of code segment, extract all occurrences
#' elements <- get_elements_by_type(x)
#' # Show all for-loops
#' elements$`language|call|for|4|`
#' 
#' # Show all if-statements
#' elements$`language|call|if|3|`
#' 
#' # Show all if-else-statements
#' elements$`language|call|if|4|`
#' 
get_elements_by_type <- function(x, result = NULL)
{
  if (is.null(result)) {
    kwb.utils::catAndRun("Analysing the parse tree", {
      result <- analyse(x)
    })
  }
  
  type_paths <- get_paths_to_types(result)
  
  lapply(
    stats::setNames(seq_along(type_paths), names(type_paths)),
    FUN = function(i) {
      
      # Remove leading slash from the type path
      type_path <- gsub("^/", "", type_paths[[i]])
      
      # Use the segments of the type path as (recursive) list indices
      lapply(strsplit(type_path, "/"), function(indices) {
        if (length(indices)) {
          x[[as.integer(indices)]]
        }
      })
    }
  )
}
