# analyse ----------------------------------------------------------------------

#' Analyse the Parse Tree of an R Script
#'
#' @param x parse tree as returned by \code{\link{parse}}
#' @param path for internal use only (when this function is called recursively)
#' @return list representing type information on the nodes in the parse tree
#' @export
#' @examples
#' # Parse an R script file (here, a file from kwb.utils)
#' x <- parse("https://raw.githubusercontent.com/KWB-R/kwb.utils/master/R/column.R")
#' 
#' # Analyse the parse tree (This may take some time!)
#' result <- kwb.code::analyse(x)
#' 
#' # Show the structure of the result list (only 3 levels!)
#' str(result, 3)
analyse <- function(x, path = "")
{
  info <- type_info(x)
  
  # result <- list(
  #   self = info_to_text(info)
  # )
  
  result <- info
  result[["path"]] <- path
  result[["fulltype"]] <- info_to_text(info)
  
  if (is.recursive(x)) {
    
    result[["children"]] <- lapply(seq_along(x), function(i) {
      analyse(x[[i]], path = paste(path, i, sep = "/"))
    })
  }
  
  result  
}
