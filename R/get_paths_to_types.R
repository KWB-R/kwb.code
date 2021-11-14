# get_paths_to_types -----------------------------------------------------------
get_paths_to_types <- function(result)
{
  # Which different full types do appear?
  extract <- function(x) {
    
    type <- paste0(x$path, ":", x$fulltype)
    
    children <- x$children
    
    if (is.null(children)) {
      return(type)
    }
    
    c(type, unlist(lapply(children, extract)))
  }
  
  types <- extract(result)
  
  parts <- strsplit(types, ":")
  
  split(sapply(parts, "[[", 1L), sapply(parts, "[[", 2L))
}
