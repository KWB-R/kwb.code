# get_full_function_info -------------------------------------------------------

#' Get information on function definitions in parsed R scripts
#'
#' @param trees list of R script parse trees as provided by
#'   \code{\link{parse_scripts}}
#' @importFrom kwb.utils moveColumnsToFront rbindAll
#' @export
#' @seealso \code{\link{parse_scripts}}
get_full_function_info <- function(trees)
{
  function_info <- trees %>%
    lapply(function(tree) {
      tree %>%
        get_functions() %>%
        lapply(FUN = get_function_info) %>%
        kwb.utils::rbindAll()
    }) %>%
    kwb.utils::rbindAll(nameColumn = "script")
  
  merge(
    x = function_info, 
    y = multi_defined_functions(function_info), 
    by = "functionName"
  ) %>%
    kwb.utils::moveColumnsToFront(c("script", "functionName", "n.def"))
}

# multi_defined_functions ------------------------------------------------------
multi_defined_functions <- function(functionInfo)
{
  count <- stats::aggregate(
    n.def ~ functionName,
    cbind(n.def = seq_len(nrow(functionInfo)), functionInfo),
    length
  )
  
  count[order(count$n, decreasing = TRUE), ]
}
