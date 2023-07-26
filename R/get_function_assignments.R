# get_function_assignments -----------------------------------------------------

#' Extract the function assignments from an R script
#' 
#' @param file path to R script from which function definitions are to be 
#'   extracted
#' @return named list of expressions. The names of the list elements represent 
#'   the names of the functions that are defined by the expressions in the list.
#' @return
#' @export
get_function_assignments <- function(file)
{
  # code <- as.list(parse(file))
  #
  # #expr <- code[[2]]
  #
  # is_function_assignment <- sapply(code, function(expr) {
  #
  #   ok <- as.character(expr[[1]]) == "<-"
  #
  #   ok && length(expr) >= 3 && as.character(expr[[3]][[1]]) == "function"
  # })
  #
  # assignments <- code[is_function_assignment]

  assignments <- parse(file) %>%
    get_functions() %>%
    as.list()
  
  # Get the function names from the assignments
  # function_names <- sapply(lapply(assignments, "[[", 2L), deparse)
  # sapply(assignments, function(x) as.character(x[[2]]))
  function_names <- assignments %>%
    lapply(split_assignment) %>%
    lapply(kwb.utils::selectElements, "leftSide")
  
  # Name the assignments according to the function names
  stats::setNames(assignments, function_names)
}
