# arg_names --------------------------------------------------------------------

#' Get Argument Names of a Function
#' 
#' @param x function name or function
#' @return vector of character
#' @export
#' @examples
#' arg_names("sum")
#' arg_names(mean)
arg_names <- function(x)
{
  args_list <- as.list(args(x))
  names(args_list[-length(args_list)])
}

# cat_formatted ----------------------------------------------------------------
cat_formatted <- function(fmt, ...)
{
  cat(sprintf(fmt, ...))
}

# filter_for -------------------------------------------------------------------
filter_for <- function(x, FUN.filter, ...)
{
  selected <- lapply(x, FUN.filter, ...)

  x[unlist(selected)]
}

# is_what ----------------------------------------------------------------------
is_what <- function(
  x, 
  exclude = c(
    # "is.na.numeric_version", 
    # "is.na.POSIXlt", 
    # "is.na.POSIXct",
    # "is.single",
    # # the following complain: 
    # #   nicht implementierte Standardmethode für Typ 'expression'
    # "is.finite", 
    # "is.infinite",
    # "is.nan",
    # # the following complain:
    # #   Argument zu 'which' ist nicht boolesch
    # "is.na",
    # "is.na.data.frame" # returns a matrix
  ),
  silent = FALSE
)
{
  #  stopifnot(length(x) == 1L)
  
  # Get names of functions within the base package
  base_functions <- ls(getNamespace("base"))
  
  # Find is.* functions
  is_functions <- base_functions[startsWith(base_functions, "is.")]

  # Which functions are not applicable, i.e. have not exactly one argument "x"
  is_applicable <- sapply(lapply(is_functions, arg_names), identical, "x")
  non_applicable <- is_functions[which(!is_applicable)]
  
  # Exclude non-applicable functions and further functions given in "exclude"
  is_functions <- setdiff(is_functions, c(non_applicable, exclude))
  
  # Call all remaining is.* functions to x
  is_results <- sapply(is_functions, function(f) {
    
    result <- try(do.call(f, list(x), quote = TRUE), silent = silent)

    cat_error <- function(what) {
      cat_formatted("%s(x) returned %s. Returning FALSE.\n", f, what)
    } 
    
    if (inherits(result, "try-error")) {
      cat_error("an error")
      return(FALSE)
    }
    
    if (!isTRUE(result) && !isFALSE(result)) {
      cat_error("neither TRUE nor FALSE")
      return(FALSE)
    }
    
    result
  })
  
  # Return the names (without "is.") of functions that returned TRUE  
  gsub("^is.", "", names(which(is_results)))
}

# remove_first_and_last_slash --------------------------------------------------
remove_first_and_last_slash <- function(x)
{
  gsub("^/+|/+$", "", x)
}

# vector_to_count_table --------------------------------------------------------
vector_to_count_table <- function(x)
{
  if (length(x) == 0L) {
    return(NULL)
  }
  
  frequency <- table(x)
  
  frequency_data <- kwb.utils::asNoFactorDataFrame(frequency)
  
  unexpected <- ncol(frequency_data) != 2
  
  kwb.utils::printIf(unexpected, x)
  kwb.utils::printIf(unexpected, frequency)
  kwb.utils::printIf(unexpected, frequency_data)
  
  stats::setNames(frequency_data, c("name", "count"))
}
