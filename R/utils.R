# filter_for -------------------------------------------------------------------
filter_for <- function(x, FUN.filter, ...)
{
  selected <- lapply(x, FUN.filter, ...)

  x[unlist(selected)]
}

# removeAttribute --------------------------------------------------------------
removeAttribute <- function(x, which)
{
  attr(x, which) <- NULL
  
  x
}

# vector_to_count_table --------------------------------------------------------
vector_to_count_table <- function(x)
{
  if (length(x) == 0) {
    
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
