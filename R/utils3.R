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
