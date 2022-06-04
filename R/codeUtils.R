# filter_for -------------------------------------------------------------------
filter_for <- function(x, FUN.filter, ...)
{
  selected <- lapply(x, FUN.filter, ...)

  x[unlist(selected)]
}
