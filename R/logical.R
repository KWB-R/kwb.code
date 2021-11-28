# is_assignment ----------------------------------------------------------------
is_assignment <- function(x)
{
  inherits(x, "<-") || inherits(x, "=")
}

# is_function_assignment -------------------------------------------------------
is_function_assignment <- function(assignment)
{
  if (! is_assignment(assignment)) {
    return(FALSE)
  }

  is_function_def_call(split_assignment(assignment)$rightSide)
}

# is_function_def_call ---------------------------------------------------------
is_function_def_call <- function(x)
{
  if (! is.call(x)) {
    return(FALSE)
  }

  as.character(as.list(x)[[1L]])[1L] == "function"
}
