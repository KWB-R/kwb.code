# split_if ---------------------------------------------------------------------
split_if <- function(x)
{
  stopifnot(inherits(x, "if"))
  
  parts <- as.list(x)[-1L]

  elements <- c("condition", "if.body", "else.body")[seq_along(parts)]
  
  stats::setNames(parts, elements)
}

# split_for --------------------------------------------------------------------
split_for <- function(x)
{
  stopifnot(inherits(x, "for"))
  
  name_parts(x, variable = 2L, values = 3L, body = 4L)
}

# split_call -------------------------------------------------------------------
split_call <- function(x)
{
  stopifnot(is.call(x))
  
  parts <- as.list(x)
  
  list(
    functionName = parts[[1L]],
    args = parts[-1L]
  )
}

# split_function_assignment ----------------------------------------------------
split_function_assignment <- function(f)
{
  stopifnot(is_function_assignment(assignment = f))
  
  parts <- split_assignment(f)
  
  callParts <- split_function_def_call(functionCall = parts$rightSide)
  
  c(list(functionName = as.character(parts$leftSide)), callParts)
}

# split_function_def_call ------------------------------------------------------
split_function_def_call <- function(functionCall)
{
  body <- functionCall[[3L]]

  expressions <- as.list(body)
  
  bodyClass <- class(body)

  if (bodyClass == "{") {
    expressions <- expressions[-1L]
  }
  
  list(
    args = functionCall[[2L]],
    bodyClass = bodyClass,
    bodyExpressions = expressions
  )
}

# split_assignment -------------------------------------------------------------
split_assignment <- function(assignment)
{
  stopifnot(is_assignment(assignment))
  
  name_parts(assignment, leftSide = 2L, rightSide = 3L)
}

# name_parts -------------------------------------------------------------------
name_parts <- function(x, ...)
{
  pairs <- list(...)
  
  stats::setNames(as.list(x)[as.integer(pairs)], names(pairs))
}
