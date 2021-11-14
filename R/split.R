# split_if ---------------------------------------------------------------------
split_if <- function(x)
{
  stopifnot(inherits(x, "if"))
  
  parts <- as.list(x)[-1]

  elements <- c("condition", "if.body", "else.body")[seq_along(parts)]
  
  structure(parts, names = elements)
}

# split_for --------------------------------------------------------------------
split_for <- function(x)
{
  stopifnot(inherits(x, "for"))
  
  list(
    variable = x[[2]],
    values = x[[3]],
    body = x[[4]]
  )
}

# split_call -------------------------------------------------------------------
split_call <- function(x)
{
  stopifnot(is.call(x))
  
  parts <- as.list(x)
  
  list(
    functionName = parts[[1]],
    args = parts[-1]
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
  body <- functionCall[[3]] # as.list(functionCall)[[3]]
  
  expressions <- as.list(body)
  
  bodyClass <- class(body)

  if (bodyClass == "{") {
    
    expressions <- expressions[-1]
  }
  
  list(
    args = functionCall[[2]],
    bodyClass = bodyClass,
    bodyExpressions = expressions
  )
}

# split_assignment -------------------------------------------------------------
split_assignment <- function(assignment)
{
  stopifnot(is_assignment(assignment))
  
  list(
    leftSide = assignment[[2]], 
    rightSide = assignment[[3]]
  )
}
