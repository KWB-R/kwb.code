# get_depth_expression ---------------------------------------------------------
#' @importFrom kwb.utils hsQuoteChr
get_depth_expression <- function(e)
{
  noExpression <- ! is.expression(e)

  if (noExpression || length(e) == 0L) {

    msg <- if (noExpression) {
      paste("Skipping non-expression of class: ", hsQuoteChr(class(e)))
    } else {
      "Skipping expression of legth 0"
    }

    message(msg)

    return (0L)
  }

  lapply(e, get_depth_any)
}

# get_depth_any ----------------------------------------------------------------
#' @importFrom kwb.utils hsQuoteChr
get_depth_any <- function(x)
{
  if (is_assignment(x)) {
    return(get_depth_assignment(x))
  }
  
  if (is.call(x)) {
    return(get_depth_call(x))
  }
  
  if (is.name(x) || is.character(x) || is.numeric(x)) {
    return (0L)
  } 
  
  stop("need to treat object of class: ", kwb.utils::hsQuoteChr(class(x)))
}

# get_depth_assignment ---------------------------------------------------------
get_depth_assignment <- function(a)
{
  stopifnot(is_assignment(a))
  0L
}

# get_depth_call ---------------------------------------------------------------
get_depth_call <- function(x)
{
  stopifnot(is.call(x))

  # different types of call possible

  if (inherits(x, "if")) {
    return(get_depth_if(x))
  }
  
  if (inherits(x, "for")) {
    return(get_depth_for(x))
  }
  
  if (inherits(x, "{")) {
    return(get_depth_open_brace(x))
  }
  
  get_depth_call_usual(x)
}

# get_depth_if -----------------------------------------------------------------
get_depth_if <- function(x)
{
  stopifnot(inherits(x, "if"))

  parts <- split_if(x)

  list(
    class = "if",
    depth.cond = get_depth_any(x = parts$condition),
    depth.if = get_depth_any(parts$if.body),
    depth.else = get_depth_any(parts$else.body)
  )
}

# get_depth_for ----------------------------------------------------------------
get_depth_for <- function(x)
{
  stopifnot(inherits(x, "for"))

  parts <- split_for(x)

  list(
    depth.values = get_depth_any(x = parts$values),
    depth.body = get_depth_any(x = parts$body)
  )
}

# get_depth_open_brace ---------------------------------------------------------
get_depth_open_brace <- function(x)
{
  stopifnot(inherits(x, "{"))

  expressions <- as.list(x)[-1]

  lapply(expressions, get_depth_any)
}

# get_depth_call_usual ---------------------------------------------------------
get_depth_call_usual <- function(x)
{
  stopifnot(is.call(x))

  parts <- split_call(x)

  result <- lapply(parts$args, get_depth_any)

  stats::setNames(result, paste0(
    parts$functionName, "_arg_", seq_along(parts$args)
  ))
}

# get_depth --------------------------------------------------------------------
#' @importFrom kwb.utils hsQuoteChr
get_depth <- function(e, depth = 0)
{
  if (is_assignment(e)) {
    
    get_depth(e = split_assignment(e)$rightSide, depth + 1)
    
  } else if (is.call(e)) {

    parts <- if (is_function_def_call(e)) {
      split_function_def_call(e)
    } else {
      list(args = as.list(e)[-1], bodyExpressions = NULL)
    }

    depths <- numeric()

    if (! is.null(parts$args)) {
      depths <- c(depths, sapply(parts$args, get_depth, depth + 1))
    }
    
    if (! is.null(parts$bodyExpressions)) {
      depths <- c(depths, sapply(parts$bodyExpressions, get_depth, depth + 1))
    }

    max(depths)
    
  } else  {
    
    message("This must be a class of depth 0: ", hsQuoteChr(class(e)))
  }
}
