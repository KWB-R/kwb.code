# normaliseFunction ------------------------------------------------------------
#' @importFrom kwb.utils printIf removeAttributes
normaliseFunction <- function(x, dbg = FALSE)
{
  printIf(dbg, x, "Original")

  # Split the function assignment or raise an error
  parts <- split_function_assignment(x)

  newArglist <- getNewPairlist(x = parts$args)

  renames <- attr(newArglist, "renames")

  x[[3]][[2]] <- kwb.utils::removeAttributes(newArglist, "renames")

  # Rename the arguments in the body of the function
  x[[3]][[3]] <- replaceNames(x[[3]][[3]], renames)

  x
}

# getNewPairlist ---------------------------------------------------------------
getNewPairlist <- function(x, version = 2)
{
  stopifnot(is.pairlist(x))

  argnames <- names(x)

  # Rename all but "..."
  toRename <- (argnames != "...")

  # Create lookup vector for the renaming
  renames <- normalNames(sum(toRename), version)
  
  names(renames) <- argnames[toRename]

  # Modify the argument list of the function
  x <- replaceNames(expr = x, renames)

  argnames[toRename] <- unname(renames)

  structure(x, names = argnames, renames = renames)
}

# normalNames ------------------------------------------------------------------
normalNames <- function(n, version = 1)
{
  x <- seq_len(n)

  if (version == 1) {
    
    paste0("arg_", x)
    
  } else if (version == 2) {
    
    paste0(".", LETTERS[x], ".")
    
  } else {
    
    paste0(".", letters[x], ".")
  }
}

# replaceNames -----------------------------------------------------------------
#' @importFrom kwb.utils catIf
## http://stackoverflow.com/questions/33850219/change-argument-names-inside-a-function-r
## Function to replace variables in function body
## expr is `body(f)`, keyvals is a lookup table for replacements
replaceNames <- function(expr, keyvals = NULL, dbg = FALSE)
{
  catIf(dbg, "replaceNames(", deparse(expr)[1L], ")...\n")

  if (is_function_assignment(expr)) {
    
    return(normaliseFunction(x = expr))
  }

  for (i in seq_along(expr)) {

    if (is_function_assignment(expr[[i]])) {
      
      expr[[i]] <- normaliseFunction(x = expr[[i]])
      
    } else if (is.call(expr[[i]])) {
      
      expr[[i]][-1L] <- Recall(expr[[i]][-1L], keyvals)
      
    } else if (is.name(expr[[i]])) {

      varname <- deparse(expr[[i]])

      if (varname  %in% names(keyvals)) {

        expr[[i]] <- as.name(keyvals[[varname]])
      }
    }
  }

  expr
}
