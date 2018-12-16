# extract_from_parse_tree ------------------------------------------------------

#' Extract Elements from Parse Tree of R Script
#' 
#' The idea of this function is to collect objects of interest from the parse 
#' tree, e.g. the names of functions that are called by a script. Therefore,
#' set the function \code{matches} so that it returns \code{TRUE} for the nodes
#' in the tree that are of interest.
#' 
#' @param x parse tree as returned by \code{parse} 
#' @param matches function that is called for each node of the tree. Give a 
#'   function here that returns \code{TRUE} if the object is to be selected and
#'   \code{FALSE} else. The value of \code{TRUE} must be given an attribute 
#'   \code{name} that is expected to be a character of length one.
#' @param dbg if \code{TRUE} each node in printed during the climing of the tree
#' @param path for internal use
#' @param parent for internal use
#' @param index for internal use
#' @return vector of character or \code{NULL}
extract_from_parse_tree <- function(
  x, matches = matches_function, dbg = FALSE, path = integer(), parent = NULL, 
  index = -1
) {

  if (is.null(matches) || ! is.function(matches)) {
    
    stop(call. = FALSE, "Please give a function in argument 'matches'")
  }
  
  if (is.list(x) && length(path) == 0) {
    
    return(lapply(
      x, extract_from_parse_tree, matches, dbg, path, parent, index
    ))
  }
    
  kwb.utils::catIf(dbg, sprintf(
    "[[%s]]: %s\n", 
    kwb.utils::commaCollapsed(path), 
    utils::capture.output(utils::str(x))
  ))

  # Is the current element wanted? If yes, store this element
  element <- if (wanted <- matches(x, parent, index)) {
    kwb.utils::getAttribute(wanted, "name")
  } else {
    NULL
  }

  # Do we have to climb further branches up?  
  if (is.list(x) || length(x) > 1) {
    
    c(element, unlist(lapply(seq_along(x), function(i) extract_from_parse_tree(
      x = x[[i]], matches = matches, dbg = dbg, path = c(path, i), 
      parent = x, index = i
    ))))
    
  } else {
    
    element
  }
}

# matches_function -------------------------------------------------------------
matches_function <- function(
  x, parent = NULL, index, exclude = base_functions()
) {
  
  if (! is.call(x)) {
    
    return(FALSE)
  }
  
  if (is.call(parent) && index == 1) {
    
    return(FALSE)  
  }
  
  name <- as.character(x[[1]])
  
  n <- length(name)
  
  if (! (n == 1 || n == 3)) {
   
    stop("Not expected: n =  ", n) 
  }
  
  if (n == 3) {
    
    name <- paste(name[c(2, 1, 3)], collapse = "")
  }

  if (name %in% exclude) {
    
    return(FALSE)
  } 
  
  structure(TRUE, name = name)
}

# default_excludes -------------------------------------------------------------
base_functions <- function() {
  c(
    "<-", "=", 
    "+", "-", "*", "/", 
    "<", ">", "<=", ">=", "==",
    "!", "|", "&", "||", "&&", 
    ":", "::", ":::", 
    "(", "{", "[", "$", "[[", 
    "c", "if", "%in%", "list"
  )
}
