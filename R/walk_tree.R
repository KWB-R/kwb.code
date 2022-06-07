#' Walk Along a Parse Tree
#' 
#' @param x parse tree as returned by \code{\link{parse}} or a sub-tree of the
#'   parse tree
#' @param path for internal use only. Path to the element in the parse tree.
#' @param depth for internal use only. Recursion depth.
#' @param max_depth maximum recursion level. Default: 20L
#' @param dbg whether or not to show debug messages
#' @param config list defining modifications of nodes in the node tree. 
#' TODO: describe further
#' @export
#' @examples
#' walk_tree(parse(text = "x <- 1:n"))
walk_tree <- function(
  x, path = "", depth = 0L, max_depth = 20L, dbg = TRUE,
  config = list(), context = NULL
)
{
  stopOnMaxDepth(depth, max_depth)
  stopOnUnexpectedProps(props <- get_properties(x))
  
  kwb.utils::printIf(dbg, path)  
  kwb.utils::printIf(dbg, props)

  if (length(config)) {
    result <- evaluate_checks(x, config, path)
    if (kwb.utils::defaultIfNULL(result$matched, FALSE)) {
      cat("file:", context$file, "\n")
    }
    if (result$modified) {
      return(structure(result$new_node, modified = TRUE))
    }
  }
  
  if (! props$is_recursive) {
    kwb.utils::catIf(dbg, sprintf("Leaf reached: '%s'\n", props$text))
    return(x)
  }
  
  is_pairlist <- is.pairlist(x)
  
  for (i in seq_along(x)) {
    x[i] <- list(walk_tree(
      x[[i]],
      path = paste0(path, "/", i),
      depth = depth + 1L,
      max_depth = max_depth,
      dbg = dbg,
      config = config,
      context = context
    ))
  }
  
  if (is_pairlist) {
    x <- as.pairlist(x)
  }
  
  x
}

# stopOnMaxDepth ---------------------------------------------------------------
stopOnMaxDepth <- function(depth, max_depth)
{
  if (depth > max_depth) {
    stop(call. = FALSE, sprintf("depth > max_depth (%d) reached", max_depth))
  }
}

# stopOnUnexpectedProps --------------------------------------------------------
stopOnUnexpectedProps <- function(props) {
  stopifnot(length(props$class) == 1L)
  stopIfNonExpected("type", props$type, expected_types())
  stopIfNonExpected("class", props$class, expected_classes())
}

# stopIfNonExpected ------------------------------------------------------------
stopIfNonExpected <- function(what, name, expected) {
  if (!name %in% expected) {
    stop("Unexpected ", what, ": ", name, call. = FALSE)
  }
}

# expected_types ---------------------------------------------------------------
expected_types <- function()
{
  c(
    "character", 
    "double", 
    "expression",
    "integer",
    "language", 
    "list",
    "logical",
    "NULL",
    "pairlist", 
    "symbol"
  )
}

# expected_classes -------------------------------------------------------------
expected_classes <- function()
{
  c(
    "<-",
    "=",
    "{", 
    "(",
    "call", 
    "character",
    "expression",
    "for",
    "if",
    "integer",
    "list",
    "logical", 
    "name", 
    "numeric",
    "NULL",
    "pairlist", 
    "srcref",
    "while"
  )
}

# get_properties ---------------------------------------------------------------
get_properties <- function(x)
{
  kwb.utils::noFactorDataFrame(
    is_atomic = is.atomic(x),
    is_recursive = is.recursive(x),
    is_call = is.call(x),
    type = typeof(x),
    mode = mode(x),
    class = class(x),
    length = length(x),
    text = kwb.utils::left(kwb.utils::collapsed(deparse(x)), 20)
  )
}

# evaluate_checks --------------------------------------------------------------
evaluate_checks <- function(x, config, path)
{
  result <- list(
    old_node = x,
    new_node = x,
    matched = FALSE,
    modified = FALSE
  )
  
  checks <- kwb.utils::selectElements(config, "checks")
  
  if (!length(checks)) {
    return(result)
  }
  
  stopifnot(all(sapply(checks, is_check)))
  
  for (check in checks) {
    
    # Go to next check if this check is not passed
    if (!kwb.utils::selectElements(check, "check")(x)) {
      next
    }

    result$matched <- TRUE
    
    if (!is.null(check$report)) {
      check$report(x)
    }
    
    if (!is.null(check$modify)) {
      # Do not use catAndRun() here as it eval()uates!
      cat("Calling the modification function for ", path, "\n")
      result$new_node <- check$modify(x)
      result$modified <- TRUE
      break
    }
  }
  
  if (result$modified) {
    cat("Node was modified:\n")
    utils::str(result[c("old_node", "new_node")])
  }
  
  result
}

# is_check ---------------------------------------------------------------------
is_check <- function(check)
{
  is.function(kwb.utils::selectElements(check, "check")) && 
    (!is.null(check$modify) || !is.null(check$report))
}
