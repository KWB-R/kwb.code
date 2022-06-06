# Configure modifications ------------------------------------------------------
config <- list(checks = list(
  list(
    check = function(x) {
      if (!is.call(x)) return(FALSE)
      #str(as.list(x))
      #substr(deparse(x)[1L], 1L, 2L) == "1:"
      identical(x[[1]], as.name(":")) && 
        identical(x[[2]], 1) &&
        is.name(x[[3]])
    },
    # report = function(x) {
    #   cat("Expression of type '1:n' found:\n")
    #   print(x)
    # },
    modify = function(x) {
      testthat::expect_is(x, "call")
      testthat::expect_identical(x[[1]], as.name(":"))
      testthat::expect_identical(x[[2]], 1)
      x[[1L]] <- as.name("seq_len")
      x[[2L]] <- x[[3L]]
      result <- x[-3L]
      #str(result)
      result
    }
  )
))

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  #files <- dir_r_files("R")
  files <- dir_r_files("~/github-repos/K/kwb.utils/R")
  files <- dir_r_files("~/R-Development/RScripts")
  
  i <- 1L
  (i <- i + 1L)
  
  i <- 4L
  
  new_tree <- walk_tree(tree, config = config, dbg = FALSE)
  
  config$checks[[1]]$modify(tree[[c(1,3,3,2,2,3)]])
  
  new_tree <- walk_tree(tree[[c(1,3,3,2,2)]], config = config)
  
  i <- 3L
  
  for (i in seq_along(files)) {
    file <- files[i]
    cat(sprintf("%3d: %s\n", i, file))
    tree <- parse(file, keep.source = TRUE)
    new_tree <- try(walk_tree(tree, dbg = FALSE, config = config))
    if (!kwb.utils::isTryError(new_tree)) {
      try(stopifnot(identical(tree, new_tree)))
    }
  }
  
  x <- tree[[c(4,3,2)]]
  x2 <- walk_tree(x)
  identical(x, x2)
  str(x)
  str(x2)
  
  str(tree)
  str(new_tree)
  writeLines(deparse(tree), "parse-tree_old.txt")
  writeLines(deparse(new_tree), "parse-tree_new.txt")
  kwb.utils::hsOpenWindowsExplorer(getwd())
  
}

walk_tree <- function(
  x, path = "", depth = 0L, max_depth = 20L, dbg = TRUE,
  config = list()
)
{
  stopOnMaxDepth(depth, max_depth)
  
  props <- get_properties(x)
  
  stopOnUnexpectedProps(props)
  
  kwb.utils::catIf(dbg, "path:", path, "\n")  
  kwb.utils::printIf(dbg, props)
  
  if (length(config)) {
    result <- evaluate_checks(x, config, path)
    if (result$modified) {
      return(result$new_node)
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
      config = config
    ))
  }
  
  if (is_pairlist) {
    x <- as.pairlist(x)
  }
  
  x
}

dir_r_files <- function(path, recursive = TRUE)
{
  dir(
    path, "\\.R$", ignore.case = TRUE, full.names = TRUE, recursive = recursive
  )
}

stopOnMaxDepth <- function(depth, max_depth)
{
  if (depth > max_depth) {
    stop(call. = FALSE, sprintf("depth > max_depth (%d) reached", max_depth))
  }
}

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

stopOnUnexpectedProps <- function(props) {
  stopifnot(length(props$class) == 1L)
  stopIfNonExpected("type", props$type, expected_types())
  stopIfNonExpected("class", props$class, expected_classes())
}

stopIfNonExpected <- function(what, name, expected) {
  if (!name %in% expected) {
    stop("Unexpected ", what, ": ", name, call. = FALSE)
  }
}

expected_classes <- function() c(
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

expected_types <- function() c(
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

evaluate_checks <- function(x, config, path)
{
  result <- list(
    old_node = x,
    new_node = x,
    modified = FALSE
  )
  
  checks <- config$checks
  
  if (!length(checks)) {
    return(result)
  }
  
  stopifnot(all(sapply(checks, is_check)))
  
  for (check in checks) {
    if (check$check(x)) {
      if (!is.null(check$report)) {
        check$report(x)
      } else if (!is.null(check$modify)) {
        result$modified <- TRUE
        # Do not use catAndRun() here as it eval()uates!
        cat("Calling the modification function for ", path, "\n")
        result$new_node <- check$modify(x)
        break
      }
    }
  }
  
  if (result$modified) {
    cat("Node was modified:\n")
    str(result[c("old_node", "new_node")])
  }
  
  result
}

is_check <- function(check)
{
  is.function(check$check) && 
    (!is.null(check$modify) || !is.null(check$report))
}
