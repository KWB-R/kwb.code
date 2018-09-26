# expressions_stat -------------------------------------------------------------
#' @importFrom kwb.utils rbindAll
#' @importFrom kwb.utils noFactorDataFrame
expressions_stat <- function(x)
{
  rbindAll(lapply(x, function(xx) noFactorDataFrame(
    mode = mode(xx),
    class = class(x),
    x2 = as.character(xx[[2]]),
    n.names = length(all.vars(xx))
  )))
}

# get_function_info ------------------------------------------------------------
#' @importFrom kwb.utils noFactorDataFrame
get_function_info <- function(f)
{
  functionParts <- split_function_assignment(f)

  args <- functionParts$args

  noFactorDataFrame(
    functionName = functionParts$functionName,
    bodyClass = functionParts$bodyClass,
    n.args = length(args),
    n.defaults = sum(sapply(args, class) != "name"),
    n.expr = length(functionParts$bodyExpressions)
  )
}

# to_full_script_info ----------------------------------------------------------
#' Get script statistics from a list of R script trees
#'
#' @param trees list of R script parse trees as provided by
#'   \code{\link{parse_scripts}}
#'
#' @importFrom kwb.utils renameColumns
#' @importFrom kwb.utils moveColumnsToFront
#'
#' @export
#'
#' @seealso \code{\link{parse_scripts}}
to_full_script_info <- function(trees)
{
  info <- merge(
    trees_to_script_info(trees),
    trees_to_type_stat(trees),
    by = "script"
  )

  info$fun <- count_functions(trees)

  columns <- c(
    "script", "errors", "rows", "expr", "rpe", "<-", "=", "fun", "call", "if", 
    "for", "{", "("
  )

  columns <- intersect(columns, names(info))

  renames <- list(character = "chr", logical = "logi", numeric = "num")

  renameColumns(moveColumnsToFront(info, columns), renames)
}

# trees_to_script_info ---------------------------------------------------------
#' @importFrom kwb.utils resetRowNames
#' @importFrom kwb.utils noFactorDataFrame
trees_to_script_info <- function(x)
{
  y <- noFactorDataFrame(
    script = names(x),
    rows = sapply(x, attr, "n.lines"),
    expr = sapply(x, length),
    errors = ifelse(sapply(x, is.expression), "", "x")
  )

  # rpe = rows per expression
  y$rpe <- round(y$rows / y$expr, 1)

  resetRowNames(y[order(y$rows, decreasing = TRUE), ])
}

# trees_to_type_stat -----------------------------------------------------------
#' @importFrom kwb.utils moveColumnsToFront
#' @importFrom kwb.utils safeRowBindAll
trees_to_type_stat <- function(trees)
{
  typelist <- lapply(trees, function(tree) {
    
    frequencies <- sapply(expressions_by_class(tree), length)
    
    as.data.frame(t(frequencies))
  })

  typestat <- safeRowBindAll(typelist)
  
  typestat$script <- names(typelist)

  moveColumnsToFront(typestat, "script")
}

# expressions_by_class ---------------------------------------------------------
expressions_by_class <- function(tree)
{
  if (! is.expression(tree) || length(tree) == 0) {
    
    return()
  }

  classes <- sort(unique(sapply(tree, class)))

  result <- lapply(classes, filter_for, x = tree, FUN.filter = inherits)

  structure(result, names = classes)
}

# count_functions --------------------------------------------------------------
count_functions <- function(trees)
{
  functions <- lapply(trees, get_functions)

  unname(sapply(functions, length))
}

# get_functions ----------------------------------------------------------------
get_functions <- function(tree)
{
  filter_for(tree, is_function_assignment)
}

# expressions_per_function -----------------------------------------------------
#' @importFrom stats aggregate
expressions_per_function <- function(functionInfo)
{
  result <- merge(
    x = aggregate(n.expr ~ script, functionInfo, FUN = sum),
    y = aggregate(n.expr ~ script, functionInfo, FUN = length),
    by = "script",
    suffixes = c(".sum", ".n")
  )

  result$epf <- round(result$n.expr.sum / result$n.expr.n, 1)

  result
}
