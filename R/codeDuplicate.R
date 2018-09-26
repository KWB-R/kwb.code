# duplicatesToFiles ------------------------------------------------------------
#' @importFrom kwb.utils catIf
#' @importFrom kwb.utils createDirectory
#' @importFrom kwb.utils selectColumns
#' @importFrom kwb.utils selectElements
duplicatesToFiles <- function
(
  trees, fun_duplicates, functionName, targetRoot = tempdir(), dbg = TRUE,
  write.all = FALSE
)
{
  selected <- selectColumns(fun_duplicates, "functionName") == functionName

  scripts <- as.character(selectColumns(fun_duplicates[selected, ], "script"))

  functionDefs <- lapply(scripts, function(script) {
    
    extract_function_definition(selectElements(trees, script), functionName)
  })

  names(functionDefs) <- scripts

  targetDir <- file.path(targetRoot, "clean", functionName)

  targetDir <- createDirectory(targetDir, dbg = FALSE)

  contents <- lapply(functionDefs, function(functionDef) {
    
    deparse(functionDef[[3]])
  })

  # Write one file per function definition
  if (write.all) {
    
    writeContentsToFiles(contents, targetDir, functionName, dbg = dbg)
  }

  # Write one file per unique function definition
  n.files <- writeContentsToLessFiles(contents, targetDir, functionName, dbg = dbg)

  if (n.files != length(contents)) {
    
    message("There are identical definitions for ", functionName)
  }

  targetDir
}

# extract_function_definition --------------------------------------------------
extract_function_definition <- function(tree, functionName)
{
  tree <- tree[sapply(tree, is_function_assignment)]

  functionNames <- sapply(tree, function(x) {
    
    split_function_assignment(x)$functionName
  })

  index <- which(functionNames == functionName)

  n.defs <- length(index)

  if (n.defs == 0) {
    
    stop("No such function: '", functionName, "' defined in the given tree")
  }

  if (n.defs > 1) {
    
    warning(
      "The function '", functionName, "' is defined multiple times in ",
      "the given tree. I return the first definition!"
    )
  }

  tree[[index[1]]]
}
