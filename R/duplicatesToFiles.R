# duplicatesToFiles ------------------------------------------------------------
#' @importFrom kwb.utils catIf
#' @importFrom kwb.utils createDirectory
#' @importFrom kwb.utils selectColumns
#' @importFrom kwb.utils selectElements
duplicatesToFiles <- function
(
  trees, fun_duplicates, function_name, target_root = tempdir(), dbg = TRUE,
  write.all = FALSE
)
{
  function_names <- selectColumns(fun_duplicates, "functionName")
  
  scripts <- fun_duplicates[function_names == function_name, ] %>%
    selectColumns("script") %>%
    as.character()

  function_defs <- scripts %>%
    lapply(function(script) {
      trees %>% 
        selectElements(script) %>%
        extract_function_definition(function_name)
    }) %>%
    stats::setNames(scripts)

  target_dir <- file.path(target_root, "clean", function_name)
  target_dir <- createDirectory(target_dir, dbg = FALSE)

  contents <- lapply(function_defs, function(x) deparse(x[[3L]]))

  # Write one file per function definition
  if (write.all) {
    writeContentsToFiles(contents, target_dir, function_name, dbg = dbg)
  }

  # Write one file per unique function definition
  n_files <- writeContentsToLessFiles(
    contents, target_dir, function_name, dbg = dbg
  )

  if (n_files != length(contents)) {
    message("There are identical definitions for ", function_name)
  }

  target_dir
}

# extract_function_definition --------------------------------------------------
extract_function_definition <- function(tree, function_name) {

  tree <- tree[sapply(tree, is_function_assignment)]

  fnames <- sapply(tree, function(x) split_function_assignment(x)$functionName)

  index <- which(fnames == function_name)

  n_defs <- length(index)

  if (n_defs == 0L) {
    stop("No such function: '", function_name, "' defined in the given tree")
  }

  if (n_defs > 1L) {
    warning(
      "The function '", function_name, "' is defined multiple times in ",
      "the given tree. I return the first definition!"
    )
  }

  tree[[index[1L]]]
}
