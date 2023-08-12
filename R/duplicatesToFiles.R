# duplicatesToFiles ------------------------------------------------------------
#' @importFrom kwb.utils catIf
#' @importFrom kwb.utils createDirectory
#' @importFrom kwb.utils selectColumns
#' @importFrom kwb.utils selectElements
duplicatesToFiles <- function
(
  trees, 
  fun_duplicates = NULL, 
  function_name = NULL, 
  target_root = tempdir(), 
  dbg = TRUE,
  write.all = FALSE
)
{
  if (is.null(fun_duplicates)) {
    fun_duplicates <- get_info_on_duplicated_function_names(trees)
  }
  
  if (nrow(fun_duplicates) == 0L) {
    message("No duplications given or no duplications found.")
    return()
  }
  
  function_names <- kwb.utils::selectColumns(fun_duplicates, "functionName")

  # Call this function for each function name if no function name is given  
  if (is.null(function_name)) {
    
    message("No function name given.")
    
    lapply(unique(function_names), function(function_name) {
      #function_name <- unique(function_names)[1L]
      kwb.utils::catAndRun(
        sprintf(
          "Calling duplicatesToFiles(..., function_name = \"%s\")", 
          function_name
        ), 
        duplicatesToFiles(trees, fun_duplicates, function_name),
        newLine = 3L
      )
    })
  }

  # Script files that contain a function <function_name>  
  scripts <- fun_duplicates[function_names == function_name, ] %>%
    selectColumns("script") %>%
    as.character()

  # From each script, extract the definition of function <function_name>
  function_defs <- lapply(stats::setNames(nm = scripts), function(script) {
    trees %>% 
      kwb.utils::selectElements(script) %>%
      extract_function_definition(function_name)
  })
  
  target_dir <- file.path(target_root, "clean", function_name)
  target_dir <- kwb.utils::createDirectory(target_dir, dbg = FALSE)

  contents <- lapply(function_defs, function(x) deparse(x[[3L]]))

  # Write one file per function definition
  if (write.all) {
    writeContentsToFiles(contents, target_dir, function_name, dbg = dbg)
  }

  # Write one file per unique function definition
  files <- writeContentsToLessFiles(
    contents, target_dir, function_name, dbg = dbg
  )

  if (length(files) != length(contents)) {
    message("There are identical definitions for ", function_name)
  }

  files
}

# get_info_on_duplicated_function_names ----------------------------------------
get_info_on_duplicated_function_names <- function(trees)
{
  function_info <- get_full_function_info(trees)
  n_definitions <- selectColumns(function_info, "n.def")
  function_info[n_definitions > 1L, ]
}

# extract_function_definition --------------------------------------------------
extract_function_definition <- function(tree, function_name) {

  tree <- tree[sapply(tree, is_function_assignment)]

  fnames <- sapply(tree, function(x) split_function_assignment(x)$functionName)

  index <- which(fnames == function_name)

  n_defs <- length(index)

  if (n_defs == 0L) {
    kwb.utils::stopFormatted(
      "No such function: '%s' defined in the given tree", 
      function_name
    )
  }
  
  if (n_defs > 1L) {
    warning(
      "The function '", function_name, "' is defined multiple times in ",
      "the given tree. I return the first definition!"
    )
  }

  tree[[index[1L]]]
}
