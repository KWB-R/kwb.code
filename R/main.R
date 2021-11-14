# parse_scripts ----------------------------------------------------------------
#' Parse all given R scripts into a tree structure
#'
#' @param root root directory to which the relative paths given in
#'   \code{scripts} relate
#' @param scripts relative file paths to R scripts. By default all files ending
#'   with ".R" or ".r" below the \code{root} folder (recursively) are parsed.
#' @param dbg if \code{TRUE} debug messages are shown
#'
#' @export
#'
#' @importFrom kwb.utils catAndRun
#'
#' @seealso \code{\link{to_full_script_info}}
#'
#' @examples
#' \dontrun{
#' # Download some example code files from github...
#' url.base <- "https://raw.githubusercontent.com/hsonne/blockrand2/master/R/"
#' urls <- paste0(url.base, c("blockrand2_create.R", "blockrand2_main.R"))
#'
#' targetdir <- file.path(tempdir(), "blockrand2")
#' targetdir <- kwb.utils::createDirectory(targetdir)
#'
#' for (url in urls) {
#'   download.file(url, file.path(targetdir, basename(url)))
#' }
#'
#' # By default, all R scripts below the root are parse
#' trees <- parse_scripts(root = targetdir)
#'
#' # All elements of trees are expressions
#' sapply(trees, is.expression)
#'
#' # Analyse the scripts on the script level
#' scriptInfo <- to_full_script_info(trees)
#'
#' scriptInfo
#'
#' # Analyse the scripts on the function level
#' functionInfo <- get_full_function_info(trees)
#'
#' functionInfo
#' }
parse_scripts <- function
(
  root,
  scripts = dir(root, "\\.R$", ignore.case = TRUE, recursive = TRUE),
  dbg = TRUE
)
{
  trees <- lapply(scripts, function(x) {
    
    file <- file.path(root, x)
    
    content <- catAndRun(
      paste("Reading", file), dbg = dbg, readLines(file, warn = FALSE)
    )

    expressions <- try(parse(text = content))
    
    structure(expressions, n.lines = length(content))
  })

  structure(trees, names = scripts)
}

# get_full_function_info -------------------------------------------------------
#'
#' Get information on function definitions in parsed R scripts
#'
#' @param trees list of R script parse trees as provided by
#'   \code{\link{parse_scripts}}
#'
#' @importFrom kwb.utils rbindAll
#' @importFrom kwb.utils moveColumnsToFront
#'
#' @export
#'
#' @seealso \code{\link{parse_scripts}}
get_full_function_info <- function(trees)
{
  infos <- lapply(trees, function(tree) {

    functions <- get_functions(tree)

    rbindAll(lapply(functions, get_function_info))
  })

  functionInfo <- rbindAll(infos, nameColumn = "script")

  count <- multi_defined_functions(functionInfo)

  functionInfo <- merge(functionInfo, count, by = "functionName")

  moveColumnsToFront(functionInfo, c("script", "functionName", "n.def"))
}

# multi_defined_functions ------------------------------------------------------
multi_defined_functions <- function(functionInfo)
{
  count <- aggregate(
    n.def ~ functionName,
    cbind(n.def = seq_len(nrow(functionInfo)), functionInfo),
    length
  )

  count[order(count$n, decreasing = TRUE), ]
}

# merge_function_info ----------------------------------------------------------
merge_function_info <- function(scriptInfo, functionInfo)
{
  funExpressions <- expressions_per_function(functionInfo)

  merge(
    scriptInfo,
    funExpressions[, c("script", "epf")],
    by = "script",
    all.x = TRUE
  )
}

# filter_scripts ---------------------------------------------------------------
#' @importFrom kwb.utils matchesCriteria
#' @importFrom kwb.utils removeEmptyColumns
filter_scripts <- function(scriptInfo, fun.min = 5, epf.min = 10)
{
  criteria <- c(paste("fun >=", fun.min), paste("epf >=", epf.min))

  scriptInfo <- scriptInfo[matchesCriteria(scriptInfo, criteria), ]

  removeEmptyColumns(scriptInfo)
}
