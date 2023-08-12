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
#' # By default, all R scripts below the root are parsed
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
  
  stats::setNames(trees, scripts)
}
