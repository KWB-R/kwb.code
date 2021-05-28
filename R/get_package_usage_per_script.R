# get_package_usage_per_script -------------------------------------------------
#' Get Package Usage per Script
#'
#' @param root root directory with R scripts
#' @param packages vector with package names to be checked 
#' @param pattern default: "\\.R$"
#' @param ... additional arguments passed to \code{\link{get_package_function_usage}}
#' @return tibble with information on used packages
#' @export
#' @importFrom stats setNames
#' @importFrom kwb.utils catAndRun
#' @importFrom dplyr bind_rows

get_package_usage_per_script <- function(root, packages, pattern = "\\.R$")
{
  # Parse all script within this root folder
  tree <- parse_scripts(
    root, scripts = dir(root, pattern, recursive = TRUE)
  )
  
  # How many scripts have been read?
  message(length(tree), " scripts have been parsed.")
  
  # For each package, check which script uses functions of this package
  who_uses_what <- lapply(stats::setNames(nm = packages), function(package) {
    kwb.utils::catAndRun(paste("Checking usage of", package), newLine = 3, {
      try(get_package_function_usage(tree,
                                     package = package,
                                     ...
                                     ))
    })
  })
  
  # Clean the information on who uses what
  failed <- sapply(who_uses_what, inherits, "try-error")
  
  dplyr::bind_rows(.id = "package", who_uses_what[! failed])
}