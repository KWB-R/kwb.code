# get_string_constants_in_scripts ----------------------------------------------

#' Get Frequency of String Constant Usage in R Scripts
#' 
#' @param root path to folder in which to look for R scripts
#' @param scripts optional. Paths to R scripts in which to search for string
#'   constants, relative to \code{root}
#' @param two_version_check if \code{TRUE} (default), two different 
#'   implementations of this function are used and the results are compared
#'   internally. Set this argument to \code{FALSE} to get the result as fast
#'   as possible.
#' @export
#' @return data frame with columns \code{file_id} (file identifier),
#'   \code{string} (string constant found in the file) and \code{count} (number
#'   of occurrences of the string counted in the file). The file identifier can
#'   be resolved to a full file name using the "file database" that is stored in
#'   the attribute "file_db".
#' @examples
#' root <- system.file(package = "kwb.code")
#' constants <- get_string_constants_in_scripts(root)
#' 
#' # Get paths to files from "file database" stored in attribute "file_db"
#' kwb.utils::getAttribute(constants, "file_db")
#' 
get_string_constants_in_scripts <- function(
  root, scripts = dir(root, "\\.[Rr]$", recursive = TRUE), 
  two_version_check = TRUE
) {

  if (FALSE) {
    kwb.utils::assignPackageObjects("kwb.code")
    #kwb.utils::assignArgumentDefaults(kwb.code:::get_string_constants_in_scripts)
    root <- system.file(package = "kwb.fakin")
    scripts = dir(root, "\\.[Rr]$", recursive = TRUE)
  }
  
  file_db <- kwb.file::to_file_database(files = file.path(root, scripts))
  
  tree <- kwb.code::parse_scripts(root, scripts)
  
  names(tree) <- file_db$files$file_id
  
  strings <- fetch_string_constants_1(tree)
  
  if (two_version_check) {
    string_constants_2 <- fetch_string_constants_2(tree)
    stopifnot(identical(strings, string_constants_2))
  }
  
  result <- lapply(strings, function(x) if (! is.null(x)) {
    f <- table(x)
    kwb.utils::noFactorDataFrame(string = names(f), count = as.integer(f))
  })
  
  structure(dplyr::bind_rows(result, .id = "file_id"), file_db = file_db)
}

# fetch_string_constants_1 -----------------------------------------------------
fetch_string_constants_1 <- function(tree)
{
  if (is.list(tree) || length(tree) > 1) {
    
    result <- lapply(tree, fetch_string_constants_1)
    
    if (is.list(tree)) {
      result
    } else {
      unname(unlist(result))
    }
    
  } else if (is.character(tree)) {
    tree
  }
}

# fetch_string_constants_2 -----------------------------------------------------
fetch_string_constants_2 <- function(tree)
{
  extract_from_parse_tree(tree, matches = matches_string)
}

# matches_string ---------------------------------------------------------------
matches_string <- function(x, parent, index) 
{
  if (is.character(x)) {
    structure(TRUE, name = x)
  } else {
    return(FALSE)
  }
}
