#' Show String Constants Used in R Scripts
#'
#' @root path from which to look recursively for R scripts
#' @export
find_string_constants <- function(root = "./R")
{
  kwb.file::add_file_info(get_string_constants_in_scripts(
      root = root, FUN = fetch_string_constants_2
  ))
}
