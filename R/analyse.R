# analyse ----------------------------------------------------------------------

#' Analyse the Parse Tree of an R Script
#'
#' @param x parse tree as returned by \code{\link{parse}}
#' @param path for internal use only (when this function is called recursively)
#' @return list representing type information on the nodes in the parse tree
#' @export
#' @examples
#' # Parse an R script file (here, a file from kwb.utils)
#' x <- parse("https://raw.githubusercontent.com/KWB-R/kwb.utils/master/R/log.R")
#' 
#' # Analyse the parse tree (This may take some time!)
#' result <- kwb.code::analyse(x)
#' 
#' # Show the structure of the result list (only 3 levels!)
#' str(result, 3)
analyse <- function(x, path = "")
{
  result <- type_info(x)
  result[["fulltype"]] <- info_to_text(result)
  result[["path"]] <- path
  
  if (is.recursive(x)) {
    
    result[["children"]] <- lapply(
      X = seq_along(x), 
      FUN = function(i) {
        analyse(x[[i]], path = paste0(path, "/", i))
      }
    )
  }
  
  result  
}

# type_info --------------------------------------------------------------------
type_info <- function(x, as_character = FALSE)
{
  #shorten <- function(x) kwb.utils::shorten(x, max_chars = 30L)
  shorten <- function(x) paste(substr(x, 1, 30), "...")
  
  text <- as.character(x)
  
  mode_x <- mode(x)
  class_x <- class(x)
  
  info <- list(
    type = typeof(x),
    mode = mode_x,
    class = class_x, 
    length = length(x),
    text = shorten(paste0("[", seq_along(text), "]", text, collapse = "")),
    is = if (length(x) == 1L) is_what(x),
    n_modes = length(mode_x),
    n_classes = length(class_x)
  )

  if (as_character) {
    info_to_text(info)
  } else {
    info
  }
}

# info_to_text -----------------------------------------------------------------
#' @importFrom kwb.utils commaCollapsed
info_to_text <- function(info)
{
  collapse <- function(element) {
    kwb.utils::selectElements(info, element) %>% 
      kwb.utils::commaCollapsed()
  }
  
  #prefix <- "type|mode|class|length|is: "
  prefix <- NULL
  
  paste0(
    prefix, 
    paste(collapse = "|", c(
      collapse("type"),
      collapse("mode"),
      collapse("class"), 
      collapse("length"),
      collapse("is")
    )) 
  )
}
