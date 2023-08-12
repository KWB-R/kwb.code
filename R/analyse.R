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
  info <- type_info(x)
  
  # result <- list(
  #   self = info_to_text(info)
  # )
  
  result <- info
  result[["path"]] <- path
  result[["fulltype"]] <- info_to_text(info)
  
  if (is.recursive(x)) {
    
    result[["children"]] <- lapply(seq_along(x), function(i) {
      analyse(x[[i]], path = paste(path, i, sep = "/"))
    })
  }
  
  result  
}

# type_info --------------------------------------------------------------------
type_info <- function(x, as.character = FALSE)
{
  #shorten <- function(x) kwb.utils::shorten(x, max_chars = 30L)
  shorten <- function(x) paste(substr(x, 1, 30), "...")
  
  text <- as.character(x)
  
  info <- list(
    type = typeof(x),
    mode = mode(x),
    class = class(x), 
    length = length(x),
    text = shorten(paste0("[", seq_along(text), "]", text, collapse = "")),
    is = if (length(x) == 1L) {
      suppressWarnings(is_what(x, silent = TRUE))
    }
  )
  
  info <- c(info, list(
    n_modes = length(info$mode),
    n_classes = length(info$class)
  ))
  
  if (! as.character) {
    return(info)
  }
  
  info_to_text(info)
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
