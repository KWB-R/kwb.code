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
  sprintf(
    paste0(#"type|mode|class|length|is: ", 
      "%s|%s|%s|%d|%s"), 
    info$type,
    kwb.utils::commaCollapsed(info$mode),
    kwb.utils::commaCollapsed(info$class), 
    info$length,
    kwb.utils::commaCollapsed(info$is)
  )
}
