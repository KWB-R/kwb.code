# analyse ----------------------------------------------------------------------
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
