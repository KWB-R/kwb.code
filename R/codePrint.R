# print_elements_with_modes ----------------------------------------------------
print_elements_with_modes <- function(x) 
{
  #stopifnot(is.list(x))
  
  for (element in as.list(x)) {
    cat("mode: ", mode(element), "\n")
    cat(as.character(element), "\n")
  }
}
