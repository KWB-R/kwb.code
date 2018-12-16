# get_string_constants_in_scripts ----------------------------------------------
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
  
  file_db <- kwb.file::to_file_database(file.path(root, scripts))
  
  tree <- kwb.code::parse_scripts(root, scripts)
  
  names(tree) <- file_db$files$file_id
  
  string_constants <- fetch_string_constants_1(tree)
  
  if (two_version_check) {
    string_constants_2 <- fetch_string_constants_2(tree)
    stopifnot(identical(string_constants, string_constants_2))
  }
  
  result <- lapply(string_constants, function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    f <- table(x)
    data.frame(
      string = names(f), count = as.integer(f), stringsAsFactors = FALSE
    )
  })
  
  structure(dplyr::bind_rows(result, .id = "file"), scripts = scripts)
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
