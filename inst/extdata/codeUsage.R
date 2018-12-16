# M A I N ----------------------------------------------------------------------
if (FALSE)
{
  library(dplyr)

  # Required on English systems
  Sys.setlocale(locale = "C")
  
  # Define different roots
  root_package_fakin <- system.file(package = "kwb.fakin")
  root_rdev <- "~/Desktop/R-Development"
  root <- root_package_fakin

  # Parse script(s) into a tree structure
  tree <- kwb.code::parse_scripts(root = "C:/Users/hsonne/Documents/R-Development")
  tree <- kwb.code::parse_scripts(root = "~/Desktop/R-Development")
  tree <- kwb.code::parse_scripts(root, scripts = "extdata/scripts/create_treemaps.R")
  tree <- kwb.code::parse_scripts(root)
  
  # Compare different versions of get_function_call_frequency()
  stats1 <- kwb.fakin:::get_function_call_frequency(tree, simple = TRUE) %>%
    bind_rows(.id = "script")
  
  stats2 <- kwb.fakin:::get_function_call_frequency(tree, simple = FALSE) %>%
    bind_rows(.id = "script")

  stats1
  stats2
  
  used_functions <- kwb.fakin::get_package_function_usage(
    tree, package = "kwb.utils"
  )
  
  View(package_function_usage)
  
  stats <- dplyr::full_join(stats1, stats2, by = c("script", "name")) %>%
    filter(is.na(count.x) | is.na(count.y) | count.x != count.y)
  
  View(stats)
  
  system.time(stats <- kwb.code::get_string_constants_in_scripts(
    root, two_version_check = FALSE
  ))
  
  stats <- stats %>% kwb.file::add_file_info()
  
  stats <- stats %>% filter(count > 10) %>% arrange(file_name, desc(count))

  View(stats)
  
  stats_2 <- stats %>% 
    group_by(string) %>%
    summarise(count = sum(count)) %>% 
    mutate(string = paste0("'", string, "'"))
  arrange(desc(count)) %>% 
    as.data.frame()
  
  grep("^'\\s*'$", stats_2$string, value = TRUE)
  
  View(stats_2)
}

# Details ----------------------------------------------------------------------
if (FALSE)
{
  root <- root_package_fakin
  tree <- kwb.code::parse_scripts(root)
  
  # Check which functions from kwb.utils are used and how often
  kwb.fakin:::get_package_function_usage(tree, package = "kwb.utils")
  
  kwb.fakin:::get_package_function_usage(tree, package = "kwb.utils")
  kwb.fakin:::get_package_function_usage(tree[5], package = "kwb.utils")
  
  all_raw_lines <- readLines(file.path(root, names(tree[1])))
  one_line <- paste(all_raw_lines, collapse = " ")
  
  
  (raw_lines <- all_raw_lines[11:20])
  
  pattern <- "[A-Za-z][A-Za-z0-9.]*(::)?[A-Za-z][A-Za-z0-9._]*\\("
  
  (function_calls <- sort(unlist(stringr::str_extract_all(raw_lines, pattern))))
  
  function_frequency <- sort(table(function_calls), decreasing = TRUE)
  
  names(function_frequency) <- gsub("\\($", "", names(function_frequency))
  
  stats::setNames(as.data.frame(function_frequency, stringsAsFactors = FALSE), 
                  c("name", "count"))
  
  tree[[c(1,2,2)]]
  kwb.code:::is_assignment(x <- tree[[c(1,1)]])
  kwb.code:::split_assignment(x)
  
  is.list(tree)
  mode(tree[[1]])
  e <- tree[[1]]
  length(e)
  mode(e[[1]])
  mode(e[[2]])
  mode(e[[3]])
  
  is.call(e[[1]])
  e[[1]]
  
  s1 <- summary(e1 <- e)
  s2 <- summary(e2 <- e[[c(1)]])
  s3 <- summary(e3 <- e[[c(1,3)]])
  
  s1
  s2
  str(s3)
  
  is_string_constant(e3)
  is.character(e1)
  mode(e3)
  
  kwb.code:::get_functions(tree)
  x <- tree[[1]]
  x <- tree[[c(1,1)]]
  (x <- tree[[c(1,1,1)]])
  
  (x <- tree[[c(1,1,2)]])
  (x <- tree[[c(1,1,3)]])
  summary(x)
}


if (FALSE) {
  
  (x <- tree[[1]])
  (x <- tree[[c(1, 7)]])
  (x <- tree[[c(1, 7, 3)]])
  (x <- tree[[c(1, 7, 3, 3)]])
  (x <- tree[[c(1, 7, 3, 3, 3)]])
  (x <- tree[[c(1, 7, 3, 3, 3, 2)]]) # do.call(...)
  (x <- tree[[c(1, 7, 3, 3, 3, 2, 1)]]) # do.call
  (x <- tree[[c(1, 7, 3, 3, 3, 2, 2)]]) # rbind
  (x <- tree[[c(1, 7, 3, 3, 3, 2, 3)]]) # path_infos[department_strings]
  
  (x <- tree[[c(1, 7, 3, 4)]]) # png_files <- ...
  (x <- tree[[c(1, 7, 3, 4, 1)]]) # "<-"
  (x <- tree[[c(1, 7, 3, 4, 2)]]) # png_file
  (x <- tree[[c(1, 7, 3, 4, 3)]]) # kwb.fakin::plot_treemaps_from_path_data(...)
  (x <- tree[[c(1, 7, 3, 4, 3, 1)]]) # kwb.fakin::plot_treemaps_from_path_data(...)
  (x <- tree[[c(1, 7, 3, 4, 3, 2)]]) # kwb.fakin::plot_treemaps_from_path_data(...)
  (x <- tree[[c(1, 7, 3, 4, 3, 3)]]) # kwb.fakin::plot_treemaps_from_path_data(...)
  (x <- tree[[c(1, 7, 3, 4, 3, 4)]]) # kwb.fakin::plot_treemaps_from_path_data(...)
  (x <- tree[[c(1, 7, 3, 4, 3, 5)]]) # kwb.fakin::plot_treemaps_from_path_data(...)
  (x <- tree[[c(1, 7, 3, 4, 3, 6)]]) # kwb.fakin::plot_treemaps_from_path_data(...)
  (x <- tree[[c(1, 7, 3, 4, 3, 6, 1)]]) # kwb.fakin::plot_treemaps_from_path_data(...)
  (x <- tree[[c(1, 7, 3, 4, 3, 6, 2)]]) # kwb.fakin::plot_treemaps_from_path_data(...)
  (x <- tree[[c(1, 7, 3, 4, 3, 6, 3)]]) # kwb.fakin::plot_treemaps_from_path_data(...)
  
  inspect(x, check = check_function)
  
  length()
  
  is.call(x[[c(3,2,3)]])
  x[[c(3,2,3,1)]]
  
  collect_called_functions(x)
  
  
  collect_called_functions(x[[1]])
  collect_called_functions(x[[2]])
  collect_called_functions(x[[3]])
}

# get_function_name_language_object --------------------------------------------
get_function_name_language_object <- function(x) {
  parts <- as.character(x)
  if (is.language(x) && length(x) == 3 && parts[1] %in% c("::", ":::")) {
    return(paste(parts[c(2, 1, 3)], collapse = ""))
  }
  return("")
}

# is_if_symbol -----------------------------------------------------------------
is_if_symbol <- function(x) {
  identical(x, as.symbol("if"))  
}

# is_assignment_symbol ---------------------------------------------------------
is_assignment_symbol <- function(x)
{
  identical(x, as.symbol("<-"))
}

# is_FALSE ---------------------------------------------------------------------
is_FALSE <- function(x) {
  identical(x, FALSE)
}

# is_TRUE ----------------------------------------------------------------------
is_TRUE <- function(x) {
  identical(x, TRUE)
}

# is_opening_brace -------------------------------------------------------------
is_opening_brace <- function(x) {
  identical(x, as.symbol("{"))
}

# is_do.call_call --------------------------------------------------------------
is_do.call_call <- function(x)
{
  is.call(x) && as.character(x[[1]]) == "do.call"
}

# to_function_name--------------------------------------------------------------
to_function_name <- function(x) {
  
  result <- as.character(x)
  
  n_parts <- length(result)
  
  if (n_parts == 1) {
    return(result)
  }
  
  if (n_parts == 3) {
    return(paste(result[c(2, 1, 3)], collapse = ""))
  }
  
  stop("to_function_name() does not know how to handle this: ", 
       capture.output(x))
}

# collect_called_functions -----------------------------------------------------
collect_called_functions <- function(x)
{
  callers <- NULL
  if (
    is_if_symbol(x) || is_FALSE(x) || is_TRUE(x) || is_opening_brace(x) ||
    is_assignment_symbol(x)
  ) {
    return(NULL)
  }
  if (FALSE) {
    if (! is.null(name <- get_private_package_function(x))) {
      return(name)
    }
    if (! is.null(name <- get_public_package_function(x))) {
      return(name)
    }
  }
  
  is_package_function <- if (is.call(x)) {
    
    (name <- to_function_name(x[[1]]))
    
    if (name == "::") {
      (callers <- c(callers, get_public_package_function(x)))
      TRUE
    } else if (name == ":::") {
      (callers <- c(callers, get_private_package_function(x)))
      TRUE
    } else if (name == "do.call") {
      arguments <- as.list(x)[-1]
      index <- which(names(arguments) == "what")
      called <- as.character(arguments[[if (length(index)) index else 1]])
      (callers <- c(callers, "do.call", called))
      FALSE
    } else if (! name %in% c("<-", "if", "{")) {
      if (! grepl("::", name)) {
        (callers <- c(callers, name))
      }
      FALSE
    } else {
      FALSE
    }
  } else {
    FALSE
  }
  
  if ((is.list(x) || length(x) > 1) && ! is_package_function) {
    
    result <- lapply(x, collect_called_functions)
    
    if (is.list(x)) {
      
      result
      
    } else {
      
      unname(c(callers, unlist(result)))
    }
  } else {
    
    callers
  }
}

# get_private_package_function -------------------------------------------------
get_private_package_function <- function(x)
{
  if (length(x) == 3 && is.name(x[[1]]) && as.character(x[[1]]) == ":::") {
    
    do.call(paste0, lapply(x, as.character)[c(2, 1, 3)])
  }
}

# get_public_package_function --------------------------------------------------
get_public_package_function <- function(x)
{
  if (length(x) == 3 && is.name(x[[1]]) && as.character(x[[1]]) == "::") {
    
    do.call(paste0, lapply(x, as.character)[c(2, 1, 3)])
  }
}
