# Configure modifications ------------------------------------------------------
config <- list(checks = list(
  seq_1_n = list(
    check = function(x) {
      if (!is.call(x)) return(FALSE)
      #str(as.list(x))
      #substr(deparse(x)[1L], 1L, 2L) == "1:"
      identical(x[[1]], as.name(":")) &&
        identical(x[[2]], 1) &&
        is.name(x[[3]])
    },
    report = function(x) {
      cat("\nExpression of type '1:n' found: ")
      print(x)
    }
    # , modify = function(x) {
    #   testthat::expect_true(is.call(x))
    #   testthat::expect_identical(x[[1]], as.name(":"))
    #   testthat::expect_identical(x[[2]], 1)
    #   x[[1L]] <- as.name("seq_len")
    #   x[[2L]] <- x[[3L]]
    #   result <- x[-3L]
    #   #str(result)
    #   result
    # }
  ),
  seq_len_length = list(
    check = function(x) {
      if (!is.call(x)) return(FALSE)
      if (!identical(x[[1L]], as.name("seq_len"))) return(FALSE)
      #str(as.list(x))
      grepl("^length\\(", deparse(x[[2L]]))
    },
    report = function(x) {
      cat("\nExpression of type 'seq_len(length(x))' found: ")
      print(x)
    }
  ),
  seq_1_length = list(
    check = function(x) {
      if (!is.call(x)) return(FALSE)
      if (!identical(x[[1L]], as.name(":"))) return(FALSE)
      if (!identical(x[[2L]], 1)) return(FALSE)
      #str(as.list(x))
      grepl("^length\\(", deparse(x[[3L]]))
    },
    report = function(x) {
      cat("\nExpression of type '1:length(x)' found: ")
      print(x)
    }
  )
  
))

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  #files <- dir_r_files("R")
  files <- dir_r_files("~/github-repos/K")
  files <- dir_r_files("~/R-Development/RScripts")

  cat("\n     ")
  
  # Apply the configuration for all files
  for (i in seq_along(files)) {
    file <- files[i]
    #cat(sprintf("%3d: %s\n", i, file))
    cat(sprintf("%s%5d", kwb.utils::backspace(5L), i))
    tree <- parse(file, keep.source = TRUE)
    new_tree <- try(kwb.code::walk_tree(
      tree, 
      dbg = FALSE, 
      config = config, 
      context = list(file = file)
    ))
    if (!kwb.utils::isTryError(new_tree)) {
      try(stopifnot(identical(tree, new_tree)))
    }
  }

  save_deparsed_for_comparison(tree, new_tree)  
}

# dir_r_files ------------------------------------------------------------------
dir_r_files <- function(path, recursive = TRUE)
{
  dir(
    path, "\\.R$", ignore.case = TRUE, full.names = TRUE, recursive = recursive
  )
}

# save_deparsed_for_comparison -------------------------------------------------
save_deparsed_for_comparison <- function(tree1, tree2)
{
  writeLines(deparse(tree1), "parse-tree_1.txt")
  writeLines(deparse(tree2), "parse-tree_2.txt")
  kwb.utils::hsOpenWindowsExplorer(getwd())
}
