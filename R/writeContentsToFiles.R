# writeContentsToLessFiles -----------------------------------------------------
writeContentsToLessFiles <- function(
  contents, targetDir, functionName, dbg = TRUE
)
{
  # Put each content into one line (a vector of length one)
  contents <- sapply(contents, paste, collapse = "\n")

  # Find the unique contents
  unique_contents <- unique(contents)

  # Index vector to walk along unique_contents
  indices <- seq_along(unique_contents)
  
  # Target file names
  files <- targetFile(targetDir, paste0(functionName, "__v"), i)
  
  for (i in indices) {
    
    # Select the corresponding content
    content <- unique_contents[i]
    
    # Scripts where the content was found
    scripts <- names(which(contents == content))

    # Write the content to the target file
    writeContentToFile(
      content = content, 
      file = files[i], 
      headerLines = found_in_scripts_header(scripts), 
      dbg = dbg
    )
  }
  
  # Return the paths to the files written
  files
}

# targetFile -------------------------------------------------------------------
targetFile <- function(targetDir, functionName, i)
{
  file.path(targetDir, sprintf("%s_%d.txt", functionName, i))
}

# found_in_scripts_header ------------------------------------------------------
found_in_scripts_header <- function(scripts)
{
  paste("# found in", scripts)  
}

# writeContentToFile -----------------------------------------------------------
#' @importFrom kwb.utils catAndRun
writeContentToFile <- function(content, file, headerLines, dbg = TRUE)
{
  catAndRun(
    paste("Writing function to", file), dbg = dbg,
    writeLines(c(headerLines, content), file)
  )
}

# writeContentsToFiles ---------------------------------------------------------
writeContentsToFiles <- function(contents, targetDir, functionName, dbg = TRUE)
{
  content_names <- names(contents)

  lapply(seq_along(contents), function(i) {
    writeContentToFile(
      content = contents[[i]], 
      file = targetFile(targetDir, functionName, i), 
      headerLines = found_in_scripts_header(scripts = content_names[i]), 
      dbg = dbg
    )
  })
}
