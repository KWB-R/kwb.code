# writeContentsToLessFiles -----------------------------------------------------
writeContentsToLessFiles <- function(
    contents, 
    targetDir, 
    functionName, 
    dbg = TRUE
)
{
  writeContentsToFiles(contents, targetDir, functionName, dbg, less = TRUE)
}

# writeContentsToFiles ---------------------------------------------------------
writeContentsToFiles <- function(
    contents, 
    targetDir, 
    functionName, 
    dbg = TRUE, 
    less = FALSE
)
{
  # Put each content into one line (a vector of length one)
  all_contents <- sapply(contents, paste, collapse = "\n")
  
  # Continue either with the unique contents or with all contents
  contents <- if (less) {
    unique(all_contents)
  } else {
    all_contents
  }
  
  # Create one file per content
  lapply(seq_along(contents), function(i) {
    
    # Select the corresponding content
    content <- contents[i]
    
    # Scripts where the content was found
    scripts <- if (less) {
      names(which(all_contents == content)) 
    } else {
      names(contents)[i]
    } 
    
    # Write the content to the target file
    writeContentToFile(
      content = content, 
      file = file.path(targetDir, sprintf("%s__%d.txt", functionName, i)),
      headerLines = paste("# found in", scripts), 
      dbg = dbg
    )
    
  })
}

# writeContentToFile -----------------------------------------------------------
#' @importFrom kwb.utils catAndRun
writeContentToFile <- function(content, file, headerLines, dbg = TRUE)
{
  catAndRun(
    paste("Writing function to", file), dbg = dbg,
    writeLines(c(headerLines, content), file)
  )
  
  # Return the path to the file
  file
}
