# writeContentsToLessFiles -----------------------------------------------------
writeContentsToLessFiles <- function(
  contents, targetDir, functionName, dbg = TRUE
)
{
  oneLineContents <- sapply(contents, paste, collapse = "\n")
  uniqueContents <- unique(oneLineContents)

  # Index vector to walk along uniqueContents
  indices <- seq_along(uniqueContents)
  
  # Target file names
  files <- targetFile(targetDir, paste0(functionName, "__v"), i)
  
  for (i in indices) {
    
    # Select the corresponding content
    content <- uniqueContents[i]
    
    # Header lines naming the scripts where the content was found
    header <- paste("# found in", names(which(oneLineContents == content)))
    
    # Write the content to the target file
    writeContentToFile(content, files[i], header, dbg = dbg)
  }
  
  # Return the paths to the files written
  files
}

# targetFile -------------------------------------------------------------------
targetFile <- function(targetDir, functionName, i)
{
  file.path(targetDir, sprintf("%s_%d.txt", functionName, i))
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

  for (i in seq_along(contents)) {

    header <- paste("# found in", content_names[i])

    file <- targetFile(targetDir, functionName, i)

    writeContentToFile(contents[[i]], file, header, dbg = dbg)
  }
}
