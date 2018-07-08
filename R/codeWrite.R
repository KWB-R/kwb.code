# writeContentsToLessFiles -----------------------------------------------------
writeContentsToLessFiles <- function
(
  contents, targetDir, functionName, dbg = TRUE
)
{
  oneLineContents <- sapply(contents, paste, collapse = "\n")

  i <- 0

  while (length(oneLineContents)) {

    i <- i + 1

    # Get the first content out of the list
    content <- oneLineContents[1]

    # Select this and identical contents
    selected <- (oneLineContents == content)

    # Write this content
    file <- targetFile(targetDir, paste0(functionName, "__v"), i)
    headerLines <- paste("# found in", names(oneLineContents[selected]))
    writeContentToFile(content, file, headerLines, dbg = dbg)

    # Remove this and the identical contents
    oneLineContents <- oneLineContents[! selected]
  }

  # Return the number of files written
  i
}

# targetFile -------------------------------------------------------------------
targetFile <- function(targetDir, functionName, i)
{
  file.path(targetDir, sprintf("%s_%d.txt", functionName, i))
}

# writeContentToFile -----------------------------------------------------------
writeContentToFile <- function(content, file, headerLines, dbg = TRUE)
{
  catIf(dbg, "Writing function to", file, "...")
  writeLines(c(headerLines, content), file)
  catIf(dbg, "ok.\n")
}

# writeContentsToFiles ---------------------------------------------------------
writeContentsToFiles <- function(contents, targetDir, functionName, dbg = TRUE)
{
  names.contents <- names(contents)

  for (i in seq_along(contents)) {

    headerLines <- paste("# found in", names.contents[i])

    file <- targetFile(targetDir, functionName, i)

    writeContentToFile(contents[[i]], file, headerLines, dbg = dbg)
  }
}
