% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/walk_tree.R
\name{walk_tree}
\alias{walk_tree}
\title{Walk Along a Parse Tree}
\usage{
walk_tree(
  x,
  path = "",
  depth = 0L,
  max_depth = 20L,
  dbg = TRUE,
  config = list(),
  context = NULL
)
}
\arguments{
\item{x}{parse tree as returned by \code{\link{parse}} or a sub-tree of the
parse tree}

\item{path}{for internal use only. Path to the element in the parse tree.}

\item{depth}{for internal use only. Recursion depth.}

\item{max_depth}{maximum recursion level. Default: 20L}

\item{dbg}{whether or not to show debug messages}

\item{config}{list defining modifications of nodes in the node tree. 
TODO: describe further}

\item{context}{if not \code{NULL} (the default) this is expected to be a list
containing additional data. Currently list element "file" is used to pass
the name of the script that the current tree was read from.}
}
\description{
Walk Along a Parse Tree
}
\examples{
walk_tree(parse(text = "x <- 1:n"))
}
