\name{buildDocs}
\alias{buildDocs}
\title{Title}
\usage{
buildDocs(docsLoc, outLoc = NULL, pageList = NULL, navPill = "",
  editHref = "", copyrightText, windowTitle = NULL, root.dir = NULL,
  knit = TRUE, purl = TRUE)
}
\arguments{
  \item{docsLoc}{base directory of the .Rmd files}

  \item{outLoc}{base directory where output .html, assets,
  code, figures should go}

  \item{pageList}{optional vector of file paths to be
  included in the document build, in the order in which
  they are provided, relative to \code{docsLoc} - this is
  useful if they aren't already in order}

  \item{navPill}{HTML to go in the nav-pill (see
  \code{\link{packagePills}})}

  \item{editHref}{link to URL where the file can be edited
  (used with github)}

  \item{copyrightText}{text to go with copyright}

  \item{windowTitle}{optional title for the window}

  \item{root.dir}{passed to \code{opts_knit$set()}}

  \item{knitr}{should the document be typeset?}

  \item{purl}{should code be pulled out and put in the code
  directory?}
}
\description{
Description
}

