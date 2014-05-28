\name{buildFunctionRef}
\alias{buildFunctionRef}
\title{Build Function Reference}
\usage{
buildFunctionRef(packageLoc, outLoc = NULL, navPill = "", copyrightText,
  windowTitle = NULL)
}
\arguments{
  \item{packageLoc}{base directory of the R package}

  \item{outLoc}{base directory where output .html, assets,
  code, figures should go}

  \item{navPill}{HTML to go in the nav-pill (see
  \code{\link{packagePills}})}

  \item{copyrightText}{text to go with copyright}

  \item{windowTitle}{optional title for the window}
}
\description{
Build function reference using staticdocs
}

