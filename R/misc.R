#' Create Navigation Links for Header for Package Docs
#' 
#' Create navigation header links specifically for package documentation (links include "Docs", "Function Ref", and "Github")
#' 
#' @param githubHref URL pointing to github documentation
#' @param docsActive should the docs link be active?
#' 
#' @export
packageNavPill <- function(githubHref, docsActive = TRUE) {
   docsStr <- ifelse(docsActive, "active", "")
   funcStr <- ifelse(docsActive, "", "active")
   paste("<li class='", docsStr, "'><a href='index.html'>Docs</a></li><li class='", funcStr, "'><a href='functionref.html'>Function Ref</a></li><li><a href='", githubHref, "'>Github <i class='fa fa-github'></i></a></li>", sep = "")
}

getCode <- function(dir) {
   # dir <- "~/Documents/Code/trelliscope-gh-pages/docs"
   ff <- list.files(dir, pattern="md$", full.names=TRUE)
   res <- sapply(ff, function(fl) {
      a <- readLines(fl)
      st_ind <- which(grepl("^```r", a))
      nd_inds <- which(grepl("```", a))
      cat(basename(fl), "\n")
      if(length(st_ind) > 0) {
         paste(c(
            paste("#####", basename(fl), "#####"), "\n",
            sapply(st_ind, function(st) {
               nd <- nd_inds[which(nd_inds - st > 0)[1]]
               paste(c("#####", a[(st+1):(nd-1)], "\n"), collapse="\n", sep="")
            })
         ), collapse="\n", sep="")         
      }
   })
   res <- paste(res, collapse="\n")
   cat(res, file=file.path(dir, "code.R"))
}

validID <- function(x) {
   x <- gsub(" ", "-", x)
   tolower(gsub("[^0-9a-zA-Z\\-]+", "", x))
}

