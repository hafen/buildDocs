
# creates directories "markdown", "code"
# markdown is intermediate, typically don't commit that

#' Title
#'
#' Description
#' 
#' @param docsLoc base directory of the .Rmd files
#' @param outLoc base directory where output .html, assets, code, figures should go
#' @param pageList optional vector of file paths to be included in the document build, in the order in which they are provided, relative to \code{docsLoc} - this is useful if they aren't already in order
#' @param optTemplates optional template content - list of named elements - currently \code{navpills} and \code{brand} (see \code{\link{packagePills}})
#' @param editHref link to URL where the file can be edited (used with github)
#' @param copyrightText text to go with copyright
#' @param windowTitle optional title for the window
#' @param root.dir passed to \code{opts_knit$set()}
#' @param knitr should the document be typeset?
#' @param purl should code be pulled out and put in the code directory?
#' @export
buildDocs <- function(docsLoc, outLoc = NULL, pageList = NULL, optTemplates = NULL, editHref = "", copyrightText, windowTitle = NULL, root.dir = NULL, knit = TRUE, purl = TRUE) {
   
   if(is.null(outLoc))
      outLoc <- docsLoc
   
   outFigLoc <- file.path(outLoc, "figures", "knitr/")
   if(!file.exists(outFigLoc))
      dir.create(outFigLoc, recursive = TRUE)

   outCacheLoc <- file.path(outLoc, "cache/")
   if(!file.exists(outCacheLoc))
      dir.create(outCacheLoc, recursive = TRUE)
   
   opts_knit$set(base.dir = outLoc)
   opts_chunk$set(comment = NA, tidy = FALSE, dpi = 150, base.dir = outLoc, fig.path = "figures/knitr/", cache.path = outCacheLoc)
   if(!is.null(root.dir))
      opts_knit$set(root.dir = normalizePath(root.dir))
   
   # knit_hooks$set(output = function(x, options) {
   #    if (knitr:::output_asis(x, options)){
   #       return(x)
   #    } 
   #    stringr::str_c('\n\n```no-highlight\n', x, '```\n\n')
   # })
   
   # first knit and purl the files
   
   if(!file.exists(file.path(outLoc, "markdown")))
      dir.create(file.path(outLoc, "markdown"))
   
   if(!file.exists(file.path(outLoc, "code")))
      dir.create(file.path(outLoc, "code"))
   
   ff <- list.files(docsLoc, full.names = TRUE, pattern = "\\.Rmd$", recursive = TRUE)
   ff2 <- list.files(docsLoc, pattern = "\\.Rmd$", recursive = TRUE)
   if(!is.null(pageList)) {
      ff <- ff[ff2 %in% pageList]
   } else {
      pageList <- ff2
   }

   for(.file in ff) {
      ll <- readLines(.file)
      if(grepl("^\\`\\`\\`$", ll[1]))
         ll <- ll[-1]
      
      f2 <- textConnection(paste(ll, collapse = "\n"))
      if(purl)
         purl(f2, output = file.path(outLoc, "code", gsub("\\.Rmd", ".R", basename(.file))), documentation = 0)
      
      f2 <- textConnection(paste(ll, collapse = "\n"))
      if(knit)
         knit(f2, output = file.path(outLoc, "markdown", gsub("\\.Rmd", ".md", basename(.file))))
   }
   
   ff <- list.files(file.path(outLoc, "markdown"))
   
   if(length(ff) == 0)
      stop("There were no markdown files")
   
   # only read files that are in pageList
   ff <- ff[gsub("md$", "Rmd", basename(ff)) %in% basename(pageList)]
   
   aList <- suppressWarnings(lapply(file.path(outLoc, "markdown", ff), readLines))
   a <- do.call(c, aList)
   aFileIdx <- do.call(c, lapply(seq_along(aList), function(x) rep(x, length(aList[[x]]))))
   
   if(grepl("^\\`\\`\\`$", a[1]))
      a <- a[-1]
   
   # fix knitr problem where it doesn't like "```{r}" in <pre><code> blocks...
   knitrProblemInd <- which(grepl("\\\\```", a))
   if(length(knitrProblemInd) > 0) {
      a[knitrProblemInd] <- gsub("(.*)\\\\(```.*)", "\\1\\2", a[knitrProblemInd])
   }
   
   # first extract the title (looks like "# asdfasdf #")
   titleInd <- which(grepl("^# (.*) #$", a))
   
   if(length(titleInd) == 0) {
      pageTitle <- "No Title"      
   } else {
      pageTitle <- gsub("^# (.*) #$", "\\1", a[titleInd])
   }
   
   if(is.null(windowTitle))
      windowTitle <- pageTitle
   
   # remove it from list
   a <- a[-titleInd]
   aFileIdx <- aFileIdx[-titleInd]
   
   # now get "chapter" starting points and names
   # (looks like ## asdfafads ##)
   chInd <- which(grepl("^## (.*) ##$", a))
   chNames <- gsub("^## (.*) ##$", "\\1", a[chInd])
   
   # this stores the .Rmd file the "chapter" comes from
   chFiles <- ff[aFileIdx[chInd]]
   chFiles <- gsub("\\.md$", ".Rmd", chFiles)
   if(is.null(editHref)) {
      chFileEditLink <- "#"
   } else {
      chFileEditLink <- paste(editHref, chFiles, sep = "")
   }
   
   # now replace the text on those lines:
   # (the only purpose they serve is to give us new chapter headers in the toc)
   a[chInd] <- ""
   
   # now within each chapter, extract section names and build TOC
   chStart <- chInd
   chEnd <- c(chStart[-1] - 1, length(a))
   
   toc <- list()
   contents <- list()
   c_ind <- 1
   firstSection <- ""
   for(i in seq_along(chStart)) {
      b <- a[chStart[i]:chEnd[i]]
      
      secInd <- which(grepl("^### (.*) ###$", b))
      secNames <- gsub("^### (.*) ###$", "\\1", b[secInd])
      secID <- buildDocs:::validID(secNames)
      
      tmp <- paste(sprintf("
      <li class='active'>
         <a target='_self' class='nav-not-header' href='#%s'>%s</a>
      </li>\n", secID, secNames), collapse="\n")
      
      toc[[i]] <- sprintf("<li class='nav-header unselectable' data-edit-href='%s'>%s</li>
      %s", chFileEditLink[i], chNames[i], tmp)
      # toc[[i]] <- sprintf("<li class='nav-header'>%s%s</li>
      # %s", chNames[i], chFileEditLink[i], tmp)
      
      secStart <- secInd
      secEnd <- c(secInd[-1] - 1, length(b))
      
      for(j in seq_along(secInd)) {
         if(c_ind == 1)
            firstSection <- secID[j]
         contents[[c_ind]] <- paste(
         "<div class='tab-pane", ifelse(c_ind == 1, " active", ""), "' id='", secID[j], "'>\n", 
         markdownToHTML(text = paste(b[secStart[j]:secEnd[j]], collapse = "\n"), options = "fragment_only"), "\n</div>\n", sep = "")
         
         c_ind <- c_ind + 1
      }
   }
   
   toc <- paste(toc, collapse = "\n\n")
   contents <- paste(contents, collapse = "\n\n")
   
   if(! "package:buildDocs" %in% search()) {
      message("* ---- running dev version - getting templates from source")
      templatePath <- "~/Documents/Code/buildDocs/inst/templates"
      assetsLoc <- "~/Documents/Code/buildDocs/inst/assets"
   } else {
      templatePath <- file.path(system.file(package = "buildDocs"), "templates")
      assetsLoc <- file.path(system.file(package = "buildDocs"), "assets")
   }
   
   # extraHeader <- paste(readLines(file.path(templatePath, "rsyntax.html")), collapse = "\n")
   
   extraHeader <- "<script type=\"text/javascript\" src=\"assets/MathJax/MathJax.js?config=TeX-AMS-MML_HTMLorMML\">
   MathJax.Hub.Config({    
     extensions: [\"tex2jax.js\"],    
     \"HTML-CSS\": { scale: 100}    
   });
   </script>"
   
   ll <- c(list(
      title = windowTitle,
      extra_css = "",
      header = pageTitle,
      side_span = "3",
      main_span = "9",
      toc = toc,
      content = contents,
      copyright = paste(copyrightText, ", ", format(Sys.time(), "%Y"), sep = ""),
      first_section = firstSection,
      extra_header = extraHeader
   ), optTemplates)
   
   pageTemplate <- readLines(file.path(templatePath, "page_template.html"))
   
   b <- whisker.render(pageTemplate, ll)
   
   if(!file.exists(outLoc))
      dir.create(outLoc, recursive = TRUE)
   
   cat(b, file = file.path(outLoc, "index.html"))
   digestMatch <- function(dirName, outLoc, assetsLoc) {
      d1 <- digest(as.character(md5sum(list.files(file.path(assetsLoc, dirName), recursive = TRUE, full.names = TRUE))))
      
      d2 <- digest(as.character(md5sum(list.files(file.path(outAssetsLoc, dirName), recursive = TRUE, full.names = TRUE))))
      d1 == d2
   }
   
   outAssetsLoc <- file.path(outLoc, "assets")
   if(!file.exists(outAssetsLoc))
      dir.create(outAssetsLoc)
   
   for(dirName in c("bootstrap", "custom", "font-awesome", "jquery", "MathJax", "prism", "svgeezy")) {
      if(!digestMatch(dirName, outAssetsLoc, assetsLoc)) {
         message("copying ", dirName)
         file.copy(file.path(assetsLoc, dirName), normalizePath(outAssetsLoc), overwrite = TRUE, recursive = TRUE)
      }
   }
}
