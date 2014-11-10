#' Build Function Reference
#'
#' Build function reference using staticdocs
#' 
#' @param packageLoc base directory of the R package
#' @param outLoc base directory where output .html, assets, code, figures should go
#' @param optTemplates optional template content - list of named elements - currently \code{navpills} and \code{brand} (see \code{\link{packagePills}})
#' @param copyrightText text to go with copyright
#' @param windowTitle optional title for the window
#' @export
buildFunctionRef <- function(packageLoc, outLoc = NULL, optTemplates = NULL, copyrightText, windowTitle = NULL) {
   options(gsubfn.engine = "R")
   require(gsubfn)
   
   package_path <- packageLoc
   # temporarily create staticdocs directory
   sdDir <- file.path(packageLoc, "inst/web")
   if(!file.exists(sdDir))
      dir.create(sdDir)
   
   devtools:::load_all(package_path)
   # package <- staticdocs:::package_info(packageLoc, tempdir(), examples)
   package <- as.sd_package(packageLoc, examples = FALSE)
   
   # build_site(packageLoc, examples = FALSE, templates_path = "~/Documents/Code/buildDocs/inst/templates")
   
   pageTitle <- paste(package$package, "R function reference")
   
   if(is.null(windowTitle))
      windowTitle <- pageTitle
   
   if(!file.exists(package$sd_path)) 
      dir.create(package$sd_path)
   
   # build_topics
   index <- package$rd_index
   paths <- file.path(package$sd_path, index$file_out)
   index$title <- ""
   index$in_index <- TRUE
   sectionNames <- list()
   contents <- list()
   
   if(! "package:buildDocs" %in% search()) {
      message("* ---- running dev version - getting templates from source")
      templatePath <- "~/Documents/Code/buildDocs/inst/templates"
   } else {
      templatePath <- file.path(system.file(package = "buildDocs"), "templates")
   }
   
   tocTemplate <- readLines(file.path(templatePath, "ftoc_template.html"))
   
   contentTemplate <- readLines(file.path(templatePath, "content_template.html"))
   
   for (i in seq_along(index$name)) {
      message(i, " Generating ", basename(paths[[i]]))
      rd <- package$rd[[i]]
      rdc <- sapply(rd, function(x) class(x)[1])
      exInd <- which(rdc == "examples")
      html <- staticdocs:::to_html(rd, 
         env = new.env(parent = globalenv()), 
         topic = stringr:::str_replace(basename(paths[[i]]), "\\.html$", ""), 
         pkg = package)
      
      # need to extract examples separately (don't want to evaluate them and want them to not look like crap)
      # text <- to_html.TEXT(x[-1], ...)
      
      html$pagetitle <- html$name
      html$package <- package[c("package", "version")]
      # render_page
      name <- "topic"
      path <- paths[[i]]
      data <- html
      
      # description is in sections for some reason
      descInd <- which(sapply(data$sections, function(a) {
         if(!is.null(names(a))) {
            if("title" %in% names(a)) {
               if(a$title == "Description")
                  return(TRUE)
            }
         }
         FALSE
      }))
      if(length(descInd) > 0) {
         data$description <- data$sections[[descInd]]$contents
         data$sections[[descInd]] <- NULL
      }
      
      zeroInd <- which(sapply(data$sections, length) == 0)
      if(length(zeroInd) > 0)
         data$sections <- data$sections[-zeroInd]
      
      rgxp <- "([a-zA-Z0-9\\.\\_]+)\\.html"
      
      # replace seealso links with hashes
      data$seealso <- gsubfn(rgxp, ~ paste("#", validID(x), sep = ""), data$seealso)
      
      # same for usage
      data$usage <- gsubfn(rgxp, ~ paste("#", validID(x), sep = ""), data$usage)
      data$usage <- gsub("\\n      ", "\n  ", data$usage)
      
      for(jj in seq_along(data$sections)) {
         if("contents" %in% names(data$sections[[jj]]))
            data$sections[[jj]]$contents <- gsubfn(rgxp, ~ paste("#", validID(x), sep = ""), data$sections[[jj]]$contents)
      }
      # "#\\L\\1"
      
      for(jj in seq_along(data$arguments)) {
         data$arguments[[jj]]$description <- gsubfn(rgxp, ~ paste("#", validID(x), sep = ""), data$arguments[[jj]]$description)
      }
      
      data2 <- list(
         active = "", # ifelse(i == 1, "active", ""),
         href = validID(data$name),
         item = data$name
      )
      # data2$href <- 
      
      sectionNames[[i]] <- whisker.render(tocTemplate, data2)
      
      data$active <- "" # ifelse(i == 1, " active", "")
      data$lname <- validID(data$name)
      contents[[i]] <- whisker.render(contentTemplate, data)
   }
   
   sectionNames <- c(whisker.render(tocTemplate, list(active = "active", href = "packagemain", item = "Package Info")), sectionNames)
   
   mainTemplate <- readLines(file.path(templatePath, "fmain_template.html"))
   
   contents <- c(whisker.render(mainTemplate, list(
      lname = "packagemain",
      title = package$title,
      version = package$version,
      date = package$date,
      description = package$description,
      license = package$license,
      depends = package$depends,
      suggests = package$suggests,
      author = package$author
   )), contents)
   
   toc <- paste(
      "<li class='nav-header'>Contents</li>",
      paste(sectionNames, collapse = "\n")
   )
   
   ll <- c(list(
      title = windowTitle,
      extra_css = "",
      header = pageTitle,
      side_span = "3",
      main_span = "9",
      toc = toc,
      content = paste(contents, collapse = "\n"),
      copyright = paste(copyrightText, ", ", format(Sys.time(), "%Y"), sep = ""),
      first_section = "packagemain",
      extra_header = ""
   ), optTemplates)
   
   pageTemplate <- readLines(file.path(templatePath, "page_template.html"))
   
   a <- whisker.render(pageTemplate, ll)
   
   cat(a, file = file.path(outLoc, "functionref.html"))
   unlink(sdDir, recursive = TRUE)
}

