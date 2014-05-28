buildDocs
=========

`buildDocs` is an R package that is used to build R package and function documentation which is meant to be hosted in the `gh-pages` branch of a github repository of an R package.

I built the package with a particular use case in mind and did not plan on releasing it as a package or thinking much about making it more flexible.  If it does not fit your use case and you can think of some easy ways to make it better, please let me know.  Also feel free to fork, send pull requests, etc.

Credit
------

This package is dependent on many other awesome projects, which deserve credit here:

- [knitr](http://yihui.name/knitr/)
- [devtools](https://github.com/hadley/devtools)
- [whisker](https://github.com/edwindj/whisker)
- [rmarkdown](https://github.com/rstudio/rmarkdown)
- [staticdocs](https://github.com/hadley/staticdocs)
- [MathJax](http://www.mathjax.org)
- [Bootstrap](http://getbootstrap.com)
- [svgeezy](http://benhowdle.im/svgeezy/)


Installation
------------

```s
library(devtools)

install_github("staticdocs", "hadley")

install_github("buildDocs", "hafen")
```

Preparing a github Repo
-----------------------

If you haven't yet set up a `gh-pages` branch for your repository, take a visit [here](http://pages.github.com) and come back when you have one.

#### After you have a gh-pages branch:

I have found it most useful to have a separate checkout of just your `gh-pages` branch on your local machine.  It is a pain to have to check this branch out as it is completely different from your code branches.

For example, here is how to clone just the `gh-pages` branch from a project called `datadr` (assuming git version 1.8 or later):

```
# clone the repo
git clone -b gh-pages --single-branch https://github.com/hafen/datadr.git
# rename it so it doesn't clash with code repo
mv datadr datadr-gh-pages
```

Usage
-----

I typically create a directory `docs` and start putting my `.Rmd` files in there.  You can split your documentation into as many `.Rmd` files as you would like.

`buildDocs()` takes all `.Rmd` files in the specified `docsLoc` directory and puts them together.  It can be helpful to number the files so that it gets them in the correct order.  Alternatively, you can specify a vector of file names as the `pageList` argument to `buildDocs()` to specify the order in which to put them together.

`buildDocs()` looks for some specific behaviors in your `.Rmd` files to determine what to do.  These include:
- "h1" header is the name and title of the page, and line must strictly start and end with "#" - there should only be one of these in the document.
- "h2" headers are the chapter names (denote separation in TOC), and these lines must strictly start and end with "##"
-  "h3" headers are the section names, and these lines must strictly start and end with "###"

The strict start and end requirements are to do with not mistaking R comments with section headings.

You can also build a function reference thanks to staticdocs using the function `buildFunctionRef()`.

Here is example usage for a project `project` with documents located in `/tmp/project-gh-pages/docs`:

```s
buildDocs(
   docsLoc = "/tmp/project-gh-pages/docs",
   outLoc = "/tmp/project-gh-pages/",
   pageList = c("1intro.Rmd", "2data.Rmd"),
   navPill = packageNavPill("https://github.com/user/project"),
   copyrightText = "Somebody",
   editHref = "https://github.com/user/project/edit/gh-pages/docs/",
   knit = TRUE
)

buildFunctionRef(
   packageLoc = "/tmp/package",
   outLoc = "/tmp/package-gh-pages/",
   navPill = packageNavPill("https://github.com/user/project", docs = FALSE),
   copyrightText = "Somebody"
)
```


