# coler

This package contains personal R functions and RMarkdown templates.
It also imports select functions from packages that are inconvenient to load entirely.

## Install

```r
devtools::install_github("coletl/coler")

# Don't check dependencies
devtools::install_github("coletl/coler", depend = FALSE)
```



To remove the RMarkdown templates from your computer, you can run

```r
unlink(file.path(find.package("easyr"), "rmarkdown"), recursive = TRUE)
```
