#!/usr/bin/env Rscript

library(knitr)
library(markdown)

knit("./baltimore.Rmd")
markdownToHTML("./baltimore.md", "./baltimore.html")
