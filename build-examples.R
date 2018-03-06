#!/usr/bin/env Rscript

stopifnot(basename(getwd()) == "repdoc")

rmarkdown::render("ex1-single-file/single.Rmd")
rmarkdown::render_site("ex2-website/")
rmarkdown::render_site("ex3-website-subdir/analysis/")
