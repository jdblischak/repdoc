## ex2: R Markdown website

To add the reproducibility features to all files in an R Markdown website,
specify the custom site generator `repdoc::repdoc_site` in the YAML header of
`index.Rmd` and specify `repdoc::repdoc_html` as the output format in the
configuration file `_site.yml`. Then the site can be built with
`rmarkdown::render_site` or the RStudio Knit button.
