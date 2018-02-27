## ex3: R Markdown website with subdirectories

This website adds a configuration file, `_repdoc.yml`, to control the
reproducibility settings across the website files. Specifically it sets the
`knit_root_dir`. The R Markdown files are saved in `analysis/`, but the code is
executed in the root of the website directory. It also uses
`devtools::session_info()` instead of `sessionInfo()` by specifying this in
`_repdoc.yml`.
