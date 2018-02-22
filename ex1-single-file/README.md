# ex1: A single R Markdown file

To add the reproducibility features to a single file, change the output format
in the YAML header to `repdoc::repdoc_html`. This performs the following:

1. Inserts the date the results were created
1. Reports the revision of the Git repo (if in Git repo)
1. Reports the Git status of the R Markdown file (if in Git repo)
1. Sets a seed for random number generation
1. Inserts `sessionInfo()` at the end of the analysis
